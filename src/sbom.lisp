;;; sbom.lisp — CycloneDX SBOM parsing and the dependency-graph producer.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Turns a CycloneDX SBOM (produced by syft) into dependency rows and feeds
;;; ingest-repo-deps. Reached three ways: the /-/internal/repos/.../deps HTTP
;;; endpoint (server.lisp), the `cave-server deps-scan` CLI (main.lisp), and an
;;; async scan on default-branch push (server.lisp post-receive).
;;; See docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *purl-type->ecosystem*
  '(("npm" . "npm") ("golang" . "Go") ("pypi" . "PyPI") ("cargo" . "crates.io")
    ("gem" . "RubyGems") ("maven" . "Maven") ("nuget" . "NuGet")
    ("composer" . "Packagist") ("hex" . "Hex") ("pub" . "Pub")
    ("apk" . "Alpine") ("deb" . "Debian") ("conan" . "ConanCenter")
    ("swift" . "SwiftURL"))
  "purl package type -> OSV ecosystem name. Unknown types pass through verbatim.")

(defun %percent-decode (s)
  "Decode %XX escapes in a purl path segment."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n)
            for c = (char s i)
            do (if (and (char= c #\%) (< (+ i 2) n))
                   (let ((code (parse-integer s :start (1+ i) :end (+ i 3)
                                                :radix 16 :junk-allowed t)))
                     (if code
                         (progn (write-char (code-char code) out) (incf i 3))
                         (progn (write-char c out) (incf i))))
                   (progn (write-char c out) (incf i)))))))

(defun parse-purl (purl)
  "Parse PURL into (values ecosystem package-name version). Returns NIL values
   when PURL is missing or unparseable. Handles npm scopes, Go module paths, and
   Maven group:artifact naming."
  (when (and (stringp purl) (uiop:string-prefix-p "pkg:" purl))
    (let* ((s (subseq purl 4))
           (s (subseq s 0 (min (or (position #\? s) (length s))
                               (or (position #\# s) (length s)))))
           (at (position #\@ s :from-end t))
           (version (when at (%percent-decode (subseq s (1+ at)))))
           (path (if at (subseq s 0 at) s))
           (segs (remove "" (uiop:split-string path :separator '(#\/))
                         :test #'string=)))
      (when segs
        (let* ((type (string-downcase (first segs)))
               (rest (mapcar #'%percent-decode (rest segs)))
               (eco (or (cdr (assoc type *purl-type->ecosystem* :test #'string=))
                        type))
               (name (cond ((null rest) nil)
                           ((string= type "maven")
                            (format nil "~{~A~^.~}:~A"
                                    (butlast rest) (car (last rest))))
                           (t (format nil "~{~A~^/~}" rest)))))
          (values eco name version))))))

(defun %component-manifest-path (component)
  "Best-effort manifest/location path from a syft CycloneDX component's
   properties (syft:location:N:path), else NIL."
  (let ((props (%osv-get component "properties")))
    (when (vectorp props)
      (loop for p across props
            for name = (%osv-get p "name")
            when (and (stringp name) (search "location" name) (search "path" name))
            return (%osv-get p "value")))))

(defun sbom->deps (sbom-json)
  "Parse a CycloneDX SBOM (JSON string) into a list of dep plists for
   ingest-repo-deps. Components without a usable purl are skipped."
  (let* ((data (com.inuoe.jzon:parse sbom-json))
         (components (%osv-get data "components"))
         (deps '()))
    (when (vectorp components)
      (loop for c across components
            for purl = (let ((p (%osv-get c "purl"))) (when (stringp p) p))
            when purl
            do (multiple-value-bind (eco name version) (parse-purl purl)
                 (when (and eco name version)
                   (push (list :manifest-path (or (%component-manifest-path c) "sbom")
                               :ecosystem eco :package-name name :version version
                               :purl purl :is-direct t)
                         deps)))))
    (nreverse deps)))

;;; --- Producer: runner-based extraction (Option B) -------------------------
;;;
;;; Extraction runs on a runner, not the server: enqueue-deps-scan schedules a
;;; workflow job (syft image) that clones the repo, runs syft, and emits the
;;; CycloneDX SBOM on stdout — captured into the step log. When the run
;;; finalizes, maybe-ingest-scan-run reads that log and ingests it. No SBOM
;;; tooling on the server, no token piped into the runner container.

(defparameter *deps-scan-workflow-name* "deps-scan"
  "workflow_name marker identifying a run as a dependency scan.")

(defun ingest-sbom-json (owner repo-name json &key ref)
  "Ingest a CycloneDX SBOM (JSON string) for OWNER/REPO-NAME and refresh the
   dashboard. Returns the dep count, or NIL if the repo is unknown."
  (let ((repo (find-repo owner repo-name)))
    (when repo
      (let ((n (ingest-repo-deps
                (getf repo :id)
                (or ref (chamber-get-default-branch owner repo-name) "main")
                (sbom->deps json))))
        (handler-case (update-dependency-dashboard (getf repo :id))
          (error (e)
            (llog:warn "Dashboard update failed"
                       :repo (format nil "~A/~A" owner repo-name)
                       :error (princ-to-string e))))
        n))))

(defun enqueue-deps-scan (repo-id &key ref commit-sha triggered-by-id)
  "Schedule a runner workflow job that produces a CycloneDX SBOM with syft and
   returns it through the step log. The completion hook ingests it. Guarded by
   :deps-scan-enabled. Returns the workflow run plist, or NIL."
  (when (config-value :deps-scan-enabled)
    (let* ((repo (find-repo-by-id repo-id))
           (owner (and repo (repo-owner-name repo)))
           (name (and repo (getf repo :name))))
      (when repo
        (let* ((the-ref (or ref (chamber-get-default-branch owner name) "main"))
               (labels (config-value :deps-scan-labels ""))
               (runs-on (when (plusp (length labels))
                          (mapcar (lambda (s) (string-trim " " s))
                                  (uiop:split-string labels :separator '(#\,)))))
               (run (create-workflow-run
                     :repo-id repo-id
                     :workflow-name *deps-scan-workflow-name*
                     :workflow-file ""
                     :trigger-event "deps_scan"
                     :commit-sha commit-sha
                     :ref the-ref
                     :triggered-by-id triggered-by-id))
               (job (create-workflow-job
                     :workflow-run-id (getf run :id)
                     :name "scan"
                     :image (config-value :deps-scan-image
                                          "ghcr.io/atgreen/cave-scan:main")
                     :runs-on runs-on)))
          ;; syft -> file, then cat: the runner captures stdout+stderr combined,
          ;; so suppress syft's progress and emit only clean CycloneDX JSON.
          (create-workflow-step
           :job-id (getf job :id) :step-order 1 :name "syft"
           :command (concatenate 'string
                                 "syft -q dir:/workspace -o cyclonedx-json=/tmp/sbom.json "
                                 ">/dev/null 2>&1 && cat /tmp/sbom.json"))
          run)))))

(defun %extract-json-object (s)
  "Substring of S from the first { to the last } — a safety net against stray
   output in the step log. NIL if no object is found."
  (when (stringp s)
    (let ((start (position #\{ s))
          (end (position #\} s :from-end t)))
      (when (and start end (< start end))
        (subseq s start (1+ end))))))

(defun maybe-ingest-scan-run (run-id)
  "Called when a workflow run finalizes. If it's a successful deps-scan run, read
   the scan step's log (the CycloneDX SBOM) and ingest it."
  (let ((run (find-workflow-run run-id)))
    (when (and run
               (equal (getf run :workflow-name) *deps-scan-workflow-name*)
               (equal (getf run :status) "success"))
      (let* ((repo (find-repo-by-id (getf run :repo-id)))
             (owner (and repo (repo-owner-name repo)))
             (name (and repo (getf repo :name)))
             (log (with-output-to-string (out)
                    (dolist (job (list-workflow-jobs run-id))
                      (dolist (step (list-workflow-steps (getf job :id)))
                        (let ((l (getf step :log)))
                          (when (and l (not (eq l :null))) (write-string l out)))))))
             (json (%extract-json-object log)))
        (when (and repo json)
          (handler-case
              (let ((n (ingest-sbom-json owner name json
                                         :ref (let ((r (getf run :ref)))
                                                (unless (eq r :null) r)))))
                (llog:info "Ingested runner SBOM"
                           :repo (format nil "~A/~A" owner name) :count n))
            (error (e)
              (llog:warn "Scan ingest failed"
                         :repo (format nil "~A/~A" owner name)
                         :error (princ-to-string e)))))))))
