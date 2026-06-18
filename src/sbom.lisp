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

;;; --- Producer ------------------------------------------------------------

(defparameter *syft-bin* "syft"
  "Default syft executable, overridable via the :syft-path config key.")

(defun run-syft-sbom (disk-path)
  "Run syft against DISK-PATH, returning a CycloneDX JSON string, or NIL on
   failure (logged). Requires syft on PATH."
  (handler-case
      (multiple-value-bind (out err exit)
          (uiop:run-program (list (config-value :syft-path *syft-bin*)
                                  (namestring disk-path)
                                  "-o" "cyclonedx-json")
                            :output '(:string)
                            :error-output '(:string)
                            :ignore-error-status t)
        (if (zerop exit)
            out
            (progn (llog:warn "syft failed" :path (namestring disk-path) :error err)
                   nil)))
    (error (e)
      (llog:warn "syft error" :error (princ-to-string e))
      nil)))

(defun scan-repo-deps (owner repo-name &key ref sbom-json)
  "Produce and ingest the dependency graph for OWNER/REPO-NAME. Uses SBOM-JSON if
   given, else runs syft against the repo's working tree. Returns the dep count,
   or NIL if no SBOM could be produced or the repo is unknown."
  (let* ((the-ref (or ref (chamber-get-default-branch owner repo-name) "main"))
         (json (or sbom-json (run-syft-sbom (repo-disk-path owner repo-name)))))
    (when json
      (let ((repo (find-repo owner repo-name)))
        (when repo
          (ingest-repo-deps (getf repo :id) the-ref (sbom->deps json)))))))

(defun maybe-scan-repo-deps-async (owner repo-name ref)
  "Background dependency scan after a push to the default branch. Guarded by the
   :deps-scan-enabled config flag; mirrors zoekt-index-repo."
  (when (config-value :deps-scan-enabled)
    (bt2:make-thread
     (lambda ()
       (handler-case
           (postmodern:with-connection *db-spec*
             (let ((n (scan-repo-deps owner repo-name :ref ref)))
               (when n
                 (llog:info "Scanned repo deps"
                            :repo (format nil "~A/~A" owner repo-name) :count n))))
         (error (e)
           (llog:warn "Dep scan error"
                      :repo (format nil "~A/~A" owner repo-name)
                      :error (princ-to-string e)))))
     :name (format nil "deps-scan-~A/~A" owner repo-name))))
