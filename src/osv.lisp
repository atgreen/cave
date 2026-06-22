;;; osv.lisp — OSV.dev advisory client and sync.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Rather than mirror OSV's per-ecosystem zip exports (no zip library is
;;; available, and a forge only cares about packages it actually hosts), this
;;; queries the OSV REST API for the distinct packages in the dependency graph,
;;; upserts the matching advisories, and re-matches affected repos. See
;;; docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *osv-api-base* "https://api.osv.dev"
  "Base URL for the OSV.dev REST API.")

(defparameter +osv-json-null+ (com.inuoe.jzon:parse "null")
  "jzon's sentinel for JSON null, used to normalize optional fields to NIL.")

(declaim (inline %osv-denull))
(defun %osv-denull (v)
  "Normalize jzon's JSON-null sentinel to CL NIL."
  (if (eq v +osv-json-null+) nil v))

(defun %osv-get (h key)
  "gethash KEY in hash-table H, normalizing JSON null to NIL. NIL when H isn't
   a hash-table."
  (when (hash-table-p h)
    (%osv-denull (gethash key h))))

;;; --- HTTP -----------------------------------------------------------------

(defparameter *osv-ecosystems*
  '("npm" "Go" "PyPI" "crates.io" "RubyGems" "Maven" "NuGet" "Packagist"
    "Hex" "Pub" "Alpine" "Debian" "ConanCenter" "SwiftURL" "GitHub Actions")
  "OSV ecosystem names we query. Packages in any other ecosystem are dropped
   before the batch — OSV rejects the entire querybatch (HTTP 400) if even one
   query carries an unknown ecosystem, so one unsupported package must not take
   down the whole sync.")

(defun osv-querybatch (packages)
  "POST a batch of {ecosystem,name} queries to OSV. PACKAGES is a list of
   (ecosystem . name) conses (<=1000). Returns a list of vulnerability IDs
   affecting them, or NIL on transport error (logged, never signalled).
   Packages whose ecosystem OSV doesn't recognize are skipped."
  (let* ((skipped (remove-if (lambda (p) (member (car p) *osv-ecosystems* :test #'string=))
                             packages))
         (packages (remove-if-not (lambda (p) (member (car p) *osv-ecosystems* :test #'string=))
                                  packages)))
    (when skipped
      (llog:warn "OSV: skipping packages in unsupported ecosystem(s)"
                 :count (length skipped)
                 :ecosystems (remove-duplicates (mapcar #'car skipped) :test #'string=)))
  (when packages
    (handler-case
        (let* ((queries (map 'vector
                             (lambda (p)
                               (let ((q (make-hash-table :test 'equal))
                                     (pkg (make-hash-table :test 'equal)))
                                 (setf (gethash "ecosystem" pkg) (car p)
                                       (gethash "name" pkg) (cdr p)
                                       (gethash "package" q) pkg)
                                 q))
                             packages))
               (root (make-hash-table :test 'equal))
               (_ (setf (gethash "queries" root) queries))
               (body (com.inuoe.jzon:stringify root)))
          (declare (ignore _))
          (multiple-value-bind (resp status)
              (dex:post (format nil "~A/v1/querybatch" *osv-api-base*)
                        :content body
                        :headers '(("content-type" . "application/json"))
                        :connect-timeout 10 :read-timeout 30)
            (if (= status 200)
                (let ((ids '())
                      (results (%osv-get (com.inuoe.jzon:parse resp) "results")))
                  (when results
                    (loop for r across results
                          for vulns = (%osv-get r "vulns")
                          when vulns
                          do (loop for v across vulns
                                   for id = (%osv-get v "id")
                                   when id do (push id ids))))
                  (nreverse ids))
                (progn (llog:warn "OSV querybatch non-200" :status status) nil))))
      (error (e)
        (llog:warn "OSV querybatch failed" :error (princ-to-string e))
        nil)))))

(defun osv-fetch-vuln (id)
  "GET /v1/vulns/ID. Returns the parsed OSV record (hash-table) or NIL."
  (handler-case
      (multiple-value-bind (resp status)
          (dex:get (format nil "~A/v1/vulns/~A" *osv-api-base* id)
                   :connect-timeout 10 :read-timeout 30)
        (if (= status 200)
            (com.inuoe.jzon:parse resp)
            (progn (llog:warn "OSV vuln fetch non-200" :id id :status status) nil)))
    (error (e)
      (llog:warn "OSV vuln fetch failed" :id id :error (princ-to-string e))
      nil)))

;;; --- Record parsing -------------------------------------------------------

(defun %osv-severity-label (record)
  "Best-effort qualitative severity (low/moderate/high/critical) from
   database_specific.severity (GHSA style), else NIL."
  (let ((sev (%osv-get (%osv-get record "database_specific") "severity")))
    (when (stringp sev)
      (let ((s (string-downcase sev)))
        (cond ((member s '("low" "high" "critical") :test #'string=) s)
              ((member s '("moderate" "medium") :test #'string=) "moderate")
              (t nil))))))

(defun %osv-affected-rows (record)
  "Flatten RECORD's affected[].ranges[] (and a bare versions[] fallback) into a
   list of plists: :ecosystem :package-name :range-type :introduced :fixed
   :last-affected :repo. SEMVER/ECOSYSTEM ranges need package.{ecosystem,name}.
   GIT ranges (a source repo + introduced/fixed commits, possibly with a null
   package — the cl-sec Common Lisp feed) are kept too: :repo holds the source
   repo and ecosystem/package_name are set to GIT/repo so the NOT NULL columns
   stay valid and the repo stays indexable."
  (let ((rows '())
        (affected (%osv-get record "affected")))
    (when affected
      (loop for a across affected
            for pkg = (%osv-get a "package")
            for eco = (%osv-get pkg "ecosystem")
            for name = (%osv-get pkg "name")
            for ranges = (%osv-get a "ranges")
            do (let ((emitted nil))
                 (labels ((push-row (rtype eco* name* introduced fixed last repo)
                            (push (list :ecosystem eco* :package-name name*
                                        :range-type rtype
                                        :introduced (or introduced "0")
                                        :fixed fixed :last-affected last :repo repo)
                                  rows)
                            (setf emitted t))
                          (emit-version (rtype introduced fixed last)
                            (push-row rtype eco name introduced fixed last nil))
                          (emit-git (repo introduced fixed)
                            (push-row "GIT" "GIT" repo introduced fixed nil repo)))
                   (when ranges
                     (loop for rng across ranges
                           for rtype = (or (%osv-get rng "type") "SEMVER")
                           for events = (%osv-get rng "events")
                           when events
                           do (if (string-equal rtype "GIT")
                                  (let ((repo (%osv-get rng "repo")) (introduced nil))
                                    (when repo
                                      (loop for ev across events
                                            for intro = (%osv-get ev "introduced")
                                            for fixed = (%osv-get ev "fixed")
                                            do (cond (intro (setf introduced intro))
                                                     (fixed (emit-git repo introduced fixed)
                                                            (setf introduced nil))))
                                      (when introduced (emit-git repo introduced nil))))
                                  (when (and eco name)
                                    (let ((introduced nil))
                                      (loop for ev across events
                                            for intro = (%osv-get ev "introduced")
                                            for fixed = (%osv-get ev "fixed")
                                            for last = (%osv-get ev "last_affected")
                                            do (cond (intro (setf introduced intro))
                                                     (fixed (emit-version rtype introduced fixed nil)
                                                            (setf introduced nil))
                                                     (last (emit-version rtype introduced nil last)
                                                           (setf introduced nil))))
                                      (when introduced (emit-version rtype introduced nil nil)))))))
                   ;; fallback: explicit versions[] when no usable ranges
                   (when (and (not emitted) eco name)
                     (let ((versions (%osv-get a "versions")))
                       (when versions
                         (loop for ver across versions
                               when (stringp ver)
                               do (emit-version "ECOSYSTEM" ver nil ver)))))))))
    (nreverse rows)))

(defun osv-ingest-record (record)
  "Upsert one OSV RECORD (hash-table) and its affected ranges. Returns the
   advisory id, or NIL if the record has no id."
  (let ((id (%osv-get record "id")))
    (when id
      (let* ((aliases (let ((a (%osv-get record "aliases")))
                        (when a (coerce a 'list))))
             (refs (%osv-get record "references"))
             (adv-id (upsert-advisory
                      :osv-id id
                      :summary (%osv-get record "summary")
                      :details (%osv-get record "details")
                      :aliases aliases
                      :severity (%osv-severity-label record)
                      :refs (when refs (com.inuoe.jzon:stringify refs))
                      :published-at (%osv-get record "published")
                      :modified-at (%osv-get record "modified")
                      :withdrawn-at (%osv-get record "withdrawn"))))
        (replace-advisory-affected adv-id (%osv-affected-rows record))
        adv-id))))

;;; --- Sync orchestration ---------------------------------------------------

(defun distinct-graph-packages (&key ecosystems)
  "Distinct (ecosystem . name) conses across the dependency graph, optionally
   restricted to ECOSYSTEMS (a list of ecosystem strings)."
  (let ((rows (if ecosystems
                  (postmodern:query
                   "SELECT DISTINCT ecosystem, package_name FROM cave_repo_deps
                    WHERE ecosystem = ANY($1)"
                   (coerce ecosystems 'vector) :plists)
                  (postmodern:query
                   "SELECT DISTINCT ecosystem, package_name FROM cave_repo_deps"
                   :plists))))
    (mapcar (lambda (r) (cons (getf r :ecosystem) (getf r :package-name))) rows)))

(defun %chunk (list n)
  "Split LIST into sublists of at most N elements."
  (loop for tail on list by (lambda (l) (nthcdr n l))
        collect (subseq tail 0 (min n (length tail)))))

(defun sync-osv-advisories (&key ecosystems (verbose t))
  "Query OSV for every package in the dependency graph, upsert matching
   advisories, and re-match affected repos. Returns (values advisories-synced
   repo-ref-pairs-rematched).

   Not incremental: every matching advisory is re-fetched and upserted (the
   upsert is idempotent). Incremental-by-modified is a later optimization."
  (let ((packages (distinct-graph-packages :ecosystems ecosystems)))
    (when verbose
      (format t "~&Querying OSV for ~A package(s)...~%" (length packages)))
    (let ((ids (make-hash-table :test 'equal)))
      (dolist (chunk (%chunk packages 1000))
        (dolist (id (osv-querybatch chunk))
          (setf (gethash id ids) t)))
      (when verbose
        (format t "~&~A advisory record(s) to fetch.~%" (hash-table-count ids)))
      (let ((adv-ids '()) (synced 0))
        (loop for id being the hash-key of ids
              for record = (osv-fetch-vuln id)
              when record
              do (let ((adv-id (osv-ingest-record record)))
                   (when adv-id
                     (push adv-id adv-ids)
                     (incf synced)
                     (when verbose (format t "  ~A~%" id)))))
        (let ((pairs 0))
          (dolist (adv-id (remove-duplicates adv-ids))
            (incf pairs (rematch-advisory adv-id)))
          (refresh-dependency-dashboards)
          (when verbose
            (format t "~&Synced ~A advisories; re-matched ~A repo/ref pair(s).~%"
                    synced pairs))
          (values synced pairs))))))

;;; --- OSV feed ingest (cl-sec and similar URL-published feeds) --------------
;;;
;;; OSV's API only covers its own ecosystems, so Common Lisp advisories (which
;;; OSV doesn't carry) are pulled from a published feed: a JSON array of OSV
;;; records. Ingest is otherwise identical — osv-ingest-record handles GIT
;;; ranges now. Matching ocicl deps against these is Stage 2.

(defun sync-feed-advisories (url &key (verbose t))
  "Fetch an OSV feed (JSON array of OSV records, or a single record) from URL and
   upsert each. Returns the number of records ingested."
  (let ((json (handler-case (dex:get url :read-timeout 30)
                (error (e)
                  (llog:warn "Advisory feed fetch failed" :url url
                                                          :error (princ-to-string e))
                  nil))))
    (if (null json)
        0
        (let* ((data (com.inuoe.jzon:parse json))
               (records (cond ((vectorp data) data)
                              ((hash-table-p data) (vector data))
                              (t #())))
               (n 0))
          (when verbose
            (format t "~&Ingesting ~A record(s) from ~A...~%" (length records) url))
          (loop for record across records
                when (and (hash-table-p record) (osv-ingest-record record))
                do (incf n) (when verbose (format t "  ~A~%" (%osv-get record "id"))))
          n))))

;;; --- ocicl project -> upstream repo resolution ----------------------------
;;;
;;; ocicl deps carry the SYSTEM name; the cl-sec advisories identify the upstream
;;; SOURCE REPO. The bridge is ocicl/<project>/README.org, an org-mode table with
;;; `| source | git:<url> |`, `| commit | <sha> |`, and `| systems | a b c |`.

(defparameter *ocicl-readme-url-template*
  "https://raw.githubusercontent.com/ocicl/~A/master/README.org"
  "Where an ocicl project's metadata lives. ~A is the project name (the lockfile
   directory prefix, == _00_OCICL_NAME).")

(defun %parse-ocicl-readme (org-text)
  "Pull (values source-repo commit systems-list) from an ocicl project
   README.org. Rows look like `| source | git:https://...git |`."
  (let (repo commit systems)
    (dolist (line (uiop:split-string (or org-text "") :separator '(#\Newline)))
      (let ((cells (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                           (remove "" (uiop:split-string line :separator '(#\|))
                                   :test #'string=))))
        (when (= (length cells) 2)
          (let ((key (string-downcase (first cells)))
                (val (second cells)))
            (cond ((string= key "source")
                   ;; strip the `git:` scheme prefix ocicl prepends
                   (setf repo (if (uiop:string-prefix-p "git:" val) (subseq val 4) val)))
                  ((string= key "commit") (setf commit val))
                  ((string= key "systems")
                   (setf systems (remove "" (uiop:split-string val :separator '(#\Space))
                                         :test #'string=))))))))
    (values repo commit systems)))

(defun resolve-ocicl-project (name &key (verbose nil))
  "Fetch ocicl/NAME's README.org, cache its source repo/commit/systems, and
   return the cached row plist. NIL if the README can't be fetched."
  (let ((text (handler-case
                  (dex:get (format nil *ocicl-readme-url-template* name) :read-timeout 20)
                (error () nil))))
    (when text
      (multiple-value-bind (repo commit systems) (%parse-ocicl-readme text)
        (upsert-ocicl-project name :source-repo repo :source-commit commit :systems systems)
        (when verbose (format t "  ~A -> ~A~%" name (or repo "?")))
        (find-ocicl-project name)))))

(defun sync-ocicl-projects (&key (verbose t))
  "Resolve every ocicl project in the dependency graph to its upstream repo,
   caching the results. Returns the number resolved."
  (let ((projects (list-graph-ocicl-projects)) (n 0))
    (when verbose
      (format t "~&Resolving ~A ocicl project(s) to upstream repos...~%"
              (length projects)))
    (dolist (p projects)
      (when (and p (resolve-ocicl-project p :verbose verbose)) (incf n)))
    n))
