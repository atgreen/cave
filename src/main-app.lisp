(in-package #:cave)

(defun make-post-receive-command ()
  (clingon:make-command
   :name "post-receive"
   :description "Handle post-receive events (schedule automations)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/name"))
   :handler #'handle-post-receive))

(defun handle-post-receive (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (repo-path (clingon:getopt cmd :repo)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
           (owner (first parts))
           (name (second parts))
           (repo (find-repo owner name)))
      (when repo
        ;; Read stdin for updated refs (format: oldsha newsha refname)
        (let ((refs nil))
          (handler-case
              (loop for line = (read-line *standard-input* nil nil)
                    while line
                    do (let ((parts (uiop:split-string line :separator '(#\Space))))
                         (when (>= (length parts) 3)
                           (push (list :old (first parts)
                                       :new (second parts)
                                       :ref (third parts))
                                 refs))))
            (error () nil))
          (when refs
            (touch-repo-pushed-at (getf repo :id)))
          ;; Schedule post_receive automations for each updated ref
          (dolist (r refs)
            (schedule-automations (getf repo :id) "post_receive"
                                  :commit-sha (getf r :new)
                                  :ref (getf r :ref))))))
    (disconnect-db)))

;;; --- SYNC-THEMES subcommand ---

(defun make-sync-themes-command ()
  (clingon:make-command
   :name "sync-themes"
   :description "Sync user themes from a cave-themes repo"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/cave-themes"))
   :handler #'handle-sync-themes))

(defparameter *theme-color-keys*
  '("bg" "bg-warm" "surface" "surface-hover" "border" "border-subtle"
    "text" "text-secondary" "text-muted"
    "accent" "accent-dim" "accent-bg"
    "link" "link-hover"
    "green" "green-bright" "green-bg"
    "red" "red-bg" "yellow" "yellow-bg" "blue" "blue-bg")
  "Theme keys that must be valid CSS colors.")

(defparameter *theme-font-keys*
  '("font-mono" "font-body")
  "Theme keys that are font families.")

(defun valid-css-color-p (value)
  "Check if VALUE looks like a valid CSS color (#hex, rgb(), rgba(), named)."
  (or (and (>= (length value) 4)
           (char= (char value 0) #\#)
           (every (lambda (c) (digit-char-p c 16)) (subseq value 1)))
      (search "rgb" value)
      (search "hsl" value)
      (member value '("transparent" "inherit" "currentColor") :test #'equalp)))

(defun valid-url-p (value)
  "Check if VALUE looks like a valid URL."
  (or (search "https://" value)
      (search "http://" value)))

(defun lint-theme-entry (key value)
  "Lint a theme key/value pair. Returns an error string or NIL if valid."
  (cond
    ((member key *theme-color-keys* :test #'equal)
     (unless (valid-css-color-p value)
       (format nil "`~A` = `~A` — expected a CSS color (#hex, rgb(), rgba())" key value)))
    ((equal key "font-url")
     (unless (valid-url-p value)
       (format nil "`font-url` = `~A` — expected a URL starting with https://" key value)))
    ((member key *theme-font-keys* :test #'equal)
     (when (uiop:emptyp value)
       (format nil "`~A` is empty — expected a font family string" key)))
    (t nil)))

(defun parse-theme-toml (content)
  "Parse a simple TOML theme file into a CSS block with validation.
   Returns (VALUES css-string errors-list)."
  (let ((vars nil)
        (imports nil)
        (errors nil))
    (dolist (line (uiop:split-string content :separator '(#\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((uiop:emptyp trimmed) nil)
          ((char= (char trimmed 0) #\#) nil)
          ((char= (char trimmed 0) #\[) nil)
          ((find #\= trimmed)
           (let* ((eq-pos (position #\= trimmed))
                  (key (string-trim '(#\Space) (subseq trimmed 0 eq-pos)))
                  (val (string-trim '(#\Space #\" #\') (subseq trimmed (1+ eq-pos))))
                  (err (lint-theme-entry key val)))
             (if err
                 (push err errors)
                 (if (equal key "font-url")
                     (push (format nil "@import url('~A');" val) imports)
                     (push (format nil "  --~A: ~A;" key val) vars))))))))
    (values
     (when vars
       (format nil "~{~A~%~}html[data-theme=\"custom\"] {~%~{~A~%~}}"
               (nreverse imports) (nreverse vars)))
     (nreverse errors))))

(defun handle-sync-themes (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (repo-path (clingon:getopt cmd :repo)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
           (owner (first parts))
           (user (find-user-by-username owner))
           (disk-path (repo-disk-path owner "cave-themes")))
      (when (and user (probe-file disk-path))
        ;; List all .toml files in the repo root
        (let ((tree (git-tree disk-path :ref "HEAD")))
          (dolist (entry tree)
            (when (and (equal (getf entry :type) "blob")
                       (search ".toml" (getf entry :name)))
              (let* ((filename (getf entry :name))
                     (theme-name (pathname-name (pathname filename)))
                     (content (git-blob disk-path "HEAD" filename)))
                (handler-case
                    (multiple-value-bind (css lint-errors)
                        (parse-theme-toml content)
                      ;; File issue if there are lint errors
                      (when lint-errors
                        (format t "  ⚠ ~A has ~A warning~:P~%" theme-name (length lint-errors))
                        (let ((repo (find-repo owner "cave-themes")))
                          (when repo
                            (create-issue :repo-id (getf repo :id)
                                          :author-id (getf user :id)
                                          :title (format nil "Theme lint: ~A" filename)
                                          :body (format nil "Found ~A issue~:P in `~A`:~%~%~{- ~A~%~}~%Fix these and push again."
                                                        (length lint-errors) filename lint-errors)))))
                      (if css
                          (progn
                            (upsert-user-theme (getf user :id) theme-name css)
                            (format t "  ✓ Synced theme: ~A~%" theme-name))
                          (progn
                            (format t "  ✗ Empty theme: ~A~%" theme-name)
                            (let ((repo (find-repo owner "cave-themes")))
                              (when repo
                                (create-issue :repo-id (getf repo :id)
                                              :author-id (getf user :id)
                                              :title (format nil "Theme parse error: ~A" filename)
                                              :body (format nil "The theme file `~A` produced no valid CSS variables.~%~%Expected format:~%```toml~%bg = \"#282a36\"~%accent = \"#ff79c6\"~%```" filename)))))))
                  (error (e)
                    (format t "  ✗ Error parsing ~A: ~A~%" theme-name e)
                    (let ((repo (find-repo owner "cave-themes")))
                      (when repo
                        (create-issue :repo-id (getf repo :id)
                                      :author-id (getf user :id)
                                      :title (format nil "Theme parse error: ~A" filename)
                                      :body (format nil "Error parsing `~A`:~%~%```~%~A~%```" filename e))))))))))))
    (disconnect-db)))

;;; --- SYNC-MIRRORS subcommand ---

(defun make-sync-mirrors-command ()
  (clingon:make-command
   :name "sync-mirrors"
   :description "Sync repo mirrors (push mirrors for a repo, or all due pull mirrors)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo
              :description "Repo path as owner/name (for push mirrors). Omit for pull mirror sync."))
   :handler #'handle-sync-mirrors))

(defun handle-sync-mirrors (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (repo-path (clingon:getopt cmd :repo)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (if repo-path
        ;; Push mirrors for a specific repo
        (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
               (owner (first parts))
               (name (second parts))
               (repo (find-repo owner name)))
          (when repo
            (let ((mirrors (list-mirrors (getf repo :id)))
                  (disk-path (repo-disk-path owner name)))
              (dolist (m mirrors)
                (when (and (equal (getf m :direction) "push") (getf m :enabled))
                  (format t "~&Pushing to ~A...~%" (getf m :remote-url))
                  (multiple-value-bind (ok err)
                      (git-push-mirror disk-path (getf m :remote-url) (getf m :auth-token))
                    (if ok
                        (progn
                          (format t "  ✓ Push mirror synced~%")
                          (update-mirror-sync (getf m :id)))
                        (progn
                          (format t "  ✗ Push mirror failed: ~A~%" err)
                          (update-mirror-sync (getf m :id) :error err)))))))))
        ;; Pull mirrors — sync all due
        (let ((due (list-due-pull-mirrors)))
          (dolist (m due)
            (let* ((owner (getf m :owner-name))
                   (name (getf m :name))
                   (disk-path (repo-disk-path owner name)))
              (format t "~&Pulling ~A/~A from ~A...~%" owner name (getf m :remote-url))
              (multiple-value-bind (ok err)
                  (git-pull-mirror disk-path (getf m :remote-url) (getf m :auth-token))
                (if ok
                    (progn
                      (format t "  ✓ Pull mirror synced~%")
                      (update-mirror-sync (getf m :id)))
                    (progn
                      (format t "  ✗ Pull mirror failed: ~A~%" err)
                      (update-mirror-sync (getf m :id) :error err))))))))
    (disconnect-db)))

;;; --- SYNC-ADVISORIES subcommand ---

(defun make-sync-advisories-command ()
  (clingon:make-command
   :name "sync-advisories"
   :description "Sync OSV security advisories for packages in the dependency graph"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "ecosystem" :key :ecosystem
              :description "Comma-separated OSV ecosystems to limit to (e.g. npm,Go,PyPI)")
             (clingon:make-option :string
              :long-name "feed" :key :feed
              :description "Comma-separated OSV feed URL(s) to also ingest (e.g. a
                           Common Lisp advisory feed OSV's API doesn't carry).
                           Adds to any :advisory-feeds in config."))
   :handler #'handle-sync-advisories))

(defun handle-sync-advisories (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (eco (clingon:getopt cmd :ecosystem))
        (feed (clingon:getopt cmd :feed)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let ((ecosystems (when (and eco (plusp (length eco)))
                        (mapcar (lambda (s) (string-trim " " s))
                                (uiop:split-string eco :separator '(#\,)))))
          ;; Feeds: :advisory-feeds in config, plus any --feed.
          (feeds (remove-duplicates
                  (append (configured-advisory-feeds)
                          (when (and feed (plusp (length feed)))
                            (mapcar (lambda (s) (string-trim " " s))
                                    (uiop:split-string feed :separator '(#\,)))))
                  :test #'string=)))
      (run-advisory-sync :ecosystems ecosystems :feeds feeds :verbose t))
    (disconnect-db)))

;;; --- DEPS-SCAN subcommand ---

(defun make-deps-scan-command ()
  (clingon:make-command
   :name "deps-scan"
   :description "Scan a repo's dependencies into the graph (runs syft, or ingests --sbom)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/name")
             (clingon:make-option :string
              :long-name "ref" :key :ref
              :description "Branch/ref to label the scan (default: repo default branch)")
             (clingon:make-option :string
              :long-name "sbom" :key :sbom
              :description "Path to a CycloneDX JSON SBOM (skip running syft)"))
   :handler #'handle-deps-scan))

(defun handle-deps-scan (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (repo-path (clingon:getopt cmd :repo))
        (ref (clingon:getopt cmd :ref))
        (sbom-file (clingon:getopt cmd :sbom)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
           (owner (first parts))
           (name (second parts)))
      (if sbom-file
          ;; Direct ingest of a pre-built SBOM (no runner).
          (let ((n (ingest-sbom-json owner name (uiop:read-file-string sbom-file)
                                     :ref ref)))
            (if n
                (format t "~&Ingested ~A dependencies for ~A.~%" n repo-path)
                (format t "~&Unknown repo: ~A~%" repo-path)))
          ;; Runner-based scan: enqueue a workflow job a syft runner picks up.
          (let* ((repo (find-repo owner name))
                 (run (when repo (enqueue-deps-scan (getf repo :id) :ref ref))))
            (cond
              ((null repo) (format t "~&Unknown repo: ~A~%" repo-path))
              ((null run)
               (format t "~&Scanning is disabled (set deps-scan-enabled in config).~%"))
              (t (format t "~&Queued scan run #~A for ~A — a syft runner will pick it up.~%"
                         (getf run :id) repo-path))))))
    (disconnect-db)))

;;; --- DEPS-FIX subcommand ---

(defun make-deps-fix-command ()
  (clingon:make-command
   :name "deps-fix"
   :description "Open a fix PR for a security alert (unambiguous manifest bumps)"
   :options (list
             (make-config-option)
             (clingon:make-option :integer
              :long-name "alert" :key :alert :required t
              :description "Dependency alert id to fix"))
   :handler #'handle-deps-fix))

(defun handle-deps-fix (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (alert-id (clingon:getopt cmd :alert)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let ((result (open-dependency-fix-pr alert-id)))
      (case (getf result :status)
        (:opened (format t "~&Opened fix PR on branch ~A (commit ~A).~%"
                         (getf result :branch)
                         (subseq (getf result :commit) 0 (min 8 (length (getf result :commit))))))
        (:manual (format t "~&Manual fix needed (~A): ~A~%"
                         (getf result :fix-kind) (getf result :reason)))
        (:no-fix (format t "~&No fix version available for this alert.~%"))
        (t (format t "~&Could not open fix: ~A~%" (getf result :reason)))))
    (disconnect-db)))

;;; --- DEPS-AUTOMERGE subcommand ---

(defun make-deps-automerge-command ()
  (clingon:make-command
   :name "deps-automerge"
   :description "Merge eligible, CI-green dependency-fix PRs (per org/repo policy)"
   :options (list (make-config-option))
   :handler #'handle-deps-automerge))

(defun handle-deps-automerge (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (process-dependency-automerge)
    (disconnect-db)))

;;; --- Top-level command ---

(defun make-app ()
  "Create the top-level CLI command with subcommands."
  (clingon:make-command
   :name "cave-server"
   :version +version+
   :description "Cave server — self-hosted code forge"
   :authors '("Cave contributors")
   :license "MIT"
   :sub-commands (list (make-init-command)
                       (make-usher-migrate-users-command)
                       (make-usher-add-user-command)
                       (make-serve-command)
                       (make-migrate-command)
                       (make-reverify-command)
                       (make-backfill-languages-command)
                       (make-git-shell-command)
                       (make-git-proxy-command)
                       (make-update-keys-command)
                       (make-run-checks-command)
                       (make-post-receive-command)
                       (make-runner-command)
                       (make-runner-token-command)
                       (make-sync-mirrors-command)
                       (make-sync-themes-command)
                       (make-sync-advisories-command)
                       (make-deps-scan-command)
                       (make-deps-fix-command)
                       (make-deps-automerge-command))
   :handler (lambda (cmd)
              (clingon:print-usage cmd *standard-output*))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      (uiop:quit 1))))
