;;; main.lisp — CLI entry point with subcommands
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

(version-string:define-version-parameter +version+ :cave)

;;; --- Shared options ---

(defun make-config-option ()
  (clingon:make-option :filepath
   :short-name #\c
   :long-name "config"
   :key :config
   :description "Path to cave.conf"
   :initial-value "cave.conf"))

;;; --- INIT subcommand ---

(defun make-init-command ()
  (clingon:make-command
   :name "init"
   :description "Initialize the database and run migrations"
   :options (list
             (make-config-option))
   :handler #'handle-init
   :examples '(("Initialize Cave:" .
                "cave init --config cave.conf"))))

(defun handle-init (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (ensure-data-dirs)
    (handler-case (connect-db)
      (error (e)
        (format *error-output*
                "~&Failed to connect to database ~A@~A:~A~%  ~A~%~%~
                 Make sure PostgreSQL is running and the database exists:~%  ~
                 sudo dnf install postgresql-server postgresql~%  ~
                 sudo postgresql-setup --initdb~%  ~
                 sudo systemctl enable --now postgresql~%  ~
                 sudo -u postgres createuser --createdb cave~%  ~
                 sudo -u postgres createdb -O cave cave~%"
                (config-value :db-name)
                (config-value :db-host)
                (config-value :db-port)
                e)
        (uiop:quit 1)))
    (run-migrations)
    (disconnect-db)
    (format t "~&Cave initialized successfully.~%")))

;;; --- MIGRATE subcommand ---

(defun make-migrate-command ()
  (clingon:make-command
   :name "migrate"
   :description "Run pending database migrations"
   :options (list (make-config-option))
   :handler #'handle-migrate))

(defun handle-migrate (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (connect-db)
    (let ((applied (run-migrations)))
      (disconnect-db)
      (format t "~&~A migration~:P applied.~%" applied))))

;;; --- SERVE subcommand ---

(defun make-serve-command ()
  (clingon:make-command
   :name "serve"
   :description "Start the Cave forge server"
   :options (list
             (make-config-option)
             (clingon:make-option :integer
              :short-name #\p :long-name "port" :key :port
              :description "HTTP port (overrides config)")
             (clingon:make-option :integer
              :short-name #\s :long-name "slynk-port" :key :slynk-port
              :description "Slynk REPL port"))
   :handler #'handle-serve
   :examples '(("Start Cave on port 8080:" . "cave serve")
               ("Start with custom port:" . "cave serve -p 9090"))))

(defun handle-serve (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (port-override (clingon:getopt cmd :port))
        (slynk-port (clingon:getopt cmd :slynk-port)))

    ;; Load environment variables if .env exists
    (handler-case (.env:load-env (merge-pathnames ".env"))
      (file-error () nil)
      (.env:malformed-entry ()
        (format *error-output* "Malformed entry in .env~%")
        (uiop:quit 1)))

    (load-config config-path)
    (ensure-data-dirs)

    ;; Connect to database and check schema
    (handler-case (connect-db)
      (error (e)
        (format *error-output*
                "~&Failed to connect to database ~A@~A:~A~%  ~A~%~%~
                 Make sure PostgreSQL is running and the database exists:~%  ~
                 sudo dnf install postgresql-server postgresql~%  ~
                 sudo postgresql-setup --initdb~%  ~
                 sudo systemctl enable --now postgresql~%  ~
                 sudo -u postgres createuser --createdb cave~%  ~
                 sudo -u postgres createdb -O cave cave~%"
                (config-value :db-name)
                (config-value :db-host)
                (config-value :db-port)
                e)
        (uiop:quit 1)))
    (handler-case (check-schema-version)
      (error (e)
        (format *error-output* "~&~A~%Run: cave migrate --config ~A~%"
                e config-path)
        (uiop:quit 1)))

    (let ((port (or port-override (config-value :http-port 8080))))
      (bt:with-lock-held (*server-lock*)
        ;; Slynk
        (when slynk-port
          (slynk:create-server :port slynk-port :interface "0.0.0.0" :dont-close t)
          (llog:info "Slynk server started" :port slynk-port))

        ;; Start HTTP
        (start-server port)
        (llog:info "Cave listening" :version +version+ :port port)

        ;; Wait forever
        (bt:condition-wait *shutdown-cv* *server-lock*)))))

;;; --- GIT-SHELL subcommand ---

(defun make-git-shell-command ()
  (clingon:make-command
   :name "git-shell"
   :description "Handle an SSH git operation (called by sshd, not directly)"
   :options (list
             (make-config-option)
             (clingon:make-option :integer
              :long-name "key-id" :key :key-id :required t
              :description "SSH key ID from the database"))
   :handler #'handle-git-shell))

;;; --- UPDATE-KEYS subcommand ---

(defun make-update-keys-command ()
  (clingon:make-command
   :name "update-keys"
   :description "Regenerate the authorized_keys file from the database"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "output" :key :output
              :description "Path to write authorized_keys"
              :initial-value (namestring
                              (merge-pathnames ".ssh/authorized_keys"
                                               (user-homedir-pathname))))
             (clingon:make-option :string
              :long-name "cave-shell" :key :cave-shell
              :description "Path to cave-shell.sh"
              :initial-value "/usr/bin/cave-shell.sh"))
   :handler #'handle-update-keys
   :examples '(("Update authorized_keys:" .
                "cave update-keys --config /etc/cave.conf"))))

;;; --- RUN-CHECKS subcommand ---

(defun make-run-checks-command ()
  (clingon:make-command
   :name "run-checks"
   :description "Run server-side push checks for a repo (called by pre-receive hook)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/name"))
   :handler #'handle-run-checks))

(defun handle-run-checks (cmd)
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
      (unless repo
        (format *error-output* "~&cave: repo not found: ~A~%" repo-path)
        (disconnect-db)
        (uiop:quit 0)) ;; Don't block push for unknown repos
      (let ((checks (list-check-configs (getf repo :id)))
            (disk-path (repo-disk-path owner name))
            (failed nil))
        (dolist (chk checks)
          (when (getf chk :enabled)
            (format t "~&Running check: ~A~%" (getf chk :name))
            (handler-case
                (multiple-value-bind (output error-output exit-code)
                    (uiop:run-program
                     (list "bash" "-c" (getf chk :command))
                     :output '(:string :stripped t)
                     :error-output '(:string :stripped t)
                     :ignore-error-status t
                     :directory (namestring disk-path))
                  (if (zerop exit-code)
                      (format t "  ✓ ~A passed~%" (getf chk :name))
                      (progn
                        (format t "  ✗ ~A failed (exit ~A)~%" (getf chk :name) exit-code)
                        (when output (format t "    ~A~%" output))
                        (when (and error-output (not (uiop:emptyp error-output)))
                          (format t "    ~A~%" error-output))
                        (push (getf chk :name) failed))))
              (error (e)
                (format t "  ✗ ~A error: ~A~%" (getf chk :name) e)
                (push (getf chk :name) failed)))))
        (disconnect-db)
        (when failed
          (format *error-output* "~&cave: push rejected — ~A check~:P failed: ~{~A~^, ~}~%"
                  (length failed) (nreverse failed))
          (uiop:quit 1))))))

;;; --- SYNC-THEMES subcommand ---

(defun make-sync-themes-command ()
  (clingon:make-command
   :name "sync-themes"
   :description "Sync user themes from a .cave-themes repo"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/.cave-themes"))
   :handler #'handle-sync-themes))

(defun parse-theme-toml (content)
  "Parse a simple TOML theme file into a CSS variable block.
   Expected format: [variables] followed by key = \"value\" pairs."
  (let ((vars nil))
    (dolist (line (uiop:split-string content :separator '(#\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (when (and (find #\= trimmed)
                   (not (char= (char trimmed 0) #\[))
                   (not (char= (char trimmed 0) #\#)))
          (let* ((eq-pos (position #\= trimmed))
                 (key (string-trim '(#\Space) (subseq trimmed 0 eq-pos)))
                 (val (string-trim '(#\Space #\" #\') (subseq trimmed (1+ eq-pos)))))
            (push (format nil "  --~A: ~A;" key val) vars)))))
    (when vars
      (format nil "html[data-theme=\"custom\"] {~%~{~A~%~}}" (nreverse vars)))))

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
           (disk-path (repo-disk-path owner ".cave-themes")))
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
                    (let ((css (parse-theme-toml content)))
                      (if css
                          (progn
                            (upsert-user-theme (getf user :id) theme-name css)
                            (format t "  ✓ Synced theme: ~A~%" theme-name))
                          (progn
                            (format t "  ✗ Empty theme: ~A~%" theme-name)
                            ;; Open issue on the themes repo
                            (let ((repo (find-repo owner ".cave-themes")))
                              (when repo
                                (create-issue :repo-id (getf repo :id)
                                              :author-id (getf user :id)
                                              :title (format nil "Theme parse error: ~A" filename)
                                              :body (format nil "The theme file `~A` produced no valid CSS variables.~%~%Expected format:~%```toml~%bg = \"#282a36\"~%accent = \"#ff79c6\"~%```" filename)))))))
                  (error (e)
                    (format t "  ✗ Error parsing ~A: ~A~%" theme-name e)
                    (let ((repo (find-repo owner ".cave-themes")))
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

;;; --- Top-level command ---

(defun make-app ()
  "Create the top-level CLI command with subcommands."
  (clingon:make-command
   :name "cave"
   :version +version+
   :description "Cave — A self-hosted code forge"
   :authors '("Cave contributors")
   :license "MIT"
   :sub-commands (list (make-init-command)
                       (make-serve-command)
                       (make-migrate-command)
                       (make-git-shell-command)
                       (make-update-keys-command)
                       (make-run-checks-command)
                       (make-sync-mirrors-command)
                       (make-sync-themes-command))
   :handler (lambda (cmd)
              (clingon:print-usage cmd *standard-output*))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      (uiop:quit 1))))
