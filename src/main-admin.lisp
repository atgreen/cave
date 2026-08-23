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
                "cave-server init --config cave.conf"))))

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

(defun make-reverify-command ()
  (clingon:make-command
   :name "reverify"
   :description "Re-verify recorded commit signatures against current keys"
   :options (list (make-config-option))
   :handler #'handle-reverify))

(defun handle-reverify (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (connect-db)
    (let ((n (reverify-all-signatures)))
      (disconnect-db)
      (format t "~&Re-verified ~D commit~:P.~%" n))))

(defun make-backfill-languages-command ()
  (clingon:make-command
   :name "backfill-languages"
   :description "Compute and store each repo's primary language (one-time)"
   :options (list (make-config-option))
   :handler #'handle-backfill-languages))

(defun handle-backfill-languages (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (connect-db)
    (let ((n 0))
      (dolist (repo (list-all-repos))
        (let* ((owner (getf repo :owner-name))
               (name (getf repo :name))
               (ref (or (chamber-get-default-branch owner name) "main"))
               (primary (first (first (ignore-errors
                                       (chamber-language-stats owner name ref))))))
          (set-repo-primary-language (getf repo :id) primary)
          (when primary (incf n))))
      (disconnect-db)
      (format t "~&Set primary language on ~D repo~:P.~%" n))))

(defun make-usher-migrate-users-command ()
  (clingon:make-command
   :name "usher-migrate-users"
   :description "Provision Usher accounts from existing cave_users (one-time)"
   :options (list (make-config-option))
   :handler #'handle-usher-migrate-users))

(defun handle-usher-migrate-users (cmd)
  (let ((config-path (clingon:getopt cmd :config)))
    (load-config config-path)
    (connect-db)
    (init-usher)
    (let ((created (usher-migrate-users)))
      (disconnect-db)
      (if created
          (progn
            (format t "~&Provisioned ~D Usher account~:P. Temporary passwords ~
                       (change on first login):~%~%" (length created))
            (loop for (username . pw) in created
                  do (format t "  ~16A ~A~%" username pw))
            (format t "~%"))
          (format t "~&No new accounts (all cave_users already exist in Usher).~%")))))

(defun make-usher-add-user-command ()
  (clingon:make-command
   :name "usher-add-user"
   :description "Create (or update) a local Usher account non-interactively.
                 With --admin, grant the cave-admin group. Used to bootstrap the
                 first administrator on a fresh instance (dev harness, deploy)."
   :options (list (make-config-option)
                  (clingon:make-option :string
                   :long-name "username" :key :username :required t
                   :description "Login username")
                  (clingon:make-option :string
                   :long-name "password" :key :password :required t
                   :description "Initial password")
                  (clingon:make-option :string
                   :long-name "email" :key :email
                   :description "Email address (marks the account email-verified)")
                  (clingon:make-option :string
                   :long-name "display-name" :key :display-name
                   :description "Display name (defaults to the username)")
                  (clingon:make-option :flag
                   :long-name "admin" :key :admin
                   :description "Grant the cave-admin group"))
   :handler #'handle-usher-add-user))

(defun handle-usher-add-user (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (username (clingon:getopt cmd :username))
        (password (clingon:getopt cmd :password))
        (email (clingon:getopt cmd :email))
        (display-name (clingon:getopt cmd :display-name))
        (admin (clingon:getopt cmd :admin)))
    (load-config config-path)
    (connect-db)
    (init-usher)
    (usher-add-user username password :email email
                    :display-name display-name :admin admin)
    (disconnect-db)
    (format t "~&Provisioned Usher account ~S~:[~; (cave-admin)~].~%"
            username admin)))

;;; --- System repos (cave-themes, cave-landing): create + seed at startup ---

(defun system-repo-empty-p (owner name)
  "True when OWNER/NAME's bare repo has no commits (no branches)."
  (let ((disk-path (repo-disk-path owner name)))
    (and (probe-file disk-path) (null (git-branches disk-path)))))

(defun seed-system-repo (owner name seed-subdir commit-msg)
  "Push the files under SEED-SUBDIR (relative to app-root, so they're found both
in dev and in the shipped image) into OWNER/NAME's main branch as one commit."
  (let* ((seed-dir (merge-pathnames seed-subdir (app-root)))
         (disk-path (repo-disk-path owner name))
         (tmpdir (format nil "/tmp/cave-seed-~A"
                         (ironclad:byte-array-to-hex-string (ironclad:random-data 4)))))
    (if (not (probe-file seed-dir))
        (llog:warn "System repo seed dir missing" :dir (namestring seed-dir))
        (handler-case
            (progn
              (uiop:run-program (list "git" "clone" (namestring disk-path) tmpdir)
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir "symbolic-ref" "HEAD" "refs/heads/main")
                                :ignore-error-status t)
              (uiop:run-program (format nil "cp -r ~A* ~A/" (namestring seed-dir) tmpdir)
                                :output :string :error-output :string :force-shell t)
              (uiop:run-program (list "git" "-C" tmpdir "add" "-A")
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir
                                      "-c" "user.name=Cave" "-c" "user.email=cave@localhost"
                                      "commit" "-m" commit-msg)
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir "push" "origin" "HEAD:main")
                                :output :string :error-output :string)
              ;; Make sure the bare repo's default branch points at main.
              (uiop:run-program (list "git" "-C" (namestring disk-path)
                                      "symbolic-ref" "HEAD" "refs/heads/main")
                                :ignore-error-status t)
              (llog:info "Seeded system repo" :repo (format nil "~A/~A" owner name)))
          (error (e)
            (llog:warn "Failed to seed system repo"
                       :repo (format nil "~A/~A" owner name)
                       :error (princ-to-string e)))))
    (uiop:run-program (list "rm" "-rf" tmpdir) :ignore-error-status t)))

(defun ensure-system-repo (name description seed-subdir commit-msg)
  "Ensure cave/NAME exists and is populated: create it (DB row + bare repo) when
missing, and seed it from SEED-SUBDIR when it has no commits — so a missing OR
empty system repo gets populated at startup."
  (let ((cave-org (find-org-by-name "cave")))
    (when cave-org
      (unless (find-repo "cave" name)
        (handler-case
            (progn
              (postmodern:query
               (:insert-into 'cave-repos
                :set 'org-id (getf cave-org :id) 'name name 'description description
                :returning '*)
               :plist)
              (init-bare-repo "cave" name)
              (llog:info "Created system repo" :repo (format nil "cave/~A" name)))
          (error (e)
            (llog:warn "Failed to create system repo"
                       :repo name :error (princ-to-string e)))))
      (when (and (find-repo "cave" name) (system-repo-empty-p "cave" name))
        (seed-system-repo "cave" name seed-subdir commit-msg)))))

(defun seed-action-repo (owner name seed-subdir commit-msg tags)
  "Like SEED-SYSTEM-REPO, but also creates each tag in TAGS (e.g. (\"v4\"))
pointing at the seeded commit — actions are referenced owner/repo@<tag>."
  (let* ((seed-dir (merge-pathnames seed-subdir (app-root)))
         (disk-path (repo-disk-path owner name))
         (tmpdir (format nil "/tmp/cave-seed-~A"
                         (ironclad:byte-array-to-hex-string (ironclad:random-data 4)))))
    (if (not (probe-file seed-dir))
        (llog:warn "Action repo seed dir missing" :dir (namestring seed-dir))
        (handler-case
            (progn
              (uiop:run-program (list "git" "clone" (namestring disk-path) tmpdir)
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir "symbolic-ref" "HEAD" "refs/heads/main")
                                :ignore-error-status t)
              (uiop:run-program (format nil "cp -r ~A* ~A/" (namestring seed-dir) tmpdir)
                                :output :string :error-output :string :force-shell t)
              (uiop:run-program (list "git" "-C" tmpdir "add" "-A")
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir
                                      "-c" "user.name=Cave" "-c" "user.email=cave@localhost"
                                      "commit" "-m" commit-msg)
                                :output :string :error-output :string)
              (uiop:run-program (list "git" "-C" tmpdir "push" "origin" "HEAD:main")
                                :output :string :error-output :string)
              ;; Tags (e.g. the floating major v4) so uses: owner/repo@v4 resolves.
              (dolist (tag tags)
                (uiop:run-program (list "git" "-C" tmpdir "tag" "-f" tag)
                                  :ignore-error-status t :output :string :error-output :string)
                (uiop:run-program (list "git" "-C" tmpdir "push" "-f" "origin"
                                        (format nil "refs/tags/~A" tag))
                                  :ignore-error-status t :output :string :error-output :string))
              (uiop:run-program (list "git" "-C" (namestring disk-path)
                                      "symbolic-ref" "HEAD" "refs/heads/main")
                                :ignore-error-status t)
              (llog:info "Seeded action repo" :repo (format nil "~A/~A" owner name) :tags tags))
          (error (e)
            (llog:warn "Failed to seed action repo"
                       :repo (format nil "~A/~A" owner name)
                       :error (princ-to-string e)))))
    (uiop:run-program (list "rm" "-rf" tmpdir) :ignore-error-status t)))

(defun ensure-action-repo (name description seed-subdir commit-msg tags)
  "Ensure actions/NAME exists (DB row + bare repo) and is seeded+tagged from
SEED-SUBDIR when empty. The `actions` org hosts cave-native uses: actions as
real, browsable repos."
  (let ((org (find-org-by-name "actions")))
    (when org
      (unless (find-repo "actions" name)
        (handler-case
            (progn
              (postmodern:query
               (:insert-into 'cave-repos
                :set 'org-id (getf org :id) 'name name 'description description
                :returning '*)
               :plist)
              (init-bare-repo "actions" name)
              (llog:info "Created action repo" :repo (format nil "actions/~A" name)))
          (error (e)
            (llog:warn "Failed to create action repo"
                       :repo name :error (princ-to-string e)))))
      (when (and (find-repo "actions" name) (system-repo-empty-p "actions" name))
        (seed-action-repo "actions" name seed-subdir commit-msg tags)))))

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
   :examples '(("Start Cave on port 8080:" . "cave-server serve")
               ("Start with custom port:" . "cave-server serve -p 9090"))))

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
        (format *error-output* "~&~A~%Run: cave-server migrate --config ~A~%"
                e config-path)
        (uiop:quit 1)))

    ;; Initialize the embedded Usher OIDC provider (migrates usher_* tables,
    ;; loads/persists signing keys, registers the cave client).
    (handler-case (init-usher)
      (error (e)
        (format *error-output* "~&Embedded Usher init failed: ~A~%" e)
        (uiop:quit 1)))

    ;; Ensure the cave system org exists, then pre-populate the system repos:
    ;; created if missing, seeded if empty.
    (unless (find-org-by-name "cave")
      (handler-case
          (progn
            (postmodern:query
             (:insert-into 'cave-orgs
              :set 'name "cave"
                   'display-name "Cave"
                   'description "System organization"
              :returning '*)
             :plist)
            (llog:info "Created cave org"))
        (error () nil)))
    (ensure-system-repo "cave-themes" "Built-in and community themes for Cave"
                        "static/seed/cave-themes/" "Seed example theme and documentation")
    (ensure-system-repo "cave-landing" "Landing page content for this Cave instance"
                        "static/seed/cave-landing/" "Seed default landing page")

    ;; The `actions` org hosts cave-native uses: actions as real, browsable repos.
    (unless (find-org-by-name "actions")
      (handler-case
          (progn
            (postmodern:query
             (:insert-into 'cave-orgs
              :set 'name "actions"
                   'display-name "Actions"
                   'description "Cave-native uses: actions"
              :returning '*)
             :plist)
            (llog:info "Created actions org"))
        (error () nil)))
    (ensure-action-repo "checkout" "Check out the workflow repository (cave-native)"
                        "static/seed/actions/checkout/" "Seed actions/checkout" '("v4"))
    (ensure-action-repo "cache" "Cache files between workflow runs (cave-native)"
                        "static/seed/actions/cache/" "Seed actions/cache" '("v4"))
    (ensure-action-repo "upload-artifact" "Upload a build artifact (cave-native)"
                        "static/seed/actions/upload-artifact/" "Seed actions/upload-artifact" '("v4"))
    (ensure-action-repo "download-artifact" "Download a build artifact (cave-native)"
                        "static/seed/actions/download-artifact/" "Seed actions/download-artifact" '("v4"))

    (let ((port (or port-override (config-value :http-port 8080))))
      (bt2:with-lock-held (*server-lock*)
        ;; Slynk
        (when slynk-port
          (slynk:create-server :port slynk-port :interface "0.0.0.0" :dont-close t)
          (llog:info "Slynk server started" :port slynk-port))

        ;; Start Chamber (git storage service)
        (when (config-value :chamber-enabled)
          (let ((chamber-port (config-value :chamber-port 9444)))
            (handler-case
                (start-chamber chamber-port)
              (error (e)
                (llog:warn "Chamber failed to start — using direct git"
                           :error (princ-to-string e))))))

        ;; Start multi-chamber router if configured
        (let ((nodes (config-value :chamber-nodes)))
          (when (and nodes (> (length nodes) 1))
            (handler-case
                (progn
                  (init-chamber-router nodes)
                  (start-chamber-health-checker))
              (error (e)
                (llog:warn "Chamber router failed to start"
                           :error (princ-to-string e))))))

        ;; Start HTTP
        (start-server port)

        ;; Refresh hooks on disk so the script matches the current binary
        ;; (carries any new query params, new event types, etc.)
        (handler-case (reinstall-all-hooks)
          (error (e)
            (llog:warn "Hook sweep failed" :error (princ-to-string e))))

        ;; Start gRPC runner service
        (let ((grpc-port (config-value :grpc-port 9443)))
          (handler-case
              (progn
                (start-grpc-server grpc-port)
                (llog:info "gRPC runner service started" :port grpc-port))
            (error (e)
              (llog:warn "gRPC server failed to start — runners disabled"
                         :error (princ-to-string e)
                         :detail (with-output-to-string (s)
                                   (trivial-backtrace:print-backtrace-to-stream s))))))
        ;; In-process periodic scheduler (advisory sync, mirror pulls). Built in
        ;; so deployment stays self-contained — no host cron/systemd timers.
        (when (config-value :scheduler-enabled t)
          (bt2:make-thread
           (lambda ()
             (loop
               ;; Tick at the finest task cadence (reap claims at 120s); each
               ;; task is DB-claimed at its own interval, so a short tick just
               ;; means cheap claim checks, not duplicate work.
               (sleep 60)
               (handler-case
                   (postmodern:with-connection *db-spec* (run-scheduled-tasks))
                 (error (e) (llog:warn "Scheduler tick failed"
                                       :error (princ-to-string e))))))
           :name "cave-scheduler")
          (llog:info "Scheduler started"
                     :advisory-sync-interval-hours
                     (config-value :advisory-sync-interval-hours 24)))

        (llog:info "Cave listening" :version +version+ :port port)

        ;; Wait forever
        (bt2:condition-wait *shutdown-cv* *server-lock*)))))

(defun %scheduled-mirror-pull ()
  "Pull every due pull-mirror (each gated by its own interval_minutes)."
  (dolist (m (list-due-pull-mirrors))
    (let ((disk (repo-disk-path (getf m :owner-name) (getf m :name))))
      (multiple-value-bind (ok err)
          (git-pull-mirror disk (getf m :remote-url) (getf m :auth-token))
        (if ok (update-mirror-sync (getf m :id))
               (update-mirror-sync (getf m :id) :error err))))))

(defun run-scheduled-tasks ()
  "One scheduler tick: run any periodic task that's due, each atomically claimed
   via the DB so concurrent instances never double-run. Called inside a DB
   connection on the scheduler thread."
  ;; Advisory sync: re-pull OSV + feeds, re-match the existing graph against newly
  ;; published advisories (no rescan needed), and dispatch fix PRs.
  (let ((hrs (config-value :advisory-sync-interval-hours 24)))
    (when (and (numberp hrs) (plusp hrs)
               (claim-scheduled-task "sync-advisories" (* 3600 hrs)))
      (llog:info "Scheduled advisory sync starting")
      (handler-case
          (run-advisory-sync :feeds (configured-advisory-feeds) :verbose nil)
        (error (e) (llog:warn "Scheduled advisory sync failed"
                              :error (princ-to-string e))))
      (llog:info "Scheduled advisory sync done")))
  ;; Mirror pulls: cheap check every 5 min; each mirror has its own cadence.
  (when (claim-scheduled-task "sync-mirrors" 300)
    (handler-case (%scheduled-mirror-pull)
      (error (e) (llog:warn "Scheduled mirror sync failed"
                            :error (princ-to-string e)))))
  ;; Reap zombie workflow runs: a runner that dies mid-job leaves the run
  ;; 'running' forever, which blocks merges on required checks.
  (when (claim-scheduled-task "reap-workflows" 120)
    (handler-case
        (multiple-value-bind (requeued failed)
            (reap-stale-workflow-jobs
             :max-minutes (config-value :workflow-run-max-minutes 120))
          (when (or (plusp requeued) (plusp failed))
            (llog:info "Reaped workflow jobs" :requeued requeued :failed failed)))
      (error (e) (llog:warn "Workflow reap failed"
                            :error (princ-to-string e))))))

;;; --- GIT-SHELL subcommand ---

(defun make-git-shell-command ()
  (clingon:make-command
   :name "git-shell"
   :description "Handle an SSH git operation (called by sshd, not directly)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "key-id" :key :key-id :required t
              :description "SSH key ID (integer for a user key, d<id> for a deploy key)"))
   :handler #'handle-git-shell))

;;; --- GIT-PROXY subcommand ---

(defun make-git-proxy-command ()
  (clingon:make-command
   :name "git-proxy"
   :description "Proxy git protocol through Chamber gRPC (called by cave-shell.sh)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "command" :key :command :required t
              :description "git-upload-pack or git-receive-pack")
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repository as owner/name"))
   :handler #'handle-git-proxy))

