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
               (sleep 300)
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
  (when (claim-scheduled-task "reap-workflows" 300)
    (handler-case
        (let ((n (reap-stale-workflow-jobs
                  :max-minutes (config-value :workflow-run-max-minutes 120))))
          (when (plusp n)
            (llog:info "Reaped stale workflow runs" :count n)))
      (error (e) (llog:warn "Workflow reap failed"
                            :error (princ-to-string e))))))

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

(defun handle-git-proxy (cmd)
  "Bridge SSH stdin/stdout to Chamber gRPC bidirectional stream.
   Single-threaded: the main thread reads gRPC responses and forwards to stdout,
   a reader thread drives the HTTP/2 connection, and a sender thread reads stdin."
  (let* ((config-path (clingon:getopt cmd :config))
         (git-command (clingon:getopt cmd :command))
         (repo-path (clingon:getopt cmd :repo))
         (parts (uiop:split-string repo-path :separator '(#\/)))
         (owner (first parts))
         (repo-name (second parts)))
    (unless (and owner repo-name)
      (format *error-output* "cave: invalid repo path~%")
      (uiop:quit 1))
    (load-config config-path)
    ;; Create binary stdin/stdout streams
    (let ((bin-in (sb-sys:make-fd-stream 0 :input t
                                          :element-type '(unsigned-byte 8)
                                          :buffering :full
                                          :name "git-proxy-stdin"))
          (bin-out (sb-sys:make-fd-stream 1 :output t
                                           :element-type '(unsigned-byte 8)
                                           :buffering :full
                                           :name "git-proxy-stdout")))
      ;; Connect to the correct Chamber node
      (let* ((is-write (equal git-command "git-receive-pack"))
             (nodes-config (config-value :chamber-nodes))
             (multi-p (and nodes-config (> (length nodes-config) 1)))
             (channel
               (if multi-p
                   ;; Multi-chamber: look up node assignment from DB
                   (progn
                     (connect-db)
                     (unwind-protect
                         (let* ((repo (find-repo owner repo-name))
                                (repo-id (when repo (getf repo :id)))
                                (node (when repo-id
                                        (if is-write
                                            (repo-primary-node repo-id)
                                            (let ((healthy (repo-healthy-nodes repo-id)))
                                              (if healthy
                                                  (nth (random (length healthy)) healthy)
                                                  (repo-primary-node repo-id))))))
                                (addr (when node (getf node :address)))
                                (colon (when addr (position #\: addr :from-end t)))
                                (host (if colon (subseq addr 0 colon) (or addr "127.0.0.1")))
                                (port (if colon
                                          (parse-integer (subseq addr (1+ colon)) :junk-allowed t)
                                          (config-value :chamber-port 9444))))
                           (ag-grpc:make-channel host (or port 9444) :timeout nil))
                       (disconnect-db)))
                   ;; Single-chamber: use config URL
                   (let* ((url (config-value :chamber-url))
                          (host (if url
                                    (let ((pos (search "://" url)))
                                      (if pos (subseq url (+ pos 3)) url))
                                    "127.0.0.1"))
                          (port-str (let ((colon (position #\: host :from-end t)))
                                      (when colon (subseq host (1+ colon)))))
                          (host-only (let ((colon (position #\: host :from-end t)))
                                       (if colon (subseq host 0 colon) host)))
                          (port (if port-str
                                    (parse-integer port-str :junk-allowed t)
                                    (config-value :chamber-port 9444))))
                     (ag-grpc:make-channel host-only port :timeout nil))))
             (conn (ag-grpc::channel-connection channel))
             (method (if (equal git-command "git-receive-pack")
                         "/cave.chamber.Chamber/ReceivePack"
                         "/cave.chamber.Chamber/UploadPack"))
             (stream (ag-grpc:call-bidirectional-streaming
                      channel method
                      :response-type 'cave::pack-data)))
        ;; Start a dedicated reader thread for the HTTP/2 connection.
        ;; This drives frame processing so stream-read-message and
        ;; stream-send (flow control) work from separate threads.
        (setf (ag-http2:connection-reader-thread-active-p conn) t)
        (let ((reader-done nil))
          (bt2:make-thread
           (lambda ()
             (handler-case
                 (loop while (not (member (ag-http2:connection-state conn)
                                          '(:closing :closed)))
                       do (ag-http2:connection-read-frame conn))
               (error () nil))
             (setf reader-done t)
             ;; Wake any waiters on flow control
             (bt2:condition-notify (ag-http2:connection-flow-control-cv conn)))
           :name "git-proxy-reader")
          (handler-case
              (progn
                ;; Send init message with repo info
                (ag-grpc:stream-send stream
                  (make-instance 'cave::pack-data
                                 :data (make-array 0 :element-type '(unsigned-byte 8))
                                 :owner owner :repo-name repo-name))
                ;; Stdin→gRPC sender thread
                (let ((sender-thread
                        (bt2:make-thread
                         (lambda ()
                           (let ((buf (make-array 32768 :element-type '(unsigned-byte 8))))
                             (handler-case
                                 (loop
                                   (let ((n (read-sequence buf bin-in)))
                                     (when (zerop n) (return))
                                     (ag-grpc:stream-send stream
                                       (make-instance 'cave::pack-data
                                                      :data (subseq buf 0 n)))))
                               (error () nil))
                             (handler-case (ag-grpc:stream-close-send stream)
                               (error () nil))))
                         :name "git-proxy-sender")))
                  ;; gRPC→stdout in main thread
                  (loop
                    (let ((msg (ag-grpc:stream-read-message stream)))
                      (unless msg (return))
                      (let ((data (slot-value msg 'cave::data)))
                        (when (and data (plusp (length data)))
                          (write-sequence data bin-out)
                          (force-output bin-out)))))
                  (bt2:join-thread sender-thread))
                (uiop:quit 0))
            (error (e)
              (format *error-output* "cave: proxy error: ~A~%" e)
              (uiop:quit 1))))))))

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
                "cave-server update-keys --config /etc/cave.conf"))))

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

(defun %pushed-shas-from-stdin ()
  "Read pre-receive ref updates (`<old> <new> <ref>` per line) from stdin and
   return the distinct non-zero new shas. Git closes stdin after the ref list,
   so this returns promptly; NIL when stdin carries none."
  (let ((shas nil))
    (handler-case
        (loop for line = (read-line *standard-input* nil nil)
              while line
              do (let ((parts (uiop:split-string
                               (string-trim '(#\Space #\Tab #\Return) line)
                               :separator '(#\Space))))
                   (when (>= (length parts) 3)
                     (let ((new (second parts)))
                       (unless (every (lambda (c) (char= c #\0)) new)
                         (pushnew new shas :test #'equal))))))
      (error () nil))
    (nreverse shas)))

(defun %git-head-sha (bare-path)
  "Resolve HEAD of a bare repo to a sha, or NIL (e.g. empty repo)."
  (let ((out (nth-value 0 (uiop:run-program
                           (list "git" "-C" (namestring bare-path) "rev-parse" "HEAD")
                           :output '(:string :stripped t) :ignore-error-status t))))
    (when (and out (plusp (length out))) out)))

(defun %make-check-workdir ()
  "Create a private temp dir for a check worktree. Returns its path (no trailing /)."
  (string-trim '(#\Newline #\Space)
               (nth-value 0 (uiop:run-program
                             (list "mktemp" "-d" "/tmp/cave-check-XXXXXX")
                             :output '(:string :stripped t)))))

(defun %extract-tree (bare-path sha dest-dir)
  "Extract the tree at SHA from the bare repo into DEST-DIR as plain files (no
   .git, so the check can't reach the repo or network through git). Returns T."
  (let ((tarfile (format nil "~A.tar" dest-dir)))
    (unwind-protect
         (and (zerop (nth-value 2 (uiop:run-program
                                   (list "git" "-C" (namestring bare-path)
                                         "archive" "--format=tar" "-o" tarfile sha)
                                   :ignore-error-status t
                                   :output nil :error-output nil)))
              (zerop (nth-value 2 (uiop:run-program
                                   (list "tar" "-xf" tarfile "-C" dest-dir)
                                   :ignore-error-status t
                                   :output nil :error-output nil))))
      (ignore-errors (delete-file tarfile)))))

(defvar *check-net-isolation* :unprobed
  "Cached result of probing whether `unshare -n` works in this environment.")

(defun %check-net-isolation-available-p ()
  "True when `unshare -n` succeeds here (needs root + an unmasked runtime).
   Probed once per process."
  (when (eq *check-net-isolation* :unprobed)
    (setf *check-net-isolation*
          (handler-case
              (zerop (nth-value 2 (uiop:run-program
                                   (list "unshare" "-n" "true")
                                   :ignore-error-status t
                                   :output nil :error-output nil)))
            (error () nil))))
  *check-net-isolation*)

(defun %sandbox-argv (command workdir sha repo-path timeout isolate-net mem-mb)
  "Build the argv to run a check COMMAND against the extracted WORKDIR under a
   sandbox: scrubbed environment, a wall-clock TIMEOUT, an optional per-process
   memory cap, and (when ISOLATE-NET) a network namespace. CWD is set to WORKDIR
   by the caller. We deliberately do NOT use `ulimit -u`: RLIMIT_NPROC is
   per-UID across the whole container, so capping it would starve the long-lived
   cave-server threads (running as the same uid) of forks. `timeout` bounds
   runaway/fork-bomb code instead."
  (let ((ulimit (if mem-mb
                    (format nil "ulimit -v ~A 2>/dev/null; " (* mem-mb 1024))
                    "")))
    ;; Landlock filesystem confinement (landrun) around the whole check. :exec t
    ;; grants the worktree --rwx so a check may run scripts it shipped (./build.sh).
    ;; landrun's network policy mirrors the check's: when isolate-net is on we also
    ;; deny TCP in landrun (belt to `unshare -n`); when the admin allows check
    ;; network (:checks-allow-network), landrun allows it too so the check works.
    (sandbox-wrap workdir
     (append
     (when isolate-net (list "unshare" "-n"))
     (list "env" "-i"
           "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
           (format nil "HOME=~A" workdir)
           "LANG=C.UTF-8"
           (format nil "CAVE_COMMIT=~A" sha)
           (format nil "CAVE_REPO=~A" repo-path))
     (list "timeout" "-k" "10" (format nil "~A" timeout))
     (list "bash" "-c" (format nil "~A~A" ulimit command)))
     :exec t :network (not isolate-net))))

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
      (let ((checks (remove-if-not (lambda (c) (getf c :enabled))
                                   (list-check-configs (getf repo :id))))
            (disk-path (repo-disk-path owner name)))
        ;; Nothing to enforce — accept without reading stdin or building worktrees.
        (when (null checks)
          (disconnect-db)
          (uiop:quit 0))
        (let* ((shas (or (%pushed-shas-from-stdin)
                         (let ((h (%git-head-sha disk-path))) (when h (list h)))))
               (want-isolation (not (config-value :checks-allow-network)))
               (isolation-ok (and want-isolation (%check-net-isolation-available-p)))
               (mem (config-value :checks-memory-mb))  ; nil = no memory cap
               (default-timeout (config-value :checks-timeout-seconds 120))
               (failed nil))
          ;; Network isolation requested but the runtime forbids `unshare -n`
          ;; (the default cave container lacks the capability). Fail closed when
          ;; an admin demands isolation; otherwise warn and run without it.
          (when (and want-isolation (not isolation-ok))
            (if (config-value :checks-require-network-isolation)
                (progn
                  (format *error-output*
                          "~&cave: push rejected — network isolation required but unavailable ~
                           (the cave container lacks unshare; grant the capability or run ~
                           checks on a runner)~%")
                  (disconnect-db)
                  (uiop:quit 1))
                (format *error-output*
                        "~&cave: warning — network isolation unavailable; running checks without it~%")))
          (dolist (sha shas)
            (let ((wt (%make-check-workdir)))
              (unwind-protect
                   (if (not (%extract-tree disk-path sha wt))
                       (progn
                         (format *error-output*
                                 "~&cave: could not extract tree ~A for checks~%" sha)
                         (push "(tree extract)" failed))
                       (dolist (chk checks)
                         (let* ((to (let ((t0 (getf chk :timeout-seconds)))
                                      (if (and t0 (plusp t0)) t0 default-timeout)))
                                (argv (%sandbox-argv (getf chk :command) wt sha repo-path
                                                     to isolation-ok mem)))
                           (format t "~&Running check: ~A (~A)~%"
                                   (getf chk :name) (subseq sha 0 (min 7 (length sha))))
                           (handler-case
                               (multiple-value-bind (output error-output exit-code)
                                   (uiop:run-program
                                    argv
                                    :output '(:string :stripped t)
                                    :error-output '(:string :stripped t)
                                    :ignore-error-status t
                                    :directory (namestring wt))
                                 (cond
                                   ((zerop exit-code)
                                    (format t "  ✓ ~A passed~%" (getf chk :name)))
                                   ((= exit-code 124)
                                    (format t "  ✗ ~A timed out after ~As~%"
                                            (getf chk :name) to)
                                    (push (getf chk :name) failed))
                                   (t
                                    (format t "  ✗ ~A failed (exit ~A)~%"
                                            (getf chk :name) exit-code)
                                    (when (and output (plusp (length output)))
                                      (format t "    ~A~%" output))
                                    (when (and error-output (plusp (length error-output)))
                                      (format t "    ~A~%" error-output))
                                    (push (getf chk :name) failed))))
                             (error (e)
                               (format t "  ✗ ~A error: ~A~%" (getf chk :name) e)
                               (push (getf chk :name) failed))))))
                (ignore-errors
                 (uiop:run-program (list "rm" "-rf" wt) :ignore-error-status t)))))
          (disconnect-db)
          (when failed
            (format *error-output*
                    "~&cave: push rejected — ~A check~:P failed: ~{~A~^, ~}~%"
                    (length failed) (nreverse failed))
            (uiop:quit 1)))))))

;;; --- RUNNER subcommand ---

(defun %string-replace-all (string old new)
  "Replace every occurrence of OLD with NEW in STRING."
  (if (or (null old) (zerop (length old)))
      string
      (with-output-to-string (out)
        (loop with olen = (length old)
              for start = 0 then (+ pos olen)
              for pos = (search old string :start2 start)
              do (write-string string out :start start :end (or pos (length string)))
              while pos do (write-string new out)))))

(defun %mask-secrets (text values)
  "Replace each secret VALUE in TEXT with *** so secrets never reach the log UI."
  (let ((result text))
    (dolist (v values result)
      (when (and v (>= (length v) 4))   ; avoid masking trivially short values
        (setf result (%string-replace-all result v "***"))))))

(defun make-runner-command ()
  (clingon:make-command
   :name "runner"
   :description "Start a Cave runner agent"
   :options (list
             (clingon:make-option :string
              :long-name "url" :key :url :required t
              :description "Cave gRPC server URL (e.g. grpc://localhost:9443)")
             (clingon:make-option :string
              :long-name "token" :key :token
              :description "Registration token (or use --token-file)")
             (clingon:make-option :string
              :long-name "token-file" :key :token-file
              :description "Read the registration token from this file. Re-read on
                           each start, so an ExecStartPre that mints a fresh token
                           keeps a restarting runner working despite single-use tokens.")
             (clingon:make-option :string
              :long-name "name" :key :name
              :description "Runner name (default: hostname)")
             (clingon:make-option :string
              :long-name "labels" :key :labels
              :description "Comma-separated labels (e.g. linux,fast)")
             (clingon:make-option :boolean
              :long-name "ephemeral" :key :ephemeral
              :description "Exit after one task"))
   :handler #'handle-runner))

;;; --- RUNNER-TOKEN subcommand ---

(defun make-runner-token-command ()
  (clingon:make-command
   :name "runner-token"
   :description "Mint a single-use runner registration token (instance/org/repo/user scope)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "scope" :key :scope :initial-value "instance"
              :description "Token scope: instance, org, repo, or user")
             (clingon:make-option :string
              :long-name "org" :key :org
              :description "Org name (implies --scope org)")
             (clingon:make-option :string
              :long-name "repo" :key :repo
              :description "Repo as owner/name (implies --scope repo)")
             (clingon:make-option :string
              :long-name "user" :key :user
              :description "Username (implies --scope user)")
             (clingon:make-option :flag
              :long-name "quiet" :key :quiet
              :description "Print only the token, with no trailing detail"))
   :handler #'handle-runner-token
   :examples '(("Mint an instance-scoped token:" . "cave-server runner-token")
               ("Mint a repo-scoped token:" . "cave-server runner-token --repo atgreen/cave")
               ("For a quadlet ExecStartPre:" . "cave-server runner-token --quiet"))))

(defun handle-runner-token (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (scope (clingon:getopt cmd :scope))
        (org-name (clingon:getopt cmd :org))
        (repo-spec (clingon:getopt cmd :repo))
        (user-name (clingon:getopt cmd :user))
        (quiet (clingon:getopt cmd :quiet))
        ;; Keep the real stdout for the token alone. llog's console appender is a
        ;; synonym-stream to *standard-output*, so binding it to *error-output*
        ;; below routes all log/config noise to stderr — leaving stdout clean
        ;; enough to redirect straight into a --token-file.
        (real-out *standard-output*))
    (let ((*standard-output* *error-output*))
    (load-config config-path)
    (connect-db)
    (flet ((fail (fmt &rest args)
             ;; uiop:quit unwinds the stack, so the unwind-protect below still
             ;; closes the DB connection.
             (format *error-output* "~&~?~%" fmt args)
             (uiop:quit 1)))
      (unwind-protect
           (let ((scope-id :null))
             ;; A scope-bearing flag overrides --scope and resolves the id.
             (cond
               (org-name
                (let ((org (find-org-by-name org-name)))
                  (unless org (fail "No such org: ~A" org-name))
                  (setf scope "org" scope-id (getf org :id))))
               (repo-spec
                (let* ((slash (position #\/ repo-spec))
                       (owner (and slash (subseq repo-spec 0 slash)))
                       (rname (and slash (subseq repo-spec (1+ slash))))
                       (repo (and owner rname (find-repo owner rname))))
                  (unless repo (fail "No such repo: ~A (use owner/name)" repo-spec))
                  (setf scope "repo" scope-id (getf repo :id))))
               (user-name
                (let ((user (find-user-by-username user-name)))
                  (unless user (fail "No such user: ~A" user-name))
                  (setf scope "user" scope-id (getf user :id)))))
             (unless (member scope '("instance" "org" "repo" "user") :test #'string=)
               (fail "Invalid scope: ~A (instance, org, repo, or user)" scope))
             (let ((record (create-registration-token
                            :scope scope
                            :scope-id (unless (eq scope-id :null) scope-id))))
               (if quiet
                   (format real-out "~A~%" (getf record :token))
                   (format real-out "~&~A~%  scope: ~A~@[ #~A~]~%  expires: ~A~%"
                           (getf record :token) scope
                           (unless (eq scope-id :null) scope-id)
                           (getf record :expires-at)))))
        (disconnect-db))))))

(defun parse-grpc-url (url)
  "Parse grpc://host:port or grpcs://host[:port] into (host port tls-p).
   grpcs:// defaults to port 443; grpc:// defaults to port 9443."
  (let* ((tls-p (uiop:string-prefix-p "grpcs://" url))
         (stripped (cond
                     ((uiop:string-prefix-p "grpcs://" url) (subseq url 8))
                     ((uiop:string-prefix-p "grpc://" url) (subseq url 7))
                     (t url)))
         (colon (position #\: stripped))
         (host (if colon (subseq stripped 0 colon) stripped))
         (port (cond
                 (colon (parse-integer (subseq stripped (1+ colon)) :junk-allowed t))
                 (tls-p 443)
                 (t 9443))))
    (list host port tls-p)))

(defun handle-runner (cmd)
  (let* ((url (clingon:getopt cmd :url))
         (token-file (clingon:getopt cmd :token-file))
         (token (or (clingon:getopt cmd :token)
                    (and token-file
                         (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (uiop:read-file-string token-file)))))
         (name (or (clingon:getopt cmd :name)
                   (machine-instance)
                   "cave-runner"))
         (runner-labels (or (clingon:getopt cmd :labels) ""))
         (ephemeral (clingon:getopt cmd :ephemeral))
         (parsed (parse-grpc-url url))
         (host (first parsed))
         (port (second parsed))
         (tls-p (third parsed)))

    (when (or (null token) (zerop (length token)))
      (format *error-output*
              "~&No registration token. Pass --token <value> or --token-file <path>.~%")
      (uiop:quit 1))

    (format t "~&Cave Runner~%  Server: ~A:~A~A~%  Name: ~A~%  Labels: ~A~%  Ephemeral: ~A~%"
            host port (if tls-p " (TLS)" "") name runner-labels ephemeral)

    (labels
        ((make-auth-metadata (auth-token)
           (ag-grpc:alist-to-metadata `(("authorization" . ,(format nil "Bearer ~A" auth-token)))))

         (execute-task (channel auth-token task)
           "Execute a task — dispatch between simple automation and workflow job."
           (let ((job-id (handler-case (slot-value task 'cave::job-id) (error () 0))))
             (if (and job-id (plusp job-id))
                 (execute-workflow-job channel auth-token task)
                 (execute-simple-task channel auth-token task))))

         (execute-simple-task (channel auth-token task)
           "Execute a simple automation task (single command)."
           (let ((run-id (slot-value task 'cave::run-id))
                 (repo-owner (slot-value task 'cave::repo-owner))
                 (repo-name (slot-value task 'cave::repo-name))
                 (command (slot-value task 'cave::command)))
             (format t "~&Task #~A: ~A/~A — ~A~%" run-id repo-owner repo-name command)
             (ag-grpc:grpc-call channel
                                "/cave.runner.RunnerService/UpdateTaskStatus"
                                (make-instance 'cave::update-task-status-request
                                               :run-id run-id :status "running")
                                :response-type 'cave::update-task-status-response
                                :metadata (make-auth-metadata auth-token))
             (multiple-value-bind (output error-output exit-code)
                 (uiop:run-program (list "bash" "-c" command)
                                   :output '(:string :stripped t)
                                   :error-output '(:string :stripped t)
                                   :ignore-error-status t)
               (let ((log (format nil "~A~@[~%~A~]" (or output "") error-output)))
                 (ag-grpc:grpc-call channel
                                    "/cave.runner.RunnerService/AppendTaskLog"
                                    (make-instance 'cave::append-task-log-request
                                                   :run-id run-id :chunk log)
                                    :response-type 'cave::append-task-log-response
                                    :metadata (make-auth-metadata auth-token)))
               (let ((status (if (zerop exit-code) "success" "failure")))
                 (format t "  Result: ~A (exit ~A)~%" status exit-code)
                 (ag-grpc:grpc-call channel
                                    "/cave.runner.RunnerService/UpdateTaskStatus"
                                    (make-instance 'cave::update-task-status-request
                                                   :run-id run-id :status status)
                                    :response-type 'cave::update-task-status-response
                                    :metadata (make-auth-metadata auth-token))))))

         (execute-workflow-job (channel auth-token task)
           "Execute a workflow job: pull image, clone repo, run steps in container."
           (let* ((job-id (slot-value task 'cave::job-id))
                  (repo-owner (slot-value task 'cave::repo-owner))
                  (repo-name (slot-value task 'cave::repo-name))
                  (image (slot-value task 'cave::image))
                  (commit-sha (slot-value task 'cave::commit-sha))
                  (clone-url (slot-value task 'cave::clone-url))
                  (steps (slot-value task 'cave::steps))
                  (privileged (handler-case (slot-value task 'cave::privileged)
                                (error () nil)))
                  ;; CI secrets for this job: newline-joined KEY=VALUE, injected
                  ;; as container env and masked in streamed logs.
                  (secrets-env (handler-case (slot-value task 'cave::secrets-env)
                                 (error () "")))
                  (secret-pairs (when (and secrets-env (plusp (length secrets-env)))
                                  (remove-if #'uiop:emptyp
                                             (uiop:split-string secrets-env
                                                                :separator '(#\Newline)))))
                  (secret-values (loop for p in secret-pairs
                                       for eq = (position #\= p)
                                       when (and eq (< (1+ eq) (length p)))
                                       collect (subseq p (1+ eq))))
                  ;; Base dir for per-job working trees. In production this is a
                  ;; NATIVE (ext4/xfs) host volume mounted into the rootless
                  ;; cave-runner, NOT the container's fuse-overlayfs /tmp: an
                  ;; SBCL/Go build reading source from a fuse-overlayfs /workspace
                  ;; crawls and wedges the job (issue #9). Override the location
                  ;; with CAVE_RUNNER_WORKDIR.
                  (workdir-base (let ((e (uiop:getenv "CAVE_RUNNER_WORKDIR")))
                                  (if (and e (plusp (length e)))
                                      (string-right-trim "/" e)
                                      "/var/lib/cave-runner/work")))
                  (workdir (format nil "~A/cave-job-~A" workdir-base job-id))
                  (container-name (format nil "cave-job-~A" job-id))
                  ;; Persisted across jobs so Lisp builds reuse compiled FASLs
                  ;; instead of recompiling the whole ocicl tree every run (the
                  ;; recompile is what drives the memory spike that wedges the
                  ;; build — see issue #9). Harmless for non-Lisp jobs.
                  (fasl-cache "/var/cache/cave-runner/common-lisp")
                  (overall-success t))
             (format t "~&Workflow job #~A: ~A/~A [~A] (~A steps)~%"
                     job-id repo-owner repo-name image (length steps))
             ;; Report job running
             (ag-grpc:grpc-call channel
                                "/cave.runner.RunnerService/UpdateTaskStatus"
                                (make-instance 'cave::update-task-status-request
                                               :run-id 0
                                               :status (format nil "job:~A:running" job-id))
                                :response-type 'cave::update-task-status-response
                                :metadata (make-auth-metadata auth-token))
             (unwind-protect
              (progn
                ;; Ensure the (native-volume) workdir base exists before cloning.
                (ignore-errors
                 (ensure-directories-exist (concatenate 'string workdir-base "/")))
                ;; Pull image
                (format t "  Pulling ~A...~%" image)
                (multiple-value-bind (_out _err exit)
                    (uiop:run-program (list "podman" "pull" image)
                                      :output '(:string :stripped t)
                                      :error-output '(:string :stripped t)
                                      :ignore-error-status t)
                  (declare (ignore _out _err))
                  (unless (zerop exit)
                    (format *error-output* "  Failed to pull image ~A~%" image)
                    (setf overall-success nil)))
                ;; Clone repo
                (when overall-success
                  (format t "  Cloning ~A/~A...~%" repo-owner repo-name)
                  (multiple-value-bind (_out _err exit)
                      (uiop:run-program (list "git" "clone" "--depth" "1" clone-url workdir)
                                        :output '(:string :stripped t)
                                        :error-output '(:string :stripped t)
                                        :ignore-error-status t)
                    (declare (ignore _out _err))
                    (unless (zerop exit)
                      (format *error-output* "  Failed to clone repo~%")
                      (setf overall-success nil)))
                  ;; Checkout specific commit if provided
                  (when (and overall-success (not (uiop:emptyp commit-sha)))
                    (uiop:run-program (list "git" "-C" workdir "checkout" commit-sha)
                                      :output :string :error-output :string
                                      :ignore-error-status t)))
                ;; Create a long-lived container for all steps
                (when overall-success
                  (format t "  Creating container ~A...~%" container-name)
                    ;; Remove any leftover container from a previous run
                    (uiop:run-program (list "podman" "rm" "-f" container-name)
                                      :output :string :error-output :string
                                      :ignore-error-status t)
                    ;; Make sure the shared FASL cache dir exists before mounting.
                    (ignore-errors
                     (ensure-directories-exist (concatenate 'string fasl-cache "/")))
                    (multiple-value-bind (_out err exit)
                        (uiop:run-program
                         (append
                          (list "podman" "create" "--name" container-name)
                          ;; A job opts in to privileged mode with `privileged:
                          ;; true` in its workflow YAML.  Privileged jobs can
                          ;; launch nested containers (podman/docker-in-podman)
                          ;; and register QEMU binfmt for foreign-arch testing.
                          ;; Off by default so untrusted repos can't escalate.
                          (when privileged (list "--privileged"))
                          ;; Inject CI secrets as environment variables.
                          (loop for p in secret-pairs append (list "-e" p))
                          (list
                           ;; :Z relabels the bind mount with a private SELinux
                           ;; label so the container can write to it on
                           ;; enforcing hosts (Fedora/RHEL).
                           "-v" (format nil "~A:/workspace:Z" workdir)
                           ;; Shared (:z) so successive job containers reuse the
                           ;; FASL cache without relabeling the whole (growing)
                           ;; tree each run.
                           "-v" (format nil "~A:/root/.cache/common-lisp:z" fasl-cache)
                           "-w" "/workspace"
                           image "sleep" "infinity"))
                         :output '(:string :stripped t)
                         :error-output '(:string :stripped t)
                         :ignore-error-status t)
                      (declare (ignore _out))
                      (unless (zerop exit)
                        (format *error-output* "  Failed to create container: ~A~%" err)
                        (setf overall-success nil)))
                    (when overall-success
                      (uiop:run-program (list "podman" "start" container-name)
                                        :output :string :error-output :string
                                        :ignore-error-status t))
                    ;; Run each step in the same container
                    (when overall-success
                      (dolist (step steps)
                        (let ((step-id (slot-value step 'cave::step-id))
                              (step-name (slot-value step 'cave::name))
                              (step-cmd (slot-value step 'cave::command))
                              (step-timeout (let ((ts (slot-value step 'cave::timeout-seconds)))
                                              (when (and ts (plusp ts)) ts)))
                              (step-continue-on-error (slot-value step 'cave::continue-on-error)))
                          (format t "  Step ~A: ~A~%"
                                  (if (uiop:emptyp step-name) "(unnamed)" step-name)
                                  (subseq step-cmd 0 (min 60 (length step-cmd))))
                          (ag-grpc:grpc-call channel
                                             "/cave.runner.RunnerService/UpdateStepStatus"
                                             (make-instance 'cave::update-step-status-request
                                                            :step-id step-id :status "running"
                                                            :exit-code 0)
                                             :response-type 'cave::update-step-status-response
                                             :metadata (make-auth-metadata auth-token))
                          ;; Execute step in container, streaming output
                          (let ((exit-code
                                  (block step-run
                                    (handler-case
                                      (let* ((log-file (format nil "/tmp/cave-step-~A.log" step-id))
                                             (shell-cmd
                                               (format nil "podman exec ~A bash -c ~A >~A 2>&1"
                                                       container-name
                                                       (uiop:escape-sh-token step-cmd)
                                                       log-file))
                                             (process (uiop:launch-program shell-cmd :force-shell t))
                                             (sent 0))
                                        ;; Poll log file and send full content to server
                                        (flet ((send-log ()
                                                 (handler-case
                                                     (when (probe-file log-file)
                                                       (let* ((content (uiop:read-file-string log-file))
                                                              (len (length content)))
                                                         (when (> len sent)
                                                           ;; Send only new content since last send (max 64KB per call)
                                                           (let* ((new-start sent)
                                                                  (raw (subseq content new-start
                                                                               (min len (+ new-start 65536))))
                                                                  (chunk (%mask-secrets raw secret-values)))
                                                             (setf sent (+ new-start (length raw)))
                                                             (ag-grpc:grpc-call channel
                                                              "/cave.runner.RunnerService/AppendStepLog"
                                                              (make-instance 'cave::append-step-log-request
                                                                             :step-id step-id :chunk chunk)
                                                              :response-type 'cave::append-step-log-response
                                                              :metadata (make-auth-metadata auth-token))))))
                                                   (error (e)
                                                     (format *error-output* "  Log send error: ~A~%" e)))))

                                          (let ((deadline (when step-timeout
                                                          (+ (get-universal-time) step-timeout))))
                                            (loop while (uiop:process-alive-p process)
                                                  do (when (and deadline (>= (get-universal-time) deadline))
                                                       (format *error-output* "    Step timed out after ~As~%" step-timeout)
                                                       (uiop:terminate-process process :urgent t)
                                                       (uiop:wait-process process)
                                                       (send-log)
                                                       (ignore-errors (delete-file log-file))
                                                       (return-from step-run 124))
                                                     (send-log) (sleep 2))
                                            (send-log)
                                            (ignore-errors (delete-file log-file))
                                            (uiop:wait-process process))))
                                      (error () 1)))))
                            (let ((step-status (if (zerop exit-code) "success" "failure")))
                              (format t "    ~A (exit ~A)~%" step-status exit-code)
                              (ag-grpc:grpc-call channel
                                                 "/cave.runner.RunnerService/UpdateStepStatus"
                                                 (make-instance 'cave::update-step-status-request
                                                                :step-id step-id :status step-status
                                                                :exit-code exit-code)
                                                 :response-type 'cave::update-step-status-response
                                                 :metadata (make-auth-metadata auth-token))
                              (unless (zerop exit-code)
                                (if step-continue-on-error
                                    (format t "    continue-on-error: proceeding despite failure~%")
                                    (progn
                                      (setf overall-success nil)
                                      (return)))))))))) ;; close unless,let,mvb,let,dolist,when(steps)
                    ;; Stop and remove container
                    (uiop:run-program (list "podman" "rm" "-f" container-name)
                                      :output :string :error-output :string
                                      :ignore-error-status t)) ;; close when(outer)
              ;; Cleanup workdir
              (uiop:run-program (list "rm" "-rf" workdir)
                                :ignore-error-status t))
             ;; Report overall job status
             (let ((status (if overall-success "success" "failure")))
               (format t "  Job result: ~A~%" status)
               (ag-grpc:grpc-call channel
                                  "/cave.runner.RunnerService/UpdateTaskStatus"
                                  (make-instance 'cave::update-task-status-request
                                                 :run-id 0
                                                 :status (format nil "job:~A:~A" job-id status))
                                  :response-type 'cave::update-task-status-response
                                  :metadata (make-auth-metadata auth-token)))))

         (run-watch-loop (channel auth-token)
           "Open a WatchTasks server stream and process tasks as they arrive."
           (format t "~&Watching for tasks...~%")
           (let ((stream (ag-grpc:call-server-stream
                          channel
                          "/cave.runner.RunnerService/WatchTasks"
                          (make-instance 'cave::watch-tasks-request
                                         :runner-labels runner-labels)
                          :response-type 'cave::task-event
                          :metadata (make-auth-metadata auth-token))))
             (loop
               (let ((task (ag-grpc:stream-receive-message stream)))
                 (unless task (return)) ; stream ended
                 (handler-case
                     (progn
                       (execute-task channel auth-token task)
                       (when ephemeral
                         (format t "~&Ephemeral runner — exiting.~%")
                         (uiop:quit 0)))
                   (error (e)
                     (format *error-output* "~&Task execution error: ~A~%" e))))))))

      ;; Register with the server (no timeout — builds can take minutes)
      (let ((channel (ag-grpc:make-channel host port :timeout nil :tls tls-p)))
        (handler-case
            (progn
              (format t "~&Registering...~%")
              (let* ((req (make-instance 'cave::register-runner-request
                                         :name name
                                         :runner-labels runner-labels
                                         :ephemeral ephemeral
                                         :registration-token token))
                     (resp (ag-grpc:grpc-call channel
                                              "/cave.runner.RunnerService/RegisterRunner"
                                              req :response-type 'cave::register-runner-response)))
                (let ((auth-token (slot-value resp 'cave::auth-token))
                      (runner-id (slot-value resp 'cave::runner-id)))
                  (format t "  Registered as runner #~A~%" runner-id)
                  ;; Main loop: watch for tasks with auto-reconnect
                  (loop
                    (handler-case
                        (let ((ch (ag-grpc:make-channel host port
                                    :timeout nil
                                    :tls tls-p
                                    :keepalive (ag-grpc:make-keepalive-config
                                                :ping-interval 15
                                                :ping-timeout 5
                                                :permit-without-calls t))))
                          (run-watch-loop ch auth-token))
                      (error (e)
                        (format *error-output* "~&Stream disconnected: ~A~%" e)
                        (format *error-output* "  Reconnecting in 5s...~%")
                        (sleep 5)))))))
          (error (e)
            (format *error-output* "~&Runner error: ~A~%" e)
            (uiop:quit 1)))))))

;;; --- POST-RECEIVE subcommand ---

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
