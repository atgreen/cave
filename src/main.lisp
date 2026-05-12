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

    ;; Ensure cave org and cave-themes repo exist
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
    (let ((cave-org (find-org-by-name "cave")))
      (when (and cave-org (not (find-repo "cave" "cave-themes")))
        (handler-case
            (let ((repo (postmodern:query
                         (:insert-into 'cave-repos
                          :set 'org-id (getf cave-org :id)
                               'name "cave-themes"
                               'description "Built-in and community themes for Cave"
                          :returning '*)
                         :plist)))
              (init-bare-repo "cave" "cave-themes")
              ;; Seed with example theme files
              (let ((disk-path (repo-disk-path "cave" "cave-themes"))
                    (seed-dir (merge-pathnames "keycloak/cave-themes-seed/" (uiop:getcwd)))
                    (tmpdir (format nil "/tmp/cave-themes-seed-~A"
                                    (ironclad:byte-array-to-hex-string
                                     (ironclad:random-data 4)))))
                (when (probe-file seed-dir)
                  (handler-case
                      (progn
                        (uiop:run-program (list "git" "clone" (namestring disk-path) tmpdir)
                                          :output :string :error-output :string)
                        (uiop:run-program (format nil "cp ~A* ~A/" (namestring seed-dir) tmpdir)
                                          :output :string :error-output :string
                                          :force-shell t)
                        (uiop:run-program (list "git" "-C" tmpdir "add" "-A")
                                          :output :string :error-output :string)
                        (uiop:run-program (list "git" "-C" tmpdir
                                                "-c" "user.name=Cave"
                                                "-c" "user.email=cave@localhost"
                                                "commit" "-m" "Add example theme and documentation")
                                          :output :string :error-output :string)
                        (uiop:run-program (list "git" "-C" tmpdir "push" "origin" "main")
                                          :output :string :error-output :string)
                        (llog:info "Seeded cave/cave-themes with example theme"))
                    (error (e)
                      (llog:warn "Failed to seed cave-themes" :error (princ-to-string e))))
                  (uiop:run-program (list "rm" "-rf" tmpdir)
                                    :ignore-error-status t)))
              (llog:info "Created cave/cave-themes repo" :id (getf repo :id)))
          (error () nil))))

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
        (llog:info "Cave listening" :version +version+ :port port)

        ;; Wait forever
        (bt2:condition-wait *shutdown-cv* *server-lock*)))))

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
      ;; Connect to Chamber
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
                       (config-value :chamber-port 9444)))
             (channel (ag-grpc:make-channel host-only port :timeout nil))
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

;;; --- RUNNER subcommand ---

(defun make-runner-command ()
  (clingon:make-command
   :name "runner"
   :description "Start a Cave runner agent"
   :options (list
             (clingon:make-option :string
              :long-name "url" :key :url :required t
              :description "Cave gRPC server URL (e.g. grpc://localhost:9443)")
             (clingon:make-option :string
              :long-name "token" :key :token :required t
              :description "Registration token")
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

(defun parse-grpc-url (url)
  "Parse grpc://host:port into (host port)."
  (let* ((stripped (if (search "grpc://" url)
                       (subseq url 7)
                       url))
         (colon (position #\: stripped)))
    (if colon
        (list (subseq stripped 0 colon)
              (parse-integer (subseq stripped (1+ colon)) :junk-allowed t))
        (list stripped 9443))))

(defun handle-runner (cmd)
  (let* ((url (clingon:getopt cmd :url))
         (token (clingon:getopt cmd :token))
         (name (or (clingon:getopt cmd :name)
                   (machine-instance)
                   "cave-runner"))
         (runner-labels (or (clingon:getopt cmd :labels) ""))
         (ephemeral (clingon:getopt cmd :ephemeral))
         (parsed (parse-grpc-url url))
         (host (first parsed))
         (port (second parsed)))

    (format t "~&Cave Runner~%  Server: ~A:~A~%  Name: ~A~%  Labels: ~A~%  Ephemeral: ~A~%"
            host port name runner-labels ephemeral)

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
                  (workdir (format nil "/tmp/cave-job-~A" job-id))
                  (container-name (format nil "cave-job-~A" job-id))
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
                    (multiple-value-bind (_out err exit)
                        (uiop:run-program
                         (list "podman" "create" "--name" container-name
                               "-v" (format nil "~A:/workspace" workdir)
                               "-w" "/workspace"
                               image "sleep" "infinity")
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
                                                                  (chunk (subseq content new-start
                                                                                 (min len (+ new-start 65536)))))
                                                             (setf sent (+ new-start (length chunk)))
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
      (let ((channel (ag-grpc:make-channel host port :timeout nil)))
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
                       (make-git-proxy-command)
                       (make-update-keys-command)
                       (make-run-checks-command)
                       (make-post-receive-command)
                       (make-runner-command)
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
