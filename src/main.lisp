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
   :description "Initialize the database and create the admin user"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "admin-user" :key :admin-user :required t
              :description "Admin username")
             (clingon:make-option :string
              :long-name "admin-password" :key :admin-password :required t
              :description "Admin password")
             (clingon:make-option :filepath
              :long-name "admin-ssh-key" :key :admin-ssh-key
              :description "Path to admin's SSH public key file"))
   :handler #'handle-init
   :examples '(("Initialize Cave:" .
                "cave init --admin-user admin --admin-password secret"))))

(defun handle-init (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (admin-user (clingon:getopt cmd :admin-user))
        (admin-pass (clingon:getopt cmd :admin-password))
        (ssh-key-path (clingon:getopt cmd :admin-ssh-key)))

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

    ;; Create or update admin user
    (let ((existing (find-user-by-username admin-user)))
      (if existing
          (llog:info "Admin user already exists" :user admin-user :id (getf existing :id))
          (let ((user (create-user :username admin-user
                                   :password admin-pass
                                   :is-admin t)))
            (llog:info "Created admin user" :user admin-user :id (getf user :id))

            ;; Add SSH key if provided
            (when ssh-key-path
              (let ((key-data (uiop:read-file-string ssh-key-path)))
                (add-ssh-key (getf user :id) "admin-key" (string-trim '(#\Newline #\Return) key-data))
                (llog:info "Added SSH key" :path ssh-key-path))))))

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
              :long-name "cave-binary" :key :cave-binary
              :description "Path to the cave binary"
              :initial-value "/usr/bin/cave"))
   :handler #'handle-update-keys
   :examples '(("Update authorized_keys:" .
                "cave update-keys --config /etc/cave.conf"))))

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
                       (make-update-keys-command))
   :handler (lambda (cmd)
              (clingon:print-usage cmd *standard-output*))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      (uiop:quit 1))))
