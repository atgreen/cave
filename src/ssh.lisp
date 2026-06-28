;;; ssh.lisp — SSH git transport via system sshd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Architecture:
;;;   1. authorized_keys has: command="cave-shell.sh /etc/cave.conf 7" ...
;;;   2. cave-shell.sh reads SSH_ORIGINAL_COMMAND, calls cave-server git-shell for auth
;;;   3. cave-server git-shell checks key->user->permissions, prints two lines to stdout:
;;;      user-id, then repo disk path
;;;   4. cave-shell.sh exports CAVE_PUSH_USER_ID and execs git-upload-pack or
;;;      git-receive-pack — the post-receive hook then forwards the actor id to
;;;      the internal post-receive HTTP endpoint, which is where rich git.push
;;;      events are recorded

(in-package #:cave)

;;; ========================== GIT SHELL ==========================

(defun parse-ssh-command (ssh-original-command)
  "Parse SSH_ORIGINAL_COMMAND from git. Returns (VALUES command repo-path) or NIL."
  (when (and ssh-original-command (plusp (length ssh-original-command)))
    (let* ((parts (uiop:split-string ssh-original-command :separator " "))
           (command (first parts))
           (raw-path (second parts)))
      (when (and command raw-path
                 (member command '("git-upload-pack" "git-receive-pack")
                         :test #'equal))
        (let ((path (string-trim "'" raw-path)))
          (values command path))))))

(defun parse-repo-from-path (path)
  "Parse owner and repo-name from a git path like '/owner/repo.git'."
  (let* ((clean (string-left-trim "/" path))
         (clean (if (uiop:string-suffix-p clean ".git")
                    (subseq clean 0 (- (length clean) 4))
                    clean))
         (slash-pos (position #\/ clean)))
    (when slash-pos
      (values (subseq clean 0 slash-pos)
              (subseq clean (1+ slash-pos))))))

(defun git-shell-fail (message)
  "Print error to stderr and exit."
  (format *error-output* "cave: ~A~%" message)
  (disconnect-db)
  (uiop:quit 1))

(defun handle-git-shell (cmd)
  "Authenticate an SSH git operation. Prints the on-disk repo path to stdout.
   All log output goes to stderr so it doesn't corrupt the path."
  (let ((config-path (clingon:getopt cmd :config))
        (key-id (clingon:getopt cmd :key-id))
        ;; Save real stdout. Redirect llog and *standard-output* to stderr
        ;; so log output doesn't corrupt the repo path we print.
        (saved-stdout *standard-output*))
    (setf *standard-output* *error-output*)
    ;; Reconfigure llog to write to stderr (it captured stdout at init time)
    (let ((root (llog:root-logger)))
      (llog:remove-output root (first (llog::logger-outputs root)))
      (llog:add-output root (llog:make-stream-output *error-output*)))

    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "cave: database error: ~A~%" e)
        (uiop:quit 1)))

    (let ((ssh-cmd (uiop:getenv "SSH_ORIGINAL_COMMAND")))
      (unless ssh-cmd (git-shell-fail "interactive shell access is not supported"))

      (multiple-value-bind (git-command repo-path) (parse-ssh-command ssh-cmd)
        (unless git-command (git-shell-fail "invalid git command"))

        (multiple-value-bind (owner-name repo-name) (parse-repo-from-path repo-path)
          (unless (and owner-name repo-name) (git-shell-fail "invalid repository path"))

          (if (and (stringp key-id) (uiop:string-prefix-p "d" key-id))
              ;; --- Deploy key: scoped to one repo; clone always, push iff RW ---
              (let* ((deploy-id (parse-integer (subseq key-id 1) :junk-allowed t))
                     (dk (and deploy-id (find-deploy-key-by-id deploy-id)))
                     (repo (find-repo owner-name repo-name))
                     (is-push (equal git-command "git-receive-pack")))
                (unless dk (git-shell-fail "invalid key"))
                (unless repo (git-shell-fail "repository not found"))
                (unless (eql (getf dk :repo-id) (getf repo :id))
                  (git-shell-fail "repository not found"))
                (when (and is-push (not (getf dk :read-write)))
                  (git-shell-fail "permission denied (read-only deploy key)"))
                (unless is-push (log-event "git.clone" :repo-id (getf repo :id)))
                (disconnect-db)
                (format saved-stdout "~D~%~A~%"
                        0 (namestring (repo-disk-path owner-name repo-name)))
                (finish-output saved-stdout)
                (uiop:quit 0))
              ;; --- User key (the common path) ---
              (let* ((kid (parse-integer (princ-to-string key-id) :junk-allowed t))
                     (key-record (and kid (find-ssh-key-by-id kid))))
                (unless key-record (git-shell-fail "invalid key"))
                (let* ((user-id (getf key-record :user-id))
                       (user (find-user-by-id user-id)))
                  (unless (and user (getf user :is-active))
                    (git-shell-fail "account disabled"))
                  (let ((repo (find-repo owner-name repo-name)))
                    (unless repo (git-shell-fail "repository not found"))
                    (let ((role (repo-member-role (getf repo :id) user-id))
                          (is-push (equal git-command "git-receive-pack")))
                      (when (and (getf repo :is-private) (not role))
                        (git-shell-fail "repository not found"))
                      (when (and is-push (not role))
                        (git-shell-fail "permission denied"))
                      (unless is-push
                        (log-event "git.clone"
                                   :user-id user-id
                                   :repo-id (getf repo :id)))
                      (disconnect-db)
                      (format saved-stdout "~D~%~A~%"
                              user-id
                              (namestring (repo-disk-path owner-name repo-name)))
                      (finish-output saved-stdout)
                      (uiop:quit 0)))))))))))

;;; ========================== AUTHORIZED KEYS ==========================

(defun authorized-keys-line (key-record config-path shell-path)
  "Generate an authorized_keys line for a single SSH key record."
  (format nil "command=\"~A ~A ~A ~A\",~
               no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ~A"
          shell-path config-path (getf key-record :id)
          (config-value :http-port 8080) (getf key-record :public-key)))

(defun deploy-authorized-keys-line (dk config-path shell-path)
  "authorized_keys line for a deploy key. The key-id is `d<id>` so git-shell
routes it to the deploy-key (repo-scoped) auth path."
  (format nil "command=\"~A ~A d~A ~A\",~
               no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ~A"
          shell-path config-path (getf dk :id)
          (config-value :http-port 8080) (getf dk :public-key)))

(defun generate-authorized-keys (config-path shell-path)
  "Generate authorized_keys content from all active user SSH keys + deploy keys."
  (with-output-to-string (s)
    (format s "# Managed by Cave — do not edit manually~%")
    (dolist (key (all-active-ssh-keys))
      (format s "~A~%" (authorized-keys-line key config-path shell-path)))
    (dolist (dk (all-deploy-keys-with-repo))
      (format s "~A~%" (deploy-authorized-keys-line dk config-path shell-path)))))

(defun write-authorized-keys (path config-path shell-path)
  "Write the authorized_keys file at PATH."
  (let ((content (generate-authorized-keys config-path shell-path)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content s))
    (uiop:run-program (list "chmod" "600" (namestring path))
                       :ignore-error-status t)
    (llog:info "Updated authorized_keys" :path path
               :key-count (length (all-active-ssh-keys)))))

(defun sync-authorized-keys ()
  "Regenerate authorized_keys if configured. Safe to call anytime."
  (let ((keys-path (config-value :authorized-keys-path))
        (shell-path (config-value :cave-shell "/usr/bin/cave-shell.sh"))
        (config-path *config-path*))
    (when keys-path
      (handler-case
          (write-authorized-keys keys-path (namestring config-path) shell-path)
        (error (e)
          (llog:error "Failed to sync authorized_keys" :error (format nil "~A" e)))))))

(defun handle-update-keys (cmd)
  "Handle the update-keys subcommand."
  (let ((config-path (clingon:getopt cmd :config))
        (keys-path (clingon:getopt cmd :output))
        (shell-path (clingon:getopt cmd :cave-shell)))
    (load-config config-path)
    (connect-db)
    (write-authorized-keys keys-path config-path shell-path)
    (disconnect-db)
    (format t "~&authorized_keys updated.~%")))
