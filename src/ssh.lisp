;;; ssh.lisp — SSH git transport via system sshd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Cave manages ~/.ssh/authorized_keys for a dedicated Unix user.
;;; Each SSH key registered in the web UI gets an entry like:
;;;
;;;   command="cave git-shell --config /etc/cave.conf --key-id 7",\
;;;   no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty \
;;;   ssh-ed25519 AAAA... user@laptop
;;;
;;; When sshd accepts a connection, it execs cave git-shell which:
;;;   1. Reads SSH_ORIGINAL_COMMAND (what git wanted to run)
;;;   2. Parses the repo path from it
;;;   3. Looks up key-id → user → permissions
;;;   4. Execs git-upload-pack or git-receive-pack with the real repo path

(in-package #:cave)

;;; ========================== GIT SHELL ==========================

(defun parse-ssh-command (ssh-original-command)
  "Parse SSH_ORIGINAL_COMMAND from git. Returns (VALUES command repo-path) or NIL.
   Git sends: git-upload-pack '/org/repo.git' or git-receive-pack '/org/repo.git'"
  (when (and ssh-original-command (plusp (length ssh-original-command)))
    (let* ((parts (uiop:split-string ssh-original-command :separator " "))
           (command (first parts))
           (raw-path (second parts)))
      (when (and command raw-path
                 (member command '("git-upload-pack" "git-receive-pack")
                         :test #'equal))
        ;; Strip surrounding quotes from path
        (let ((path (string-trim "'" raw-path)))
          (values command path))))))

(defun parse-repo-from-path (path)
  "Parse org-name and repo-name from a git path like '/org/repo.git'.
   Returns (VALUES org-name repo-name) or NIL."
  (let* ((clean (string-left-trim "/" path))
         ;; Strip .git suffix
         (clean (if (uiop:string-suffix-p clean ".git")
                    (subseq clean 0 (- (length clean) 4))
                    clean))
         (slash-pos (position #\/ clean)))
    (when slash-pos
      (values (subseq clean 0 slash-pos)
              (subseq clean (1+ slash-pos))))))

(defun handle-git-shell (cmd)
  "Handle the git-shell subcommand. Called by sshd via authorized_keys command=."
  (let ((config-path (clingon:getopt cmd :config))
        (key-id (clingon:getopt cmd :key-id)))

    (load-config config-path)

    ;; Connect to DB
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "cave: database error: ~A~%" e)
        (uiop:quit 1)))

    ;; Read SSH_ORIGINAL_COMMAND
    (let ((ssh-cmd (uiop:getenv "SSH_ORIGINAL_COMMAND")))
      (unless ssh-cmd
        (format *error-output* "cave: interactive shell access is not supported~%")
        (disconnect-db)
        (uiop:quit 1))

      ;; Parse the git command
      (multiple-value-bind (git-command repo-path) (parse-ssh-command ssh-cmd)
        (unless git-command
          (format *error-output* "cave: invalid git command~%")
          (disconnect-db)
          (uiop:quit 1))

        ;; Parse org/repo from path
        (multiple-value-bind (org-name repo-name) (parse-repo-from-path repo-path)
          (unless (and org-name repo-name)
            (format *error-output* "cave: invalid repository path~%")
            (disconnect-db)
            (uiop:quit 1))

          ;; Look up key → user
          (let ((key-record (find-ssh-key-by-id key-id)))
            (unless key-record
              (format *error-output* "cave: invalid key~%")
              (disconnect-db)
              (uiop:quit 1))

            (let* ((user-id (getf key-record :user-id))
                   (user (find-user-by-id user-id)))
              (unless (and user (getf user :is-active))
                (format *error-output* "cave: account disabled~%")
                (disconnect-db)
                (uiop:quit 1))

              ;; Look up repo
              (let ((repo (find-repo org-name repo-name)))
                (unless repo
                  ;; Don't leak whether private repos exist
                  (format *error-output* "cave: repository not found~%")
                  (disconnect-db)
                  (uiop:quit 1))

                ;; Check permissions
                (let ((role (repo-member-role (getf repo :id) user-id))
                      (is-push (equal git-command "git-receive-pack")))

                  ;; For private repos, any access requires membership
                  (when (and (getf repo :is-private) (not role))
                    (format *error-output* "cave: repository not found~%")
                    (disconnect-db)
                    (uiop:quit 1))

                  ;; For push, need at least writer role
                  (when (and is-push (not role))
                    (format *error-output* "cave: permission denied~%")
                    (disconnect-db)
                    (uiop:quit 1))

                  ;; Log the access
                  (log-event (if is-push "git.push" "git.clone")
                             :user-id user-id
                             :repo-id (getf repo :id))

                  (disconnect-db)

                  ;; Exec the git command with the real on-disk repo path
                  (let ((disk-path (namestring (repo-disk-path org-name repo-name))))
                    (uiop:run-program (list git-command disk-path)
                                      :input :interactive
                                      :output :interactive
                                      :error-output :interactive)
                    (uiop:quit 0)))))))))))

;;; ========================== AUTHORIZED KEYS ==========================

(defun authorized-keys-line (key-record config-path cave-binary)
  "Generate an authorized_keys line for a single SSH key record."
  (format nil "command=\"~A git-shell --config ~A --key-id ~A\",~
               no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ~A"
          cave-binary config-path (getf key-record :id) (getf key-record :public-key)))

(defun generate-authorized-keys (config-path cave-binary)
  "Generate the full authorized_keys file content from all active SSH keys."
  (let ((keys (all-active-ssh-keys)))
    (with-output-to-string (s)
      (format s "# Managed by Cave — do not edit manually~%")
      (dolist (key keys)
        (format s "~A~%" (authorized-keys-line key config-path cave-binary))))))

(defun write-authorized-keys (path config-path cave-binary)
  "Write the authorized_keys file at PATH."
  (let ((content (generate-authorized-keys config-path cave-binary)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content s))
    ;; Correct permissions — sshd requires 600
    (uiop:run-program (list "chmod" "600" (namestring path))
                       :ignore-error-status t)
    (llog:info "Updated authorized_keys" :path path
               :key-count (length (all-active-ssh-keys)))))

(defun sync-authorized-keys ()
  "Regenerate authorized_keys if a keys-path is configured. Safe to call anytime."
  (let ((keys-path (config-value :authorized-keys-path))
        (cave-binary (config-value :cave-binary "/usr/bin/cave"))
        (config-path *config-path*))
    (when keys-path
      (handler-case
          (write-authorized-keys keys-path
                                 (namestring config-path)
                                 cave-binary)
        (error (e)
          (llog:error "Failed to sync authorized_keys" :error (format nil "~A" e)))))))

(defun handle-update-keys (cmd)
  "Handle the update-keys subcommand."
  (let ((config-path (clingon:getopt cmd :config))
        (keys-path (clingon:getopt cmd :output))
        (cave-binary (clingon:getopt cmd :cave-binary)))

    (load-config config-path)
    (connect-db)
    (write-authorized-keys keys-path config-path cave-binary)
    (disconnect-db)
    (format t "~&authorized_keys updated.~%")))
