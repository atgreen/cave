;;; notify.lisp — Email notifications
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

(defun smtp-configured-p ()
  "Check if SMTP is configured."
  (and (config-value :smtp-host)
       (config-value :smtp-from)))

(defun send-email (to subject body)
  "Send an email. Silently fails if SMTP is not configured."
  (when (smtp-configured-p)
    (handler-case
        (cl-smtp:send-email
         (config-value :smtp-host)
         (config-value :smtp-from)
         to
         subject
         body
         :port (config-value :smtp-port 587)
         :authentication (when (config-value :smtp-user)
                           (list :login
                                 (config-value :smtp-user)
                                 (config-value :smtp-password)))
         :ssl (config-value :smtp-ssl))
      (error (e)
        (llog:warn "Failed to send email" :to to :error (princ-to-string e))))))

(defun notify-repo-participants (repo-id subject body &key exclude-user-id)
  "Send an email to all repo members (except exclude-user-id)."
  (let ((members (list-repo-members repo-id)))
    (dolist (m members)
      (unless (and exclude-user-id (= (getf m :user-id) exclude-user-id))
        (let ((user (find-user-by-id (getf m :user-id))))
          (when (and user (getf user :email))
            (send-email (getf user :email) subject body)))))))

(defun notify-issue-created (repo owner-name repo-name issue)
  "Notify repo members about a new issue."
  (let ((subject (format nil "[~A/~A] New issue #~A: ~A"
                         owner-name repo-name
                         (getf issue :number) (getf issue :title)))
        (body (format nil "~A opened issue #~A: ~A~%~%~A~%~%~A/~A/~A/issues/~A"
                      (when *current-user* (getf *current-user* :username))
                      (getf issue :number) (getf issue :title)
                      (or (getf issue :body) "")
                      (config-value :base-url) owner-name repo-name
                      (getf issue :number))))
    (notify-repo-participants (getf repo :id) subject body
                              :exclude-user-id *current-user-id*)))

(defun notify-issue-comment (repo owner-name repo-name issue comment-body)
  "Notify about a new issue comment."
  (let ((subject (format nil "[~A/~A] Comment on #~A: ~A"
                         owner-name repo-name
                         (getf issue :number) (getf issue :title)))
        (body (format nil "~A commented on issue #~A:~%~%~A~%~%~A/~A/~A/issues/~A"
                      (when *current-user* (getf *current-user* :username))
                      (getf issue :number) comment-body
                      (config-value :base-url) owner-name repo-name
                      (getf issue :number))))
    (notify-repo-participants (getf repo :id) subject body
                              :exclude-user-id *current-user-id*)))

(defun notify-pr-review (repo owner-name repo-name pr state)
  "Notify about a PR review."
  (let ((subject (format nil "[~A/~A] Review on PR #~A: ~A"
                         owner-name repo-name
                         (getf pr :number) state))
        (body (format nil "~A reviewed PR #~A (~A → ~A): ~A~%~%~A/~A/~A/pulls/~A"
                      (when *current-user* (getf *current-user* :username))
                      (getf pr :number)
                      (getf pr :source-branch) (getf pr :target-branch)
                      state
                      (config-value :base-url) owner-name repo-name
                      (getf pr :number))))
    (notify-repo-participants (getf repo :id) subject body
                              :exclude-user-id *current-user-id*)))

(defun notify-pr-merged (repo owner-name repo-name pr)
  "Notify about a PR merge."
  (let ((subject (format nil "[~A/~A] PR #~A merged: ~A → ~A"
                         owner-name repo-name
                         (getf pr :number)
                         (getf pr :source-branch) (getf pr :target-branch)))
        (body (format nil "~A merged PR #~A (~A → ~A)~%~%~A/~A/~A/pulls/~A"
                      (when *current-user* (getf *current-user* :username))
                      (getf pr :number)
                      (getf pr :source-branch) (getf pr :target-branch)
                      (config-value :base-url) owner-name repo-name
                      (getf pr :number))))
    (notify-repo-participants (getf repo :id) subject body
                              :exclude-user-id *current-user-id*)))
