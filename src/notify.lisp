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

;;; --- Webhooks ---

(defun fire-webhooks (repo-id event payload)
  "Fire all enabled webhooks for REPO-ID that subscribe to EVENT.
   PAYLOAD is a hash-table that will be JSON-encoded."
  (let ((hooks (list-repo-webhooks-for-event repo-id event)))
    (dolist (hook hooks)
      (handler-case
          (let* ((json-body (com.inuoe.jzon:stringify payload))
                 (headers `(("Content-Type" . "application/json")
                            ("X-Cave-Event" . ,event)))
                 ;; HMAC signature if secret is set
                 (secret (getf hook :secret)))
            (when (and secret (not (eq secret :null)))
              (let ((sig (ironclad:byte-array-to-hex-string
                          (ironclad:produce-mac
                           (let ((mac (ironclad:make-mac :hmac
                                       (flexi-streams:string-to-octets secret :external-format :utf-8)
                                       :sha256)))
                             (ironclad:update-mac mac
                              (flexi-streams:string-to-octets json-body :external-format :utf-8))
                             mac)))))
                (push (cons "X-Cave-Signature" (format nil "sha256=~A" sig)) headers)))
            (multiple-value-bind (body status)
                (dex:post (getf hook :url)
                          :content json-body
                          :headers headers
                          :connect-timeout 10
                          :read-timeout 30)
              (declare (ignore body))
              (update-webhook-status (getf hook :id) status)))
        (error (e)
          (update-webhook-status (getf hook :id) 0 (princ-to-string e))
          (llog:warn "Webhook delivery failed" :url (getf hook :url)
                     :error (princ-to-string e)))))))

(defun make-webhook-payload (event &rest pairs)
  "Build a webhook payload hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "event" ht) event)
    (setf (gethash "timestamp" ht) (princ-to-string (get-universal-time)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    ht))

;;; --- Automation scheduling ---

(defun schedule-automations (repo-id trigger &key commit-sha ref triggered-by-id)
  "Schedule automation runs for all enabled definitions matching TRIGGER."
  (let ((defs (list-automation-definitions repo-id :trigger trigger)))
    (dolist (def defs)
      (let ((run (create-automation-run
                  :repo-id repo-id
                  :definition-id (getf def :id)
                  :definition-name (getf def :name)
                  :trigger-event trigger
                  :commit-sha commit-sha
                  :ref ref
                  :triggered-by-id triggered-by-id)))
        (llog:info "Scheduled automation"
                   :name (getf def :name)
                   :trigger trigger
                   :run-id (getf run :id))))))

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
