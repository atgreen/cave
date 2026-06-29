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

(defun repo-recipient-ids (repo-id)
  "User-ids who should be notified about activity on REPO-ID: the repo owner
   (tracked via owner_id, NOT a cave_repo_members row), explicit members, and
   watchers. De-duplicated."
  (remove-duplicates
   (append (let ((r (find-repo-by-id repo-id)))
             (when (and r (integerp (getf r :owner-id)))
               (list (getf r :owner-id))))
           (mapcar (lambda (m) (getf m :user-id)) (list-repo-members repo-id))
           (repo-watcher-ids repo-id))))

(defun notify-repo-participants (repo-id subject body &key exclude-user-id)
  "Email the repo owner, members, and watchers (except exclude-user-id)."
  (dolist (uid (repo-recipient-ids repo-id))
    (unless (and exclude-user-id (= uid exclude-user-id))
      (let ((user (find-user-by-id uid)))
        (when (and user (getf user :email)
                   (stringp (getf user :email))
                   (find #\@ (getf user :email)))
          (send-email (getf user :email) subject body))))))

(defun notify-inapp (repo-id kind subject link &key exclude-user-id)
  "Create in-app notifications for a repo's owner, members, and watchers (minus the actor)."
  (let ((recipients (repo-recipient-ids repo-id)))
    (dolist (uid recipients)
      (when (and uid (not (and exclude-user-id (= uid exclude-user-id))))
        (handler-case
            (create-notification :user-id uid :repo-id repo-id
                                 :kind kind :subject subject :link link)
          (error (e)
            (llog:warn "in-app notify failed" :error (princ-to-string e))))))))

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
                              :exclude-user-id *current-user-id*)
    (notify-inapp (getf repo :id) "issue"
                  (format nil "New issue #~A: ~A" (getf issue :number) (getf issue :title))
                  (format nil "/~A/~A/issues/~A" owner-name repo-name (getf issue :number))
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
                              :exclude-user-id *current-user-id*)
    (notify-inapp (getf repo :id) "issue_comment"
                  (format nil "Comment on #~A: ~A" (getf issue :number) (getf issue :title))
                  (format nil "/~A/~A/issues/~A" owner-name repo-name (getf issue :number))
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
                              :exclude-user-id *current-user-id*)
    (notify-inapp (getf repo :id) "pr_review"
                  (format nil "Review on PR #~A: ~A" (getf pr :number) state)
                  (format nil "/~A/~A/pulls/~A" owner-name repo-name (getf pr :number))
                  :exclude-user-id *current-user-id*)))

(defun notify-pr-opened (repo owner-name repo-name pr)
  "Notify a repo's owner/members/watchers about a newly opened PR (minus the
   author). Works for human PRs and bot-opened ones (e.g. dependency auto-fix);
   the author is taken from the PR record, so *current-user* need not be bound."
  (let* ((author (when (integerp (getf pr :author-id))
                   (find-user-by-id (getf pr :author-id))))
         (who (or (and author (getf author :username))
                  (and *current-user* (getf *current-user* :username))
                  "someone"))
         (subject (format nil "[~A/~A] New PR #~A: ~A → ~A"
                          owner-name repo-name (getf pr :number)
                          (getf pr :source-branch) (getf pr :target-branch)))
         (body (format nil "~A opened pull request #~A (~A → ~A)~%~%~A/~A/~A/pulls/~A"
                       who (getf pr :number)
                       (getf pr :source-branch) (getf pr :target-branch)
                       (config-value :base-url) owner-name repo-name (getf pr :number))))
    (notify-repo-participants (getf repo :id) subject body
                              :exclude-user-id (getf pr :author-id))
    (notify-inapp (getf repo :id) "pr_opened"
                  (format nil "New PR #~A: ~A → ~A" (getf pr :number)
                          (getf pr :source-branch) (getf pr :target-branch))
                  (format nil "/~A/~A/pulls/~A" owner-name repo-name (getf pr :number))
                  :exclude-user-id (getf pr :author-id))))

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
                 ;; SSRF guard: never deliver to loopback/private/internal hosts,
                 ;; and never follow redirects (an external 302 -> internal would
                 ;; bypass the host check).
                 (target-url (ensure-safe-remote-url (getf hook :url)))
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
                (dex:post target-url
                          :content json-body
                          :headers headers
                          :max-redirects 0
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
                              :exclude-user-id *current-user-id*)
    (notify-inapp (getf repo :id) "pr_merged"
                  (format nil "PR #~A merged: ~A → ~A" (getf pr :number)
                          (getf pr :source-branch) (getf pr :target-branch))
                  (format nil "/~A/~A/pulls/~A" owner-name repo-name (getf pr :number))
                  :exclude-user-id *current-user-id*)))
