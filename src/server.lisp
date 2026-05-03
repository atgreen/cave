;;; server.lisp — HTTP server, routing, and request handling
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Server infrastructure

(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))
(defvar *acceptor* nil)

(defclass cave-acceptor (easy-routes:easy-routes-acceptor)
  ()
  (:documentation "Cave HTTP acceptor with per-request auth."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor cave-acceptor) request)
  "Wrap every request with a pooled DB connection, auth context, and metrics."
  (let ((method (hunchentoot:request-method request))
        (start (get-internal-real-time)))
    (bt:with-lock-held (*metrics-lock*)
      (incf *active-requests*))
    (unwind-protect
         (postmodern:with-connection *db-spec*
           (let ((*current-user* nil)
                 (*current-user-id* nil))
             (authenticate-request)
             (call-next-method)))
      (let* ((elapsed (/ (- (get-internal-real-time) start)
                         (float internal-time-units-per-second 1.0d0)))
             (status (hunchentoot:return-code*)))
        (bt:with-lock-held (*metrics-lock*)
          (decf *active-requests*))
        (record-request method status elapsed)))))

(defun app-root ()
  "Return the application root directory."
  (uiop:getcwd))

;; ----------------------------------------------------------------------------
;; Static files

(defvar *static-dispatch-table* nil)

(defun init-static-dispatch ()
  "Initialize the static file dispatch table from the current working directory."
  (setf *static-dispatch-table*
        (list
         (hunchentoot:create-folder-dispatcher-and-handler
          "/static/" (fad:pathname-as-directory
                      (merge-pathnames "static/" (app-root)))))))

;; ----------------------------------------------------------------------------
;; Response helpers

(defun html-response (html)
  "Return an HTML response string."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  html)

(defun require-login ()
  "Redirect to login if not authenticated. Returns T if logged in."
  (unless *current-user*
    (hunchentoot:redirect
     (format nil "/-/auth/login?next=~A"
             (hunchentoot:url-encode (hunchentoot:request-uri*))))
    (return-from require-login nil))
  t)

(defun json-response (data &key (status 200))
  "Return a JSON response."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:return-code*) status)
  (com.inuoe.jzon:stringify data))

(defun json-error (message &key (status 400))
  "Return a JSON error response."
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "error" obj) message)
    (json-response obj :status status)))

(defun not-found ()
  "Return 404."
  (setf (hunchentoot:return-code*) 404)
  "Not found")

(defun sanitize-next-url (next-url)
  "Return NEXT-URL when it is a safe in-app redirect target, otherwise \"/\"."
  (if (and next-url
           (> (length next-url) 0)
           (char= (char next-url 0) #\/)
           (or (= (length next-url) 1)
               (not (char= (char next-url 1) #\/)))
           (not (find #\Newline next-url))
           (not (find #\Return next-url)))
      next-url
      "/"))

(defun repo-visible-p (repo)
  "Check if the current user can see REPO. Public repos are always visible."
  (or (not (getf repo :is-private))
      (and *current-user-id*
           (repo-member-role (getf repo :id) *current-user-id*))))

(defun ensure-repo-visible (repo responder)
  "Return REPO when visible, otherwise return RESPONDER's not-found response."
  (unless repo
    (return-from ensure-repo-visible (funcall responder)))
  (unless (repo-visible-p repo)
    (return-from ensure-repo-visible (funcall responder)))
  repo)

;; ----------------------------------------------------------------------------
;; Routes: Metrics

(easy-routes:defroute metrics-endpoint ("/-/metrics" :method :get) ()
  (setf (hunchentoot:content-type*) "text/plain; version=0.0.4; charset=utf-8")
  (collect-metrics))

;; ----------------------------------------------------------------------------
;; Routes: Auth

;; Backwards compat: /login redirects to OIDC login
(easy-routes:defroute login-redirect ("/login" :method :get) ()
  (let ((next-url (sanitize-next-url (hunchentoot:get-parameter "next"))))
    (hunchentoot:redirect
     (format nil "/-/auth/login?next=~A"
             (hunchentoot:url-encode next-url)))))

(easy-routes:defroute oidc-login ("/-/auth/login" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/")
      (let* ((next-url (sanitize-next-url (hunchentoot:get-parameter "next")))
             (state (generate-oidc-state)))
        ;; Store state + next-url in a short-lived cookie
        (hunchentoot:set-cookie "cave_oidc_state"
                                :value (format nil "~A:~A" state next-url)
                                :path "/"
                                :http-only t
                                :max-age 600)
        (hunchentoot:redirect (oidc-authorization-url state)))))

(easy-routes:defroute oidc-callback ("/-/auth/callback" :method :get) ()
  (let* ((code (hunchentoot:get-parameter "code"))
         (state (hunchentoot:get-parameter "state"))
         (cookie (hunchentoot:cookie-in "cave_oidc_state"))
         (colon-pos (when cookie (position #\: cookie)))
         (saved-state (when colon-pos (subseq cookie 0 colon-pos)))
         (next-url (sanitize-next-url (if colon-pos (subseq cookie (1+ colon-pos)) "/"))))
    ;; Clear the state cookie
    (hunchentoot:set-cookie "cave_oidc_state" :value "" :path "/" :max-age 0)
    ;; Validate state
    (unless (and code state saved-state (string= state saved-state))
      (setf (hunchentoot:return-code*) 400)
      (return-from oidc-callback "Invalid OIDC state"))
    ;; Exchange code for tokens
    (let ((tokens (exchange-oidc-code code)))
      (unless tokens
        (setf (hunchentoot:return-code*) 502)
        (return-from oidc-callback "Failed to exchange authorization code"))
      (let* ((access-token (gethash "access_token" tokens))
             (userinfo (fetch-oidc-userinfo access-token)))
        (unless userinfo
          (setf (hunchentoot:return-code*) 502)
          (return-from oidc-callback "Failed to fetch user info"))
        ;; Provision/update local user
        (let ((user (provision-oidc-user userinfo)))
          (unless (getf user :is-active)
            (setf (hunchentoot:return-code*) 403)
            (return-from oidc-callback "Account is deactivated"))
          ;; Create Cave session
          (let ((session-token (create-session (getf user :id))))
            (hunchentoot:set-cookie "cave_session"
                                    :value session-token
                                    :path "/"
                                    :http-only t
                                    :max-age (* *session-duration-hours* 3600))
            (hunchentoot:redirect (or next-url "/"))))))))

(easy-routes:defroute logout ("/logout" :method :post) ()
  (delete-session (hunchentoot:cookie-in "cave_session"))
  (hunchentoot:set-cookie "cave_session" :value "" :path "/" :max-age 0)
  ;; Redirect to Keycloak logout to end SSO session
  (let ((issuer (config-value :oidc-issuer)))
    (if issuer
        (hunchentoot:redirect
         (format nil "~A/protocol/openid-connect/logout?post_logout_redirect_uri=~A&client_id=~A"
                 issuer
                 (hunchentoot:url-encode (config-value :base-url))
                 (config-value :oidc-client-id)))
        (hunchentoot:redirect "/"))))

;; ----------------------------------------------------------------------------
;; Routes: Dashboard

(easy-routes:defroute dashboard ("/" :method :get) ()
  (if *current-user*
      (html-response
       (view-dashboard :orgs (list-user-orgs *current-user-id*)
                       :repos (list-user-repos *current-user-id* :include-private t)
                       :username (getf *current-user* :username)))
      (hunchentoot:redirect "/login")))

;; ----------------------------------------------------------------------------
;; Routes: Org creation

(easy-routes:defroute new-org-page ("/-/new-org" :method :get) ()
  (when (require-login)
    (html-response (view-new-org))))

(easy-routes:defroute create-org-submit ("/-/new-org" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name"))
          (display-name (hunchentoot:post-parameter "display_name"))
          (description (hunchentoot:post-parameter "description")))
      (handler-case
          (progn
            (create-org :name name
                        :display-name (if (uiop:emptyp display-name) name display-name)
                        :description (unless (uiop:emptyp description) description)
                        :creator-id *current-user-id*)
            (hunchentoot:redirect (format nil "/o/~A" name)))
        (error (e)
          (html-response (view-new-org :error (format nil "~A" e))))))))

;; ----------------------------------------------------------------------------
;; Routes: Admin

(easy-routes:defroute admin-page ("/-/admin" :method :get) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-page "Forbidden"))
    (html-response (view-admin :users (list-users :active-only nil)))))


;; ----------------------------------------------------------------------------
;; Routes: User settings

(easy-routes:defroute settings-page ("/-/settings" :method :get) ()
  (when (require-login)
    (html-response
     (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                    :api-tokens (list-api-tokens *current-user-id*)))))

(easy-routes:defroute add-ssh-key-submit ("/-/settings/ssh-keys" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name"))
          (public-key (string-trim '(#\Newline #\Return #\Space)
                                   (hunchentoot:post-parameter "public_key"))))
      (handler-case
          (progn (add-ssh-key *current-user-id* name public-key)
                 (sync-authorized-keys)
                 (hunchentoot:redirect "/-/settings"))
        (error (e)
          (html-response
           (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                          :api-tokens (list-api-tokens *current-user-id*)
                          :ssh-error (format nil "~A" e))))))))

(easy-routes:defroute generate-ssh-key-submit
    ("/-/settings/ssh-keys/generate" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name")))
      (handler-case
          (multiple-value-bind (private-key _record)
              (generate-ssh-keypair *current-user-id* name)
            (declare (ignore _record))
            (sync-authorized-keys)
            (html-response
             (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                            :api-tokens (list-api-tokens *current-user-id*)
                            :generated-private-key private-key
                            :generated-key-name name)))
        (error (e)
          (html-response
           (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                          :api-tokens (list-api-tokens *current-user-id*)
                          :ssh-error (format nil "~A" e))))))))

(easy-routes:defroute delete-ssh-key-submit
    ("/-/settings/ssh-keys/:key-id/delete" :method :post) ()
  (when (require-login)
    (let ((kid (parse-integer key-id :junk-allowed t)))
      (when kid (delete-ssh-key kid *current-user-id*)))
    (sync-authorized-keys)
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute create-token-submit ("/-/settings/tokens" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name")))
      (multiple-value-bind (token-string _record)
          (create-api-token *current-user-id* name)
        (declare (ignore _record))
        (html-response
         (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                        :api-tokens (list-api-tokens *current-user-id*)
                        :new-token token-string))))))

(easy-routes:defroute delete-token-submit
    ("/-/settings/tokens/:token-id/delete" :method :post) ()
  (when (require-login)
    (let ((tid (parse-integer token-id :junk-allowed t)))
      (when tid (delete-api-token tid *current-user-id*)))
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute download-cav ("/-/downloads/cav" :method :get) ()
  (let ((path (cav-download-path)))
    (unless path
      (setf (hunchentoot:return-code*) 404)
      (return-from download-cav "cav is not installed on this Cave host"))
    (setf (hunchentoot:header-out "Content-Disposition")
          "attachment; filename=\"cav\"")
    (hunchentoot:handle-static-file path "application/octet-stream")))

;; ----------------------------------------------------------------------------
;; Routes: Personal repo creation

(easy-routes:defroute new-personal-repo-page ("/-/new-repo" :method :get) ()
  (when (require-login)
    (html-response (view-new-personal-repo))))

(easy-routes:defroute create-personal-repo-submit ("/-/new-repo" :method :post) ()
  (when (require-login)
    (let* ((name (hunchentoot:post-parameter "name"))
           (description (hunchentoot:post-parameter "description"))
           (is-private (hunchentoot:post-parameter "is_private"))
           (username (getf *current-user* :username)))
      (handler-case
          (let ((repo (create-repo :owner-id *current-user-id*
                                   :name name
                                   :description description
                                   :is-private (when is-private t))))
            (init-bare-repo username name)
            (log-event "repo.created" :user-id *current-user-id*
                                      :repo-id (getf repo :id))
            (hunchentoot:redirect (format nil "/~A/~A" username name)))
        (error (e)
          (html-response (view-new-personal-repo :error (format nil "~A" e))))))))

;; ----------------------------------------------------------------------------
;; Routes: User profile (public repos listing)

(easy-routes:defroute user-profile-page ("/u/:username" :method :get) ()
  (let ((user (find-user-by-username username)))
    (unless user (return-from user-profile-page (not-found)))
    (let* ((is-self (and *current-user-id* (= *current-user-id* (getf user :id))))
           (repos (list-user-repos (getf user :id) :include-private is-self)))
      (html-response (view-user-profile :user user :repos repos :is-self is-self)))))

;; ----------------------------------------------------------------------------
;; Routes: Orgs (keep /o/ prefix for explicit org access)

(easy-routes:defroute org-page ("/o/:org-name" :method :get) ()
  (let ((org (find-org-by-name org-name)))
    (unless org (return-from org-page (not-found)))
    (let* ((is-member (and *current-user-id*
                           (org-member-role (getf org :id) *current-user-id*)))
           (repos (list-org-repos (getf org :id) :include-private is-member)))
      (html-response (view-org :org org :repos repos :is-member is-member)))))

;; ----------------------------------------------------------------------------
;; Routes: Owner (user or org) profile — single-segment catch-all

(easy-routes:defroute owner-page ("/:name" :method :get) ()
  ;; Try user first, then org
  (let ((user (find-user-by-username name)))
    (when user
      (let* ((is-self (and *current-user-id* (= *current-user-id* (getf user :id))))
             (repos (list-user-repos (getf user :id) :include-private is-self)))
        (return-from owner-page
          (html-response (view-user-profile :user user :repos repos :is-self is-self))))))
  (let ((org (find-org-by-name name)))
    (when org
      (let* ((is-member (and *current-user-id*
                             (org-member-role (getf org :id) *current-user-id*)))
             (repos (list-org-repos (getf org :id) :include-private is-member)))
        (return-from owner-page
          (html-response (view-org :org org :repos repos :is-member is-member))))))
  (not-found))

;; ----------------------------------------------------------------------------
;; Routes: Repos

(easy-routes:defroute repo-page ("/:owner/:repo-name" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from repo-page repo))
    (let* ((role (and *current-user-id*
                      (repo-member-role (getf repo :id) *current-user-id*)))
           (disk-path (repo-disk-path owner repo-name))
           (empty (git-repo-empty-p disk-path))
           (default-branch (unless empty (or (git-default-branch disk-path) "main")))
           (branches (unless empty (git-branches disk-path)))
           (tags (unless empty (git-tags disk-path)))
           (commit-count (unless empty (git-commit-count disk-path :branch default-branch)))
           (file-tree (unless empty (git-tree disk-path :ref default-branch)))
           (readme-entry (unless empty (git-readme-path disk-path :ref default-branch)))
           (readme-content (when readme-entry
                             (git-blob disk-path default-branch
                                       (getf readme-entry :name))))
           (readme-html (when (and readme-content
                                   (search ".md" (string-downcase
                                                   (getf readme-entry :name))))
                          (render-markdown readme-content)))
           ;; Plain text README (no markdown)
           (readme-html (or readme-html
                            (when readme-content
                              (format nil "<pre>~A</pre>"
                                      (spinneret::escape-string readme-content)))))
           (recent-commits (unless empty (git-log disk-path :limit 5)))
           (open-issues (list-issues (getf repo :id) :status "open" :limit 5))
           (open-pulls (list-pull-requests (getf repo :id) :status "open" :limit 5)))
      (html-response
       (view-repo :owner-name owner :repo repo :role role
                  :empty empty :branches branches :tags tags
                  :default-branch default-branch :commit-count commit-count
                  :file-tree file-tree
                  :readme-html readme-html
                  :readme-filename (when readme-entry (getf readme-entry :name))
                  :recent-commits recent-commits
                  :issues open-issues :pulls open-pulls)))))

;; Tree (directory) browsing
(easy-routes:defroute tree-page ("/:owner/:repo-name/tree/:ref" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from tree-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (path (or (hunchentoot:get-parameter "path") ""))
           (file-tree (git-tree disk-path :ref ref :path path)))
      (html-response
       (view-tree :owner-name owner :repo repo :ref ref
                  :path path :file-tree file-tree)))))

;; Blob (file) viewing
(easy-routes:defroute blob-page ("/:owner/:repo-name/blob/:ref" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from blob-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (path (or (hunchentoot:get-parameter "path") ""))
           (file-size (git-blob-size disk-path ref path))
           (content (when (and file-size (<= file-size (* 2 1024 1024)))
                      (git-blob disk-path ref path)))
           (is-binary (git-blob-binary-p content))
           (language (file-language path)))
      (unless file-size (return-from blob-page (not-found)))
      (html-response
       (view-blob :owner-name owner :repo repo :ref ref :path path
                  :content (unless is-binary content)
                  :is-binary is-binary
                  :file-size file-size
                  :language language)))))

;; Raw file content
(easy-routes:defroute raw-page ("/:owner/:repo-name/raw/:ref" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from raw-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (path (or (hunchentoot:get-parameter "path") ""))
           (content (git-blob disk-path ref path)))
      (unless content (return-from raw-page (not-found)))
      (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
      content)))

;; Commit detail page (also handles .patch and .diff suffixes)
(easy-routes:defroute commit-page ("/:owner/:repo-name/commit/:hash" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from commit-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           ;; Strip .patch or .diff suffix
           (is-patch (and (> (length hash) 6)
                          (string= ".patch" (subseq hash (- (length hash) 6)))))
           (is-diff (and (> (length hash) 5)
                         (string= ".diff" (subseq hash (- (length hash) 5)))))
           (clean-hash (cond (is-patch (subseq hash 0 (- (length hash) 6)))
                             (is-diff (subseq hash 0 (- (length hash) 5)))
                             (t hash))))
      (cond
        ;; .patch — git format-patch output
        (is-patch
         (let ((patch (git-format-patch disk-path clean-hash)))
           (unless patch (return-from commit-page (not-found)))
           (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
           patch))
        ;; .diff — raw unified diff
        (is-diff
         (let ((diff (git-commit-diff disk-path clean-hash)))
           (unless diff (return-from commit-page (not-found)))
           (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
           diff))
        ;; HTML commit page
        (t
         (let ((commit (git-show-commit disk-path clean-hash)))
           (unless commit (return-from commit-page (not-found)))
           (let ((diff-raw (git-commit-diff disk-path clean-hash)))
             (html-response
              (view-commit :owner-name owner :repo repo :commit commit
                           :diff-raw diff-raw)))))))))

(easy-routes:defroute new-org-repo-page ("/o/:org-name/-/new-repo" :method :get) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from new-org-repo-page (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from new-org-repo-page "Forbidden"))
      (html-response (view-new-repo :org org)))))

(easy-routes:defroute create-org-repo-submit ("/o/:org-name/-/new-repo" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from create-org-repo-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from create-org-repo-submit "Forbidden"))
      (let* ((name (hunchentoot:post-parameter "name"))
             (description (hunchentoot:post-parameter "description"))
             (is-private (hunchentoot:post-parameter "is_private"))
             (repo (create-repo :org-id (getf org :id)
                                :name name
                                :description description
                                :is-private (when is-private t))))
        (init-bare-repo org-name name)
        (log-event "repo.created" :user-id *current-user-id*
                                  :repo-id (getf repo :id))
        (hunchentoot:redirect (format nil "/~A/~A" org-name name))))))

;; ----------------------------------------------------------------------------
;; Routes: Issues

(easy-routes:defroute issues-page ("/:owner/:repo-name/issues" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from issues-page repo))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (html-response
       (view-issues :owner-name owner :repo repo :issues issues :current-status status)))))

(easy-routes:defroute new-issue-page
    ("/:owner/:repo-name/issues/new" :method :get) ()
  (when (require-login)
    (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
      (unless repo (return-from new-issue-page repo))
      (html-response (view-new-issue :owner-name owner :repo repo)))))

(easy-routes:defroute create-issue-submit
    ("/:owner/:repo-name/issues/new" :method :post) ()
  (when (require-login)
    (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
      (unless repo (return-from create-issue-submit repo))
      (let ((issue (create-issue :repo-id (getf repo :id)
                                 :author-id *current-user-id*
                                 :title (hunchentoot:post-parameter "title")
                                 :body (hunchentoot:post-parameter "body"))))
        (log-event "issue.created" :user-id *current-user-id*
                                   :repo-id (getf repo :id)
                                   :entity-type "issue"
                                   :entity-id (getf issue :id))
        (hunchentoot:redirect
         (format nil "/~A/~A/issues/~A" owner repo-name (getf issue :number)))))))

(easy-routes:defroute issue-page
    ("/:owner/:repo-name/issues/:number" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from issue-page repo))
    (let* ((num (parse-integer number :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from issue-page (not-found)))
      (html-response
       (view-issue :owner-name owner :repo repo :issue issue
                   :author (find-user-by-id (getf issue :author-id))
                   :comments (list-issue-comments (getf issue :id)))))))

(easy-routes:defroute issue-comment-submit
    ("/:owner/:repo-name/issues/:number/comment" :method :post) ()
  (when (require-login)
    (let* ((repo (find-repo owner repo-name))
           (num (parse-integer number :junk-allowed t))
           (issue (when (and repo num) (find-issue (getf repo :id) num))))
      (unless issue (return-from issue-comment-submit (not-found)))
      (let ((body (hunchentoot:post-parameter "body"))
            (action (hunchentoot:post-parameter "action")))
        ;; Add comment if body is non-empty
        (when (and body (not (uiop:emptyp body)))
          (create-issue-comment :issue-id (getf issue :id)
                                :author-id *current-user-id*
                                :body body))
        ;; Close or reopen
        (when (equal action "close")
          (update-issue (getf issue :id) :status "closed")
          (when (or (not body) (uiop:emptyp body))
            (create-issue-comment :issue-id (getf issue :id)
                                  :author-id *current-user-id*
                                  :body "Closed this issue.")))
        (when (equal action "reopen")
          (update-issue (getf issue :id) :status "open")
          (when (or (not body) (uiop:emptyp body))
            (create-issue-comment :issue-id (getf issue :id)
                                  :author-id *current-user-id*
                                  :body "Reopened this issue."))))
      (hunchentoot:redirect
       (format nil "/~A/~A/issues/~A" owner repo-name number)))))

;; ----------------------------------------------------------------------------
;; Routes: Pull requests

(easy-routes:defroute pulls-page
    ("/:owner/:repo-name/pulls" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from pulls-page repo))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (pulls (list-pull-requests (getf repo :id) :status status)))
      (html-response
       (view-pull-requests :owner-name owner :repo repo :pulls pulls
                        :current-status status)))))

(easy-routes:defroute new-pull-request-page
    ("/:owner/:repo-name/pulls/new" :method :get) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from new-pull-request-page (not-found)))
      (let* ((disk-path (repo-disk-path owner repo-name))
             (branches (git-branches disk-path))
             (default-branch (or (git-default-branch disk-path) "main")))
        (html-response
         (view-new-pull-request :owner-name owner :repo repo
                                :branches branches
                                :default-branch default-branch))))))

(easy-routes:defroute create-pull-request-submit
    ("/:owner/:repo-name/pulls/new" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from create-pull-request-submit (not-found)))
      (let* ((source (hunchentoot:post-parameter "source_branch"))
             (target (hunchentoot:post-parameter "target_branch"))
             (disk-path (repo-disk-path owner repo-name))
             (head-commit (handler-case
                              (string-trim '(#\Newline #\Space)
                                           (uiop:run-program
                                            (list "git" "-C" (namestring disk-path)
                                                  "rev-parse" source)
                                            :output :string))
                            (error () nil)))
             (pr (create-pull-request :repo-id (getf repo :id)
                                      :author-id *current-user-id*
                                      :source-branch source
                                      :target-branch target
                                      :head-commit head-commit)))
        (hunchentoot:redirect
         (format nil "/~A/~A/pulls/~A" owner repo-name (getf pr :number)))))))

(easy-routes:defroute pull-request-page
    ("/:owner/:repo-name/pulls/:number" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from pull-request-page repo))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from pull-request-page (not-found)))
      (let* ((author (find-user-by-id (getf pr :author-id)))
             (reviews-raw (list-reviews (getf pr :id)))
             (concerns-all (list-concerns (getf pr :id)))
             (reviews (mapcar
                       (lambda (r)
                         (append r
                          (list :is-stale (review-is-stale-p r pr)
                                :concerns (remove-if-not
                                           (lambda (c) (= (getf c :review-id) (getf r :id)))
                                           concerns-all))))
                       reviews-raw))
             (eligibility (compute-merge-eligibility pr repo))
             (can-merge (and (pull-request-mergeable-p eligibility)
                             *current-user-id*
                             (equal (repo-member-role (getf repo :id) *current-user-id*)
                                    "admin")))
             (stack (find-stack-by-id (getf pr :stack-id)))
             (stack-items (when stack (list-stack-pull-requests (getf stack :id))))
             ;; Diff
             (disk-path (repo-disk-path owner repo-name))
             (source (getf pr :source-branch))
             (target (getf pr :target-branch))
             (diff-raw (git-diff-merge-base disk-path target source))
             ;; Inline diff comments
             (raw-comments (list-diff-comments (getf pr :id)))
             (comment-hts (mapcar (lambda (c)
                                    (let ((ht (make-hash-table :test 'equal)))
                                      (setf (gethash "file_path" ht) (getf c :file-path))
                                      (setf (gethash "line_number" ht) (getf c :line-number))
                                      (setf (gethash "side" ht) (getf c :side))
                                      (setf (gethash "body" ht) (getf c :body))
                                      (setf (gethash "username" ht) (getf c :username))
                                      (setf (gethash "created_at" ht)
                                            (princ-to-string (getf c :created-at)))
                                      ht))
                                  raw-comments))
             (comments-json (if comment-hts
                                (com.inuoe.jzon:stringify comment-hts)
                                "[]")))
        (html-response
         (view-pull-request :owner-name owner :repo repo :pr pr
                         :author author :reviews reviews
                         :eligibility eligibility :can-merge can-merge
                         :stack stack :stack-items stack-items
                         :diff-raw diff-raw
                         :diff-comments-json comments-json
                         :comment-action (format nil "/~A/~A/pulls/~A/diff-comment"
                                                 owner repo-name num)))))))

;; Inline diff comment
(easy-routes:defroute diff-comment-submit
    ("/:owner/:repo-name/pulls/:number/diff-comment" :method :post) ()
  (when (require-login)
    (let* ((repo (find-repo owner repo-name))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless pr (return-from diff-comment-submit (not-found)))
      (let ((file-path (hunchentoot:post-parameter "file_path"))
            (line-number (parse-integer (or (hunchentoot:post-parameter "line_number") "0")
                                        :junk-allowed t))
            (side (or (hunchentoot:post-parameter "side") "new"))
            (body (hunchentoot:post-parameter "body")))
        (when (and file-path line-number body (not (uiop:emptyp body)))
          (create-diff-comment :changeset-id (getf pr :id)
                               :author-id *current-user-id*
                               :file-path file-path
                               :line-number line-number
                               :side side
                               :body body)))
      (hunchentoot:redirect
       (format nil "/~A/~A/pulls/~A" owner repo-name number)))))

(easy-routes:defroute submit-review
    ("/:owner/:repo-name/pulls/:number/review" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless repo (return-from submit-review repo))
      (unless pr (return-from submit-review (not-found)))
      (let* ((state (hunchentoot:post-parameter "state"))
             (body (hunchentoot:post-parameter "body"))
             (concern-text (hunchentoot:post-parameter "concern_text"))
             (review (create-review
                      :changeset-id (getf pr :id)
                      :reviewer-id *current-user-id*
                      :state state
                      :body (unless (uiop:emptyp body) body)
                      :changeset-version (getf pr :version))))
        (when (and (equal state "approve_with_concerns")
                   (not (uiop:emptyp concern-text)))
          (create-concern :review-id (getf review :id)
                          :changeset-id (getf pr :id)
                          :author-id *current-user-id*
                          :body concern-text))
        (log-event "review.submitted"
                   :user-id *current-user-id*
                   :repo-id (getf repo :id)
                   :entity-type "review"
                   :entity-id (getf review :id))
        (hunchentoot:redirect
         (format nil "/~A/~A/pulls/~A" owner repo-name number))))))

(easy-routes:defroute resolve-concern-submit
    ("/:owner/:repo-name/concerns/:concern-id/resolve" :method :post) ()
  (when (require-login)
    (let* ((cid (parse-integer concern-id :junk-allowed t))
           (concern (when cid (find-concern-by-id cid))))
      (when concern
        (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
               (role (when repo (repo-member-role (getf repo :id) *current-user-id*))))
          (when (or (= (getf concern :author-id) *current-user-id*)
                    (equal role "admin"))
            (resolve-concern cid *current-user-id*))))
      (let ((pr (when concern (find-pull-request-by-id (getf concern :changeset-id)))))
        (hunchentoot:redirect
         (if pr
             (format nil "/~A/~A/pulls/~A" owner repo-name (getf pr :number))
             (format nil "/~A/~A" owner repo-name)))))))

(easy-routes:defroute merge-pull-request-submit
    ("/:owner/:repo-name/pulls/:number/merge" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless repo (return-from merge-pull-request-submit repo))
      (unless pr (return-from merge-pull-request-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from merge-pull-request-submit "Forbidden"))
      (let ((eligibility (compute-merge-eligibility pr repo)))
        (unless (pull-request-mergeable-p eligibility)
          (setf (hunchentoot:return-code*) 422)
          (return-from merge-pull-request-submit "Pull request is not mergeable")))
      (merge-pull-request (getf pr :id))
      (log-event "pr.merged"
                 :user-id *current-user-id*
                 :repo-id (getf repo :id)
                 :entity-type "pull_request"
                 :entity-id (getf pr :id))
      (hunchentoot:redirect
       (format nil "/~A/~A/pulls/~A" owner repo-name number)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Issues

(easy-routes:defroute api-list-issues
    ("/api/v1/repos/:owner/:repo-name/issues" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name)
                                   (lambda () (json-error "not found" :status 404)))))
    (unless repo (return-from api-list-issues repo))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (json-response issues))))

(easy-routes:defroute api-create-issue
    ("/api/v1/repos/:owner/:repo-name/issues" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-issue (json-error "unauthorized" :status 401)))
  (let ((repo (ensure-repo-visible (find-repo owner repo-name)
                                   (lambda () (json-error "not found" :status 404)))))
    (unless repo (return-from api-create-issue repo))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (title (gethash "title" json))
           (body (gethash "body" json)))
      (unless title (return-from api-create-issue (json-error "title required")))
      (json-response
       (create-issue :repo-id (getf repo :id)
                     :author-id *current-user-id*
                     :title title
                     :body (if (eq body 'null) nil body))
       :status 201))))

(easy-routes:defroute api-get-issue
    ("/api/v1/repos/:owner/:repo-name/issues/:id" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name)
                                   (lambda () (json-error "not found" :status 404)))))
    (unless repo (return-from api-get-issue repo))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-get-issue (json-error "not found" :status 404)))
      (json-response issue))))

;; ----------------------------------------------------------------------------
;; Git helpers

(defun repo-disk-path (owner repo-name)
  "Return the on-disk path for a bare git repo."
  (merge-pathnames (format nil "~A/~A.git/" owner repo-name) (repos-dir)))

(defun init-bare-repo (owner repo-name)
  "Initialize a bare git repository on disk with HEAD pointing to main.
   Sets ownership to cave:cave so SSH pushes work."
  (let ((path (repo-disk-path owner repo-name)))
    (ensure-directories-exist path)
    (uiop:run-program (list "git" "init" "--bare" "-b" "main" (namestring path))
                       :output :string :error-output :string)
    ;; Ensure the cave user owns the repo (server may run as root)
    (uiop:run-program (list "chown" "-R" "cave:cave" (namestring path))
                       :output :string :error-output :string :ignore-error-status t)
    (llog:info "Initialized bare repo" :path path)
    path))

;; ----------------------------------------------------------------------------
;; Server start

(defun start-server (port)
  "Start the web application."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* nil)
  (setf hunchentoot:*show-lisp-backtraces-p* nil)
  ;; Disable hunchentoot's built-in session handling — we manage our own sessions
  (setf hunchentoot:*session-max-time* 0)
  (setf hunchentoot:*use-user-agent-for-sessions* nil)
  (setf hunchentoot:*rewrite-for-session-urls* nil)
  ;; Suppress the "No session" log noise
  (setf hunchentoot:*log-lisp-warnings-p* nil)
  (init-static-dispatch)
  (setf hunchentoot:*dispatch-table* *static-dispatch-table*)
  (setf *acceptor* (make-instance 'cave-acceptor :port port))
  (handler-case (hunchentoot:start *acceptor*)
    (usocket:address-in-use-error ()
      (format *error-output* "~&Port ~A is already in use.~%" port)
      (uiop:quit 1)))
  (llog:info "HTTP server started" :port port)
  *acceptor*)
