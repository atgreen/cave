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
             ;; Intercept git smart HTTP before easy-routes dispatch
             (let ((uri (hunchentoot:script-name request)))
               (when (and (search ".git/" uri)
                          (or (search "/info/refs" uri)
                              (search "/git-upload-pack" uri)))
                 (let* ((git-suffix-pos (search ".git/" uri))
                        (repo-path (subseq uri 1 git-suffix-pos))
                        (slash (position #\/ repo-path)))
                   (when slash
                     (return-from hunchentoot:acceptor-dispatch-request
                       (handle-git-http (subseq repo-path 0 slash)
                                        (subseq repo-path (1+ slash)))))))
               ;; Intercept SSE log streaming
               (when (and (search "/runs/w/" uri)
                          (uiop:string-suffix-p uri "/logs")
                          (eq method :get))
                 (handler-case
                     (handle-workflow-logs-sse uri)
                   (error () nil))
                 (return-from hunchentoot:acceptor-dispatch-request nil)))
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

(defun require-sudo (return-url)
  "Redirect to sudo re-authentication if not in sudo mode. Returns T if sudo is active."
  (unless *current-user*
    (require-login)
    (return-from require-sudo nil))
  (unless (sudo-active-p)
    (hunchentoot:redirect (format nil "/-/sudo?next=~A"
                                  (hunchentoot:url-encode return-url)))
    (return-from require-sudo nil))
  t)

(defun plist-to-hash-table (plist)
  "Convert a plist to a hash table with lowercase string keys."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key val) on plist by #'cddr
          do (setf (gethash (string-downcase (substitute #\_ #\- (symbol-name key))) ht)
                   (if (eq val :null) 'null val)))
    ht))

(defun api-serialize (data)
  "Convert postmodern query results to JSON-friendly structures.
Plists become objects, lists of plists become arrays of objects, NIL becomes #()."
  (cond
    ((null data) #())
    ((and (listp data) (keywordp (car data)))
     (plist-to-hash-table data))
    ((and (listp data) (listp (car data)) (keywordp (caar data)))
     (map 'vector #'plist-to-hash-table data))
    (t data)))

(defun json-response (data &key (status 200))
  "Return a JSON response."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:return-code*) status)
  (com.inuoe.jzon:stringify (api-serialize data)))

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

(defmacro with-visible-repo ((var owner repo-name responder) &body body)
  "Bind VAR to the repo if visible, otherwise return the responder's error response."
  (let ((err (gensym "ERR")))
    `(multiple-value-bind (,var ,err)
         (ensure-repo-visible (find-repo ,owner ,repo-name) ,responder)
       (if ,var
           (progn ,@body)
           ,err))))

(defun ensure-repo-visible (repo responder)
  "Return REPO when visible, otherwise return (VALUES NIL error-response)."
  (unless repo
    (return-from ensure-repo-visible (values nil (funcall responder))))
  (unless (repo-visible-p repo)
    (return-from ensure-repo-visible (values nil (funcall responder))))
  repo)

;; ----------------------------------------------------------------------------
;; Routes: Internal hooks (called by git hooks inside the container)

(easy-routes:defroute internal-post-receive
    ("/-/internal/hook/post-receive/:owner/:repo-name" :method :post) ()
  ;; Only accept from localhost
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-post-receive "Forbidden"))
  (let ((repo (find-repo owner repo-name)))
    (unless repo
      (setf (hunchentoot:return-code*) 404)
      (return-from internal-post-receive "Not found"))
    ;; Parse refs from POST body (one per line: oldsha newsha refname)
    (let ((body (hunchentoot:raw-post-data :force-text t))
          (refs nil))
      (dolist (line (uiop:split-string body :separator '(#\Newline)))
        (let ((parts (uiop:split-string line :separator '(#\Space))))
          (when (>= (length parts) 3)
            (push (list :old (first parts) :new (second parts) :ref (third parts))
                  refs))))
      ;; Schedule automations
      (dolist (r refs)
        (schedule-automations (getf repo :id) "post_receive"
                              :commit-sha (getf r :new)
                              :ref (getf r :ref))
        ;; Schedule workflow runs from .cave/workflows/
        (handler-case
            (parse-and-schedule-workflows (getf repo :id) "post_receive"
                                          :commit-sha (getf r :new)
                                          :ref (getf r :ref))
          (error (e)
            (llog:error "Workflow scheduling failed" :error (princ-to-string e)))))
      ;; Trigger Zoekt reindexing
      (zoekt-index-repo owner repo-name)
      ;; Fire webhooks
      (dolist (r refs)
        (fire-webhooks (getf repo :id) "push"
                       `(("ref" . ,(getf r :ref))
                         ("after" . ,(getf r :new))
                         ("before" . ,(getf r :old))
                         ("repository" . (("owner" . ,owner)
                                          ("name" . ,repo-name)))))))
    "ok"))

;; ----------------------------------------------------------------------------
;; Routes: Search

(easy-routes:defroute search-page ("/-/search" :method :get) ()
  (when (require-login)
    (let* ((query (or (hunchentoot:get-parameter "q") ""))
           (repo-scope (hunchentoot:get-parameter "repo"))
           (results (if (string= query "")
                        (list :files nil)
                        (zoekt-search-visible query :limit 50
                                              :repo-scope repo-scope))))
      (html-response
       (view-search-results :query query
                            :repo-scope repo-scope
                            :results results)))))

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
         (rest-of-cookie (when colon-pos (subseq cookie (1+ colon-pos))))
         (is-sudo (and rest-of-cookie (>= (length rest-of-cookie) 5)
                       (string= "sudo:" (subseq rest-of-cookie 0 5))))
         (next-url (sanitize-next-url
                    (cond (is-sudo (subseq rest-of-cookie 5))
                          (rest-of-cookie rest-of-cookie)
                          (t "/")))))
    ;; Clear the state cookie
    (hunchentoot:set-cookie "cave_oidc_state" :value "" :path "/" :max-age 0)
    ;; Validate state — if invalid, redirect to login (e.g. after password reset)
    (unless (and code state saved-state (string= state saved-state))
      (hunchentoot:redirect "/-/auth/login")
      (return-from oidc-callback nil))
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
          (if is-sudo
              ;; Sudo flow — set sudo cookie, keep existing session
              (progn
                (set-sudo-cookie)
                (hunchentoot:redirect (or next-url "/-/settings")))
              ;; Normal login — create new session
              (let ((session-token (create-session (getf user :id))))
                (hunchentoot:set-cookie "cave_session"
                                        :value session-token
                                        :path "/"
                                        :http-only t
                                        :max-age (* *session-duration-hours* 3600))
                (hunchentoot:redirect (or next-url "/")))))))))

;; Sudo mode — force re-authentication for dangerous actions
(easy-routes:defroute sudo-redirect ("/-/sudo" :method :get) ()
  (if *current-user*
      (let* ((next-url (or (hunchentoot:get-parameter "next") "/-/settings"))
             (state (generate-oidc-state)))
        ;; Store state with sudo: prefix so callback knows to set sudo cookie
        (hunchentoot:set-cookie "cave_oidc_state"
                                :value (format nil "~A:sudo:~A" state next-url)
                                :path "/"
                                :http-only t
                                :max-age 600)
        (hunchentoot:redirect (oidc-authorization-url state :force-login t)))
      (hunchentoot:redirect "/-/auth/login")))

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
                       :username (getf *current-user* :username)
                       :events (list-recent-events :limit 20)))
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
        (cl-postgres-error:unique-violation ()
          (html-response
           (view-new-org :error (format nil "An organization named \"~A\" already exists." name))))
        (error (e)
          (html-response (view-new-org :error (format nil "~A" e))))))))

;; ----------------------------------------------------------------------------
;; Routes: Admin

(easy-routes:defroute admin-page ("/-/admin" :method :get) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-page "Forbidden"))
    (html-response (view-admin :users (list-users :active-only nil)
                               :runners (list-runners)))))

(easy-routes:defroute admin-create-runner-token ("/-/admin/runners/token" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-create-runner-token "Forbidden"))
    (let ((token (create-registration-token :created-by-id *current-user-id*)))
      (html-response (view-admin :users (list-users :active-only nil)
                                 :runners (list-runners)
                                 :registration-token token)))))

(easy-routes:defroute admin-delete-runner ("/-/admin/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-delete-runner "Forbidden"))
    (let ((rid (parse-integer runner-id :junk-allowed t)))
      (when rid (delete-runner rid)))
    (hunchentoot:redirect "/-/admin")))


;; ----------------------------------------------------------------------------
;; Routes: User settings

(easy-routes:defroute settings-page ("/-/settings" :method :get) ()
  (when (require-login)
    (html-response
     (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                    :api-tokens (list-api-tokens *current-user-id*)
                    :runners (list-runners :scope "user" :scope-id *current-user-id*)))))

(easy-routes:defroute set-theme-submit ("/-/settings/theme" :method :post) ()
  (when (require-login)
    (let ((theme (hunchentoot:post-parameter "theme")))
      (when (and theme (not (uiop:emptyp theme)))
        (set-user-theme *current-user-id* theme)))
    (hunchentoot:redirect "/-/settings")))

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
  (when (require-sudo "/-/settings")
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
  (when (require-sudo "/-/settings")
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

(easy-routes:defroute user-create-runner-token ("/-/settings/runners/token" :method :post) ()
  (when (require-login)
    (let ((token (create-registration-token :scope "user" :scope-id *current-user-id*
                                            :created-by-id *current-user-id*)))
      (html-response
       (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                      :api-tokens (list-api-tokens *current-user-id*)
                      :runners (list-runners :scope "user" :scope-id *current-user-id*)
                      :registration-token token)))))

(easy-routes:defroute user-delete-runner ("/-/settings/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (let ((rid (parse-integer runner-id :junk-allowed t)))
      (when rid
        ;; Only delete if the runner belongs to this user
        (let ((runner (postmodern:query
                       (:select '* :from 'cave-runners
                        :where (:and (:= 'id rid)
                                     (:= 'scope "user")
                                     (:= 'scope-id *current-user-id*)))
                       :plist)))
          (when runner (delete-runner rid)))))
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
    (let* ((mode (or (hunchentoot:post-parameter "mode") "empty"))
           (name (hunchentoot:post-parameter "name"))
           (description (hunchentoot:post-parameter "description"))
           (is-private (hunchentoot:post-parameter "is_private"))
           (url (hunchentoot:post-parameter "url"))
           (auth-token (hunchentoot:post-parameter "auth_token"))
           (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                    :junk-allowed t))
           (username (getf *current-user* :username))
           ;; Auto-derive name from URL if name is empty
           (name (if (and (or (string= mode "import") (string= mode "mirror"))
                          (or (null name) (uiop:emptyp name))
                          url (not (uiop:emptyp url)))
                     (repo-name-from-url url)
                     name)))
      (handler-case
          (let ((repo (create-repo :owner-id *current-user-id*
                                   :name name
                                   :description description
                                   :is-private (when is-private t))))
            (cond
              ((string= mode "import")
               (import-repo-from-url username name url
                                     :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                   auth-token)))
              ((string= mode "mirror")
               (import-repo-from-url username name url
                                     :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                   auth-token))
               (create-mirror :repo-id (getf repo :id)
                              :direction "pull"
                              :remote-url url
                              :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                            auth-token)
                              :interval-minutes (or interval 60)))
              (t (init-bare-repo username name)))
            (log-event "repo.created" :user-id *current-user-id*
                                      :repo-id (getf repo :id)
                                      :metadata (format nil "{\"mode\": \"~A\"}" mode))
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
      (html-response (view-org :org org :repos repos :is-member is-member
                               :is-admin (equal is-member "admin"))))))

(easy-routes:defroute org-settings-page ("/o/:org-name/-/settings" :method :get) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-settings-page (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-settings-page "Forbidden"))
      (html-response
       (view-org-settings :org org :members (list-org-members (getf org :id))
                          :runners (list-runners :scope "org" :scope-id (getf org :id)))))))

(easy-routes:defroute org-create-runner-token ("/o/:org-name/-/settings/runners/token" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-create-runner-token (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-create-runner-token "Forbidden"))
      (let ((token (create-registration-token :scope "org" :scope-id (getf org :id)
                                              :created-by-id *current-user-id*)))
        (html-response
         (view-org-settings :org org :members (list-org-members (getf org :id))
                            :runners (list-runners :scope "org" :scope-id (getf org :id))
                            :registration-token token))))))

(easy-routes:defroute org-delete-runner ("/o/:org-name/-/settings/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-delete-runner (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-delete-runner "Forbidden"))
      (let ((rid (parse-integer runner-id :junk-allowed t)))
        (when rid (delete-runner rid)))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

(easy-routes:defroute org-add-member-submit ("/o/:org-name/-/settings/members" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-add-member-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-add-member-submit "Forbidden"))
      (let* ((username (hunchentoot:post-parameter "username"))
             (role (or (hunchentoot:post-parameter "role") "member"))
             (user (find-user-by-username username)))
        (when user
          (handler-case
              (add-org-member (getf org :id) (getf user :id) :role role)
            (error () nil))))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

(easy-routes:defroute org-remove-member-submit
    ("/o/:org-name/-/settings/members/:user-id/remove" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-remove-member-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-remove-member-submit "Forbidden"))
      (let ((uid (parse-integer user-id :junk-allowed t)))
        (when uid (remove-org-member (getf org :id) uid)))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

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
          (html-response (view-org :org org :repos repos :is-member is-member
                                   :is-admin (equal is-member "admin")))))))
  (not-found))

;; ----------------------------------------------------------------------------
;; Routes: Repos

;; Overview (default landing — README + clone URL)
(easy-routes:defroute repo-page ("/:owner/:repo-name" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from repo-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (empty (git-repo-empty-p disk-path))
           (default-branch (unless empty (or (git-default-branch disk-path) "main")))
           (readme-entry (unless empty (git-readme-path disk-path :ref default-branch)))
           (readme-content (when readme-entry
                             (git-blob disk-path default-branch
                                       (getf readme-entry :name))))
           (readme-html (when (and readme-content
                                   (search ".md" (string-downcase
                                                   (getf readme-entry :name))))
                          (render-markdown readme-content)))
           (readme-html (or readme-html
                            (when readme-content
                              (format nil "<pre>~A</pre>"
                                      (spinneret::escape-string readme-content))))))
      (html-response
       (view-repo :owner-name owner :repo repo :empty empty
                  :default-branch default-branch
                  :readme-html readme-html
                  :readme-filename (when readme-entry (getf readme-entry :name)))))))

;; Code (file browser)
(easy-routes:defroute code-page ("/:owner/:repo-name/code" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from code-page repo))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (empty (git-repo-empty-p disk-path))
           (default-branch (unless empty (or (git-default-branch disk-path) "main")))
           (branches (unless empty (git-branches disk-path)))
           (tags (unless empty (git-tags disk-path)))
           (commit-count (unless empty (git-commit-count disk-path :branch default-branch)))
           (file-tree (unless empty (git-tree disk-path :ref default-branch)))
           (recent-commits (unless empty (git-log disk-path :limit 10))))
      (if empty
          (hunchentoot:redirect (format nil "/~A/~A" owner repo-name))
          (html-response
           (view-code :owner-name owner :repo repo
                      :branches branches :tags tags
                      :default-branch default-branch
                      :commit-count commit-count
                      :recent-commits recent-commits
                      :file-tree file-tree))))))

;; Fork
(easy-routes:defroute fork-repo-submit
    ("/:owner/:repo-name/fork" :method :post) ()
  (when (require-login)
    (let ((source-repo (find-repo owner repo-name)))
      (unless source-repo (return-from fork-repo-submit (not-found)))
      (let* ((username (getf *current-user* :username))
             (existing (find-repo username repo-name)))
        (when existing
          ;; Already forked
          (hunchentoot:redirect (format nil "/~A/~A" username repo-name))
          (return-from fork-repo-submit nil))
        ;; Create the repo record
        (let ((repo (create-repo :owner-id *current-user-id*
                                 :name repo-name
                                 :description (format nil "Fork of ~A/~A" owner repo-name))))
          ;; Clone the bare repo on disk
          (let ((source-path (repo-disk-path owner repo-name))
                (dest-path (repo-disk-path username repo-name)))
            (ensure-directories-exist dest-path)
            (uiop:run-program (list "git" "clone" "--bare"
                                    (namestring source-path)
                                    (namestring dest-path))
                               :output :string :error-output :string)
            ;; Install hooks
            (let ((pre-hook (merge-pathnames "hooks/pre-receive" dest-path)))
              (with-open-file (out pre-hook :direction :output :if-exists :supersede)
                (format out "#!/bin/bash~%exec cave run-checks --config /etc/cave.conf --repo ~A/~A~%"
                        username repo-name))
              (uiop:run-program (list "chmod" "+x" (namestring pre-hook))
                                 :ignore-error-status t))
            (let ((post-hook (merge-pathnames "hooks/post-receive" dest-path)))
              (with-open-file (out post-hook :direction :output :if-exists :supersede)
                (format out "#!/bin/bash~%cave sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
                        username repo-name)
                (when (string= repo-name "cave-themes")
                  (format out "cave sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
                          username)))
              (uiop:run-program (list "chmod" "+x" (namestring post-hook))
                                 :ignore-error-status t))
            ;; Fix ownership
            (uiop:run-program (list "chown" "-R" "cave:cave" (namestring dest-path))
                               :output :string :error-output :string :ignore-error-status t))
          (log-event "repo.forked" :user-id *current-user-id*
                                   :repo-id (getf repo :id)
                                   :metadata (format nil "{\"source\": \"~A/~A\"}" owner repo-name))
          (hunchentoot:redirect (format nil "/~A/~A" username repo-name)))))))

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
           (path (or (hunchentoot:get-parameter "path") "")))
      ;; If path is a directory, redirect to tree view
      (when (git-object-is-tree-p disk-path ref path)
        (hunchentoot:redirect
         (format nil "/~A/~A/tree/~A?path=~A" owner repo-name ref path))
        (return-from blob-page nil))
      (let* ((file-size (git-blob-size disk-path ref path))
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
                    :language language))))))

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
      (let* ((mode (or (hunchentoot:post-parameter "mode") "empty"))
             (name (hunchentoot:post-parameter "name"))
             (description (hunchentoot:post-parameter "description"))
             (is-private (hunchentoot:post-parameter "is_private"))
             (url (hunchentoot:post-parameter "url"))
             (auth-token (hunchentoot:post-parameter "auth_token"))
             (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                      :junk-allowed t))
             (name (if (and (or (string= mode "import") (string= mode "mirror"))
                            (or (null name) (uiop:emptyp name))
                            url (not (uiop:emptyp url)))
                       (repo-name-from-url url)
                       name)))
        (handler-case
            (let ((repo (create-repo :org-id (getf org :id)
                                     :name name
                                     :description description
                                     :is-private (when is-private t))))
              (cond
                ((string= mode "import")
                 (import-repo-from-url org-name name url
                                       :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                     auth-token)))
                ((string= mode "mirror")
                 (import-repo-from-url org-name name url
                                       :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                     auth-token))
                 (create-mirror :repo-id (getf repo :id)
                                :direction "pull"
                                :remote-url url
                                :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                              auth-token)
                                :interval-minutes (or interval 60)))
                (t (init-bare-repo org-name name)))
              (log-event "repo.created" :user-id *current-user-id*
                                        :repo-id (getf repo :id)
                                        :metadata (format nil "{\"mode\": \"~A\"}" mode))
              (hunchentoot:redirect (format nil "/~A/~A" org-name name)))
          (error (e)
            (html-response (view-new-repo :org org :error (format nil "~A" e)))))))))

;; ----------------------------------------------------------------------------
;; Routes: Repo settings

(easy-routes:defroute repo-settings-page
    ("/:owner/:repo-name/settings" :method :get) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-settings-page (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-settings-page "Forbidden"))
      (html-response
       (view-repo-settings :owner-name owner :repo repo
                           :members (list-repo-members (getf repo :id))
                           :checks (list-check-configs (getf repo :id))
                           :mirrors (list-mirrors (getf repo :id))
                           :webhooks (list-webhooks (getf repo :id))
                           :automations (list-automation-definitions (getf repo :id))
                           :runners (list-runners :scope "repo" :scope-id (getf repo :id)))))))

(easy-routes:defroute repo-settings-submit
    ("/:owner/:repo-name/settings" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-settings-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-settings-submit "Forbidden"))
      (let ((section (hunchentoot:post-parameter "section")))
        (when (equal section "merge")
          (update-repo-settings (getf repo :id)
            :required-approvals (or (parse-integer
                                     (or (hunchentoot:post-parameter "required_approvals") "1")
                                     :junk-allowed t) 1)
            :allow-self-approval (when (hunchentoot:post-parameter "allow_self_approval") t)
            :allow-stale-approvals (when (hunchentoot:post-parameter "allow_stale_approvals") t)
            :concerns-count-as-approval (when (hunchentoot:post-parameter "concerns_count") t)
            :block-on-request-changes (when (hunchentoot:post-parameter "block_on_request_changes") t)
            :auto-delete-branch (when (hunchentoot:post-parameter "auto_delete_branch") t))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-add-member-submit
    ("/:owner/:repo-name/settings/members" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-add-member-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-add-member-submit "Forbidden"))
      (let* ((username (hunchentoot:post-parameter "username"))
             (role (or (hunchentoot:post-parameter "role") "writer"))
             (user (find-user-by-username username)))
        (when user
          (handler-case
              (add-repo-member (getf repo :id) (getf user :id) :role role)
            (error () nil))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-remove-member-submit
    ("/:owner/:repo-name/settings/members/:user-id/remove" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-remove-member-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-remove-member-submit "Forbidden"))
      (let ((uid (parse-integer user-id :junk-allowed t)))
        (when uid (remove-repo-member (getf repo :id) uid)))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-archive-submit
    ("/:owner/:repo-name/settings/archive" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-archive-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-archive-submit "Forbidden"))
      (archive-repo (getf repo :id) :archived t)
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-unarchive-submit
    ("/:owner/:repo-name/settings/unarchive" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-unarchive-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-unarchive-submit "Forbidden"))
      (archive-repo (getf repo :id) :archived nil)
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-delete-submit
    ("/:owner/:repo-name/settings/delete" :method :post) ()
  (when (require-sudo (format nil "/~A/~A/settings" owner repo-name))
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-submit "Forbidden"))
      ;; Delete disk files
      (let ((disk-path (repo-disk-path owner repo-name)))
        (when (probe-file disk-path)
          (uiop:delete-directory-tree disk-path :validate t :if-does-not-exist :ignore)))
      ;; Delete from DB (cascades to issues, PRs, etc.)
      (delete-repo (getf repo :id))
      (hunchentoot:redirect (format nil "/~A" owner)))))

;; Automation runs page
(easy-routes:defroute runs-page ("/:owner/:repo-name/runs" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from runs-page repo))
    (html-response
     (view-runs :owner-name owner :repo repo
                :runs (list-automation-runs (getf repo :id))
                :workflow-runs (list-workflow-runs (getf repo :id))))))

(easy-routes:defroute workflow-run-detail-page
    ("/:owner/:repo-name/runs/w/:run-id" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from workflow-run-detail-page repo))
    (let* ((rid (parse-integer run-id :junk-allowed t))
           (run (when rid (find-workflow-run rid))))
      (unless (and run (= (getf run :repo-id) (getf repo :id)))
        (return-from workflow-run-detail-page (not-found)))
      (let ((jobs (list-workflow-jobs rid)))
        (html-response
         (view-workflow-run :owner-name owner :repo repo :run run
                            :jobs (mapcar (lambda (j)
                                            (list :job j
                                                  :steps (list-workflow-steps (getf j :id))))
                                          jobs)))))))

(defun handle-workflow-logs-sse (uri)
  "Handle SSE streaming for workflow run logs. Called from acceptor dispatch."
  ;; Parse run-id from URI: /:owner/:repo/runs/w/:id/logs
  (let* ((w-pos (search "/runs/w/" uri))
         (id-start (+ w-pos 8))
         (id-end (position #\/ uri :start id-start))
         (run-id (parse-integer (subseq uri id-start id-end) :junk-allowed t))
         (run (when run-id (find-workflow-run run-id))))
    (unless run (return-from handle-workflow-logs-sse nil))
    ;; Send SSE headers
    (setf (hunchentoot:content-type*) "text/event-stream")
    (setf (hunchentoot:header-out "Cache-Control") "no-cache")
    (setf (hunchentoot:header-out "X-Accel-Buffering") "no")
    (let ((stream (hunchentoot:send-headers))
          (sent-lengths (make-hash-table))
          (prev-statuses (make-hash-table :test #'equal)))
      (flet ((sse-send (event data)
               (write-sequence
                (flexi-streams:string-to-octets
                 (format nil "event: ~A~%data: ~A~%~%" event data)
                 :external-format :utf-8)
                stream)
               (force-output stream)))
        (handler-case
            (loop repeat 600
                  do (let* ((refreshed-run (find-workflow-run run-id))
                            (run-status (getf refreshed-run :status))
                            (jobs (list-workflow-jobs run-id))
                            (any-active nil))
                       ;; Send run status changes
                       (unless (equal run-status (gethash "run" prev-statuses))
                         (setf (gethash "run" prev-statuses) run-status)
                         (sse-send "run-status" run-status))
                       ;; Send step updates
                       (dolist (job jobs)
                         (dolist (step (list-workflow-steps (getf job :id)))
                           (let* ((step-id (getf step :id))
                                  (log-text (getf step :log))
                                  (log-len (if (and log-text (not (eq log-text :null)))
                                               (length log-text) 0))
                                  (prev-len (gethash step-id sent-lengths 0))
                                  (status (getf step :status))
                                  (status-key (format nil "s~A" step-id)))
                             ;; New log content
                             (when (> log-len prev-len)
                               (let* ((new-text (subseq log-text prev-len))
                                      (escaped (with-output-to-string (s)
                                                 (loop for ch across new-text
                                                       do (if (char= ch #\Newline)
                                                              (write-string "\\n" s)
                                                              (write-char ch s))))))
                                 (sse-send "step-log" (format nil "~A ~A" step-id escaped))
                                 (setf (gethash step-id sent-lengths) log-len)))
                             ;; Status changes
                             (unless (equal status (gethash status-key prev-statuses))
                               (setf (gethash status-key prev-statuses) status)
                               (sse-send "step-status" (format nil "~A ~A" step-id status)))
                             (when (member status '("pending" "running") :test #'equal)
                               (setf any-active t)))))
                       ;; Done?
                       (when (and (member run-status '("success" "failure" "cancelled")
                                          :test #'equal)
                                  (not any-active))
                         (sse-send "done" run-status)
                         (return)))
                     (sleep 1))
          (error () nil))))))

;; Automation definition management
(easy-routes:defroute repo-add-automation-submit
    ("/:owner/:repo-name/settings/automations" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-add-automation-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-add-automation-submit "Forbidden"))
      (let ((name (hunchentoot:post-parameter "name"))
            (trigger (hunchentoot:post-parameter "trigger"))
            (command (hunchentoot:post-parameter "command"))
            (runner-labels (hunchentoot:post-parameter "runner_labels"))
            (timeout (parse-integer (or (hunchentoot:post-parameter "timeout") "60")
                                    :junk-allowed t)))
        (when (and name command (not (uiop:emptyp name)) (not (uiop:emptyp command)))
          (handler-case
              (create-automation-definition
               :repo-id (getf repo :id)
               :name name :trigger trigger :command command
               :runner-labels (or runner-labels "")
               :timeout-seconds (or timeout 60))
            (error () nil))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-delete-automation-submit
    ("/:owner/:repo-name/settings/automations/:auto-id/delete" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-automation-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-automation-submit "Forbidden"))
      (let ((aid (parse-integer auto-id :junk-allowed t)))
        (when aid (delete-automation-definition aid (getf repo :id))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

;; Repo-scoped runner management
(easy-routes:defroute repo-create-runner-token
    ("/:owner/:repo-name/settings/runners/token" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-create-runner-token (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-create-runner-token "Forbidden"))
      (let ((token (create-registration-token :scope "repo" :scope-id (getf repo :id)
                                              :created-by-id *current-user-id*)))
        (html-response
         (view-repo-settings :owner-name owner :repo repo
                             :members (list-repo-members (getf repo :id))
                             :checks (list-check-configs (getf repo :id))
                             :mirrors (list-mirrors (getf repo :id))
                             :webhooks (list-webhooks (getf repo :id))
                             :automations (list-automation-definitions (getf repo :id))
                             :runners (list-runners :scope "repo" :scope-id (getf repo :id))
                             :registration-token token))))))

(easy-routes:defroute repo-delete-runner
    ("/:owner/:repo-name/settings/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-runner (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-runner "Forbidden"))
      (let ((rid (parse-integer runner-id :junk-allowed t)))
        (when rid (delete-runner rid)))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-add-webhook-submit
    ("/:owner/:repo-name/settings/webhooks" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-add-webhook-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-add-webhook-submit "Forbidden"))
      (let ((url (hunchentoot:post-parameter "url"))
            (secret (hunchentoot:post-parameter "secret"))
            (events (hunchentoot:post-parameter "events")))
        (when (and url (not (uiop:emptyp url)))
          (create-webhook :repo-id (getf repo :id)
                          :url url
                          :secret (unless (uiop:emptyp secret) secret)
                          :events (or events "push,pull_request,issue"))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-delete-webhook-submit
    ("/:owner/:repo-name/settings/webhooks/:webhook-id/delete" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-webhook-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-webhook-submit "Forbidden"))
      (let ((wid (parse-integer webhook-id :junk-allowed t)))
        (when wid (delete-webhook wid (getf repo :id))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-add-mirror-submit
    ("/:owner/:repo-name/settings/mirrors" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-add-mirror-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-add-mirror-submit "Forbidden"))
      (let ((direction (hunchentoot:post-parameter "direction"))
            (remote-url (hunchentoot:post-parameter "remote_url"))
            (auth-token (hunchentoot:post-parameter "auth_token"))
            (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                     :junk-allowed t)))
        (when (and direction remote-url (not (uiop:emptyp remote-url)))
          (let ((mirror (create-mirror :repo-id (getf repo :id)
                                       :direction direction
                                       :remote-url remote-url
                                       :auth-token (unless (uiop:emptyp auth-token) auth-token)
                                       :interval-minutes (or interval 60))))
            ;; Immediately sync pull mirrors
            (when (equal direction "pull")
              (let ((disk-path (repo-disk-path owner repo-name))
                    (token (unless (uiop:emptyp auth-token) auth-token)))
                (multiple-value-bind (ok err)
                    (git-pull-mirror disk-path remote-url token)
                  (update-mirror-sync (getf mirror :id)
                                      :error (unless ok err))))))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-delete-mirror-submit
    ("/:owner/:repo-name/settings/mirrors/:mirror-id/delete" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-mirror-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-mirror-submit "Forbidden"))
      (let ((mid (parse-integer mirror-id :junk-allowed t)))
        (when mid (delete-mirror mid (getf repo :id))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-add-check-submit
    ("/:owner/:repo-name/settings/checks" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-add-check-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-add-check-submit "Forbidden"))
      (let ((name (hunchentoot:post-parameter "name"))
            (command (hunchentoot:post-parameter "command"))
            (timeout (parse-integer (or (hunchentoot:post-parameter "timeout") "60")
                                    :junk-allowed t)))
        (when (and name command (not (uiop:emptyp name)) (not (uiop:emptyp command)))
          (handler-case
              (create-check-config :repo-id (getf repo :id)
                                   :name name :command command
                                   :timeout-seconds (or timeout 60))
            (error () nil))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

(easy-routes:defroute repo-delete-check-submit
    ("/:owner/:repo-name/settings/checks/:check-id/delete" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-check-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-check-submit "Forbidden"))
      (let ((cid (parse-integer check-id :junk-allowed t)))
        (when cid (delete-check-config cid (getf repo :id))))
      (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name)))))

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
        (notify-issue-created repo owner repo-name issue)
        (fire-webhooks (getf repo :id) "issue" (make-webhook-payload "issue.created" :owner owner :repo repo-name :number (getf issue :number) :title (getf issue :title)))
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
                                  :body "Reopened this issue.")))
        ;; Notify
        (let ((comment-text (or body
                                (cond ((equal action "close") "Closed this issue.")
                                      ((equal action "reopen") "Reopened this issue.")))))
          (when comment-text
            (notify-issue-comment repo owner repo-name issue comment-text)
            (fire-webhooks (getf repo :id) "issue" (make-webhook-payload "issue.comment" :owner owner :repo repo-name :number (getf issue :number))))))
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
        ;; Schedule automations
        (schedule-automations (getf repo :id) "changeset_opened"
                              :commit-sha head-commit
                              :ref source
                              :triggered-by-id *current-user-id*)
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
                                                 owner repo-name num)
                         :commit-statuses (when (getf pr :head-commit)
                                            (list-commit-statuses (getf repo :id)
                                                                  (getf pr :head-commit)))))))))

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
        (notify-pr-review repo owner repo-name pr state)
        (fire-webhooks (getf repo :id) "pull_request" (make-webhook-payload "pull_request.reviewed" :owner owner :repo repo-name :number (getf pr :number) :state state))
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
      ;; Perform the actual git merge
      (let* ((disk-path (repo-disk-path owner repo-name))
             (source (getf pr :source-branch))
             (target (getf pr :target-branch))
             (strategy (hunchentoot:post-parameter "strategy"))
             (merged (git-merge-branch disk-path source target
                                       :squash (equal strategy "squash"))))
        (unless merged
          (setf (hunchentoot:return-code*) 409)
          (return-from merge-pull-request-submit "Merge failed — conflicts?")))
      (merge-pull-request (getf pr :id))
      ;; Auto-delete source branch
      (when (getf repo :auto-delete-branch)
        (let ((source (getf pr :source-branch))
              (disk-path (repo-disk-path owner repo-name)))
          (git-delete-branch disk-path source)))
      (log-event "pr.merged"
                 :user-id *current-user-id*
                 :repo-id (getf repo :id)
                 :entity-type "pull_request"
                 :entity-id (getf pr :id))
      (notify-pr-merged repo owner repo-name pr)
      (schedule-automations (getf repo :id) "changeset_merged"
                            :commit-sha (getf pr :head-commit)
                            :ref (getf pr :source-branch)
                            :triggered-by-id *current-user-id*)
      (fire-webhooks (getf repo :id) "pull_request" (make-webhook-payload "pull_request.merged" :owner owner :repo repo-name :number (getf pr :number)))
      (hunchentoot:redirect
       (format nil "/~A/~A/pulls/~A" owner repo-name number)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Issues

(easy-routes:defroute api-list-issues
    ("/api/v1/repos/:owner/:repo-name/issues" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (json-response issues))))

(easy-routes:defroute api-create-issue
    ("/api/v1/repos/:owner/:repo-name/issues" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-issue (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
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
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-get-issue (json-error "not found" :status 404)))
      (json-response issue))))

(easy-routes:defroute api-update-issue
    ("/api/v1/repos/:owner/:repo-name/issues/:id" :method :patch) ()
  (unless *current-user-id*
    (return-from api-update-issue (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-update-issue (json-error "not found" :status 404)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             (status (gethash "status" json)))
        (when status
          (unless (member status '("open" "closed") :test #'equal)
            (return-from api-update-issue (json-error "status must be open or closed")))
          (update-issue (getf issue :id) :status status))
        (json-response (find-issue (getf repo :id) num))))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Pull Requests

(easy-routes:defroute api-list-pulls
    ("/api/v1/repos/:owner/:repo-name/pulls" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (pulls (list-pull-requests (getf repo :id) :status status)))
      (json-response pulls))))

(easy-routes:defroute api-get-pull
    ("/api/v1/repos/:owner/:repo-name/pulls/:number" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-get-pull (json-error "not found" :status 404)))
      (json-response pr))))

(easy-routes:defroute api-create-pull
    ("/api/v1/repos/:owner/:repo-name/pulls" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-pull (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (source (gethash "source_branch" json))
           (target (gethash "target_branch" json)))
      (unless (and source target)
        (return-from api-create-pull (json-error "source_branch and target_branch required")))
      (let* ((disk-path (repo-disk-path owner repo-name))
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
        (json-response pr :status 201)))))

(easy-routes:defroute api-list-reviews
    ("/api/v1/repos/:owner/:repo-name/pulls/:number/reviews" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-list-reviews (json-error "not found" :status 404)))
      (json-response (list-reviews (getf pr :id))))))

(easy-routes:defroute api-submit-review
    ("/api/v1/repos/:owner/:repo-name/pulls/:number/reviews" :method :post) ()
  (unless *current-user-id*
    (return-from api-submit-review (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-submit-review (json-error "not found" :status 404)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             (state (gethash "state" json))
             (body (gethash "body" json)))
        (unless (member state '("approve" "approve_with_concerns" "request_changes" "comment")
                        :test #'equal)
          (return-from api-submit-review (json-error "invalid state")))
        (json-response
         (create-review :changeset-id (getf pr :id)
                        :reviewer-id *current-user-id*
                        :state state
                        :body (if (eq body 'null) nil body)
                        :changeset-version (getf pr :version))
         :status 201)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Commit Statuses

(easy-routes:defroute api-list-commit-statuses
    ("/api/v1/repos/:owner/:repo-name/statuses/:sha" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (json-response (list-commit-statuses (getf repo :id) sha))))

(easy-routes:defroute api-set-commit-status
    ("/api/v1/repos/:owner/:repo-name/statuses/:sha" :method :post) ()
  (unless *current-user-id*
    (return-from api-set-commit-status (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (state (gethash "state" json))
           (context (gethash "context" json))
           (description (gethash "description" json))
           (target-url (gethash "target_url" json)))
      (unless (member state '("pending" "success" "failure" "error") :test #'equal)
        (return-from api-set-commit-status (json-error "invalid state")))
      (json-response
       (set-commit-status :repo-id (getf repo :id)
                          :commit-sha sha
                          :state state
                          :context (or context "default")
                          :description (when (and description (not (eq description 'null))) description)
                          :target-url (when (and target-url (not (eq target-url 'null))) target-url))
       :status 201))))

;; ----------------------------------------------------------------------------
;; Git smart HTTP transport (read-only, for runner clones)

(defun handle-git-http (owner repo-name)
  "Handle git smart HTTP for a repo. Dispatches based on URL suffix."
  (let* ((repo (find-repo owner repo-name))
         (disk-path (when repo (repo-disk-path owner repo-name))))
    (unless (and repo (probe-file disk-path))
      (setf (hunchentoot:return-code*) 404)
      (return-from handle-git-http "Not found"))
    (unless (repo-visible-p repo)
      (setf (hunchentoot:return-code*) 404)
      (return-from handle-git-http "Not found"))
    (let ((uri (hunchentoot:script-name*)))
      (cond
        ;; GET /owner/repo.git/info/refs?service=git-upload-pack
        ((and (search "/info/refs" uri)
              (equal (hunchentoot:request-method*) :get))
         (unless (equal (hunchentoot:get-parameter "service") "git-upload-pack")
           (setf (hunchentoot:return-code*) 403)
           (return-from handle-git-http "Forbidden"))
         (multiple-value-bind (output _err exit-code)
             (uiop:run-program (list "git" "upload-pack" "--stateless-rpc"
                                     "--advertise-refs" (namestring disk-path))
                               :output :string
                               :error-output :string
                               :ignore-error-status t)
           (declare (ignore _err))
           (unless (zerop exit-code)
             (setf (hunchentoot:return-code*) 500)
             (return-from handle-git-http "Internal error"))
           (setf (hunchentoot:content-type*)
                 "application/x-git-upload-pack-advertisement")
           (concatenate 'string
                     "001e# service=git-upload-pack" (string #\Newline)
                     "0000" output)))
        ;; POST /owner/repo.git/git-upload-pack
        ((and (search "/git-upload-pack" uri)
              (equal (hunchentoot:request-method*) :post))
         (let* ((request-body (hunchentoot:raw-post-data :force-binary t))
                (in-path (format nil "/tmp/cave-git-in-~A"
                                 (ironclad:byte-array-to-hex-string (ironclad:random-data 8))))
                (out-path (format nil "/tmp/cave-git-out-~A"
                                  (ironclad:byte-array-to-hex-string (ironclad:random-data 8)))))
           (unwind-protect
            (progn
              (with-open-file (s in-path :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :supersede)
                (write-sequence request-body s))
              (let ((exit-code (nth-value 2
                                (uiop:run-program
                                 (format nil "git upload-pack --stateless-rpc '~A' < '~A' > '~A'"
                                         (namestring disk-path) in-path out-path)
                                 :error-output :string
                                 :ignore-error-status t))))
                (unless (zerop exit-code)
                  (setf (hunchentoot:return-code*) 500)
                  (return-from handle-git-http "Internal error"))
                (setf (hunchentoot:content-type*)
                      "application/x-git-upload-pack-result")
                (with-open-file (s out-path :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence buf s)
                    buf))))
            (ignore-errors (delete-file in-path))
            (ignore-errors (delete-file out-path)))))
        (t
         (setf (hunchentoot:return-code*) 404)
         "Not found")))))

;; ----------------------------------------------------------------------------
;; Git helpers

(defun repo-disk-path (owner repo-name)
  "Return the on-disk path for a bare git repo."
  (merge-pathnames (format nil "~A/~A.git/" owner repo-name) (repos-dir)))

(defun install-repo-hooks (path owner repo-name)
  "Install pre-receive and post-receive hooks on a bare repo."
  ;; Pre-receive hook (checks)
  (let ((hook-path (merge-pathnames "hooks/pre-receive" path)))
    (ensure-directories-exist hook-path)
    (with-open-file (out hook-path :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%exec cave run-checks --config /etc/cave.conf --repo ~A/~A~%"
              owner repo-name))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Post-receive hook (calls back into running Cave server)
  (let ((hook-path (merge-pathnames "hooks/post-receive" path)))
    (with-open-file (out hook-path :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%# Pipe ref updates to the running Cave server~%tee >(curl -sf -X POST --data-binary @- http://localhost:~A/-/internal/hook/post-receive/~A/~A) >/dev/null~%"
              (config-value :http-port 8080) owner repo-name)
      (format out "cave sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
              owner repo-name)
      (when (string= repo-name "cave-themes")
        (format out "cave sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
                owner)))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Ensure the cave user owns the repo
  (uiop:run-program (list "chown" "-R" "cave:cave" (namestring path))
                     :output :string :error-output :string :ignore-error-status t))

(defun init-bare-repo (owner repo-name)
  "Initialize a bare git repository on disk with HEAD pointing to main.
   Sets ownership to cave:cave so SSH pushes work."
  (let ((path (repo-disk-path owner repo-name)))
    (ensure-directories-exist path)
    (uiop:run-program (list "git" "init" "--bare" "-b" "main" (namestring path))
                       :output :string :error-output :string)
    (install-repo-hooks path owner repo-name)
    (llog:info "Initialized bare repo" :path path)
    path))

(defun import-repo-from-url (owner repo-name url &key auth-token)
  "Clone a repo from an external URL as a bare repo, install hooks."
  (let ((path (repo-disk-path owner repo-name)))
    (multiple-value-bind (success-p err)
        (git-clone-bare-from-url url path :auth-token auth-token)
      (unless success-p
        (error "Clone failed: ~A" err))
      (install-repo-hooks path owner repo-name)
      (zoekt-index-repo owner repo-name)
      (llog:info "Imported repo from URL" :path path :url url)
      path)))

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
