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
  "Wrap every request with a pooled DB connection and auth context."
  (postmodern:with-connection *db-spec*
    (let ((*current-user* nil)
          (*current-user-id* nil))
      (authenticate-request)
      (call-next-method))))

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
     (format nil "/login?next=~A"
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

;; Visibility check macro
(defmacro check-repo-visible (repo)
  "Return 404 if repo is private and user is not a member."
  `(when (and (getf ,repo :is-private)
              (not (and *current-user-id*
                        (repo-member-role (getf ,repo :id) *current-user-id*))))
     (return-from route-handler (not-found))))

;; ----------------------------------------------------------------------------
;; Routes: Auth

(easy-routes:defroute login-page ("/login" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/")
      (html-response (view-login :next (hunchentoot:get-parameter "next")))))

(easy-routes:defroute login-submit ("/login" :method :post) ()
  (let* ((username (hunchentoot:post-parameter "username"))
         (password (hunchentoot:post-parameter "password"))
         (next-url (or (hunchentoot:post-parameter "next") "/"))
         (user (authenticate-user username password)))
    (if user
        (let ((token (create-session (getf user :id))))
          (hunchentoot:set-cookie "cave_session"
                                  :value token
                                  :path "/"
                                  :http-only t
                                  :max-age (* *session-duration-hours* 3600))
          (hunchentoot:redirect next-url))
        (html-response
         (view-login :error "Invalid username or password" :next next-url)))))

(easy-routes:defroute logout ("/logout" :method :post) ()
  (delete-session (hunchentoot:cookie-in "cave_session"))
  (hunchentoot:set-cookie "cave_session" :value "" :path "/" :max-age 0)
  (hunchentoot:redirect "/login"))

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

(easy-routes:defroute admin-create-user ("/-/admin/users" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-create-user "Forbidden"))
    (let ((username (hunchentoot:post-parameter "username"))
          (password (hunchentoot:post-parameter "password"))
          (is-admin (hunchentoot:post-parameter "is_admin")))
      (handler-case
          (progn
            (create-user :username username :password password
                         :is-admin (when is-admin t))
            (html-response
             (view-admin :users (list-users :active-only nil)
                         :message (format nil "User '~A' created." username))))
        (error (e)
          (html-response
           (view-admin :users (list-users :active-only nil)
                       :message (format nil "Error: ~A" e))))))))

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
;; Routes: Repos

(easy-routes:defroute repo-page ("/:owner/:repo-name" :method :get) ()
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from repo-page (not-found)))
    (when (and (getf repo :is-private)
               (not (and *current-user-id*
                         (repo-member-role (getf repo :id) *current-user-id*))))
      (return-from repo-page (not-found)))
    (let* ((role (and *current-user-id*
                      (repo-member-role (getf repo :id) *current-user-id*)))
           (disk-path (repo-disk-path owner repo-name))
           (empty (git-repo-empty-p disk-path))
           (branches (unless empty (git-branches disk-path)))
           (recent-commits (unless empty (git-log disk-path :limit 5)))
           (open-issues (list-issues (getf repo :id) :status "open" :limit 5))
           (open-changesets (list-changesets (getf repo :id) :status "open" :limit 5)))
      (html-response
       (view-repo :owner-name owner :repo repo :role role
                  :empty empty :branches branches
                  :recent-commits recent-commits
                  :issues open-issues :changesets open-changesets)))))

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
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from issues-page (not-found)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (html-response
       (view-issues :owner-name owner :repo repo :issues issues :current-status status)))))

(easy-routes:defroute new-issue-page
    ("/:owner/:repo-name/issues/new" :method :get) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from new-issue-page (not-found)))
      (html-response (view-new-issue :owner-name owner :repo repo)))))

(easy-routes:defroute create-issue-submit
    ("/:owner/:repo-name/issues/new" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from create-issue-submit (not-found)))
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
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from issue-page (not-found)))
    (let* ((num (parse-integer number :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from issue-page (not-found)))
      (html-response
       (view-issue :owner-name owner :repo repo :issue issue
                   :author (find-user-by-id (getf issue :author-id)))))))

;; ----------------------------------------------------------------------------
;; Routes: Changesets

(easy-routes:defroute changesets-page
    ("/:owner/:repo-name/changesets" :method :get) ()
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from changesets-page (not-found)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (changesets (list-changesets (getf repo :id) :status status)))
      (html-response
       (view-changesets :owner-name owner :repo repo :changesets changesets
                        :current-status status)))))

(easy-routes:defroute changeset-page
    ("/:owner/:repo-name/changesets/:number" :method :get) ()
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from changeset-page (not-found)))
    (let* ((num (parse-integer number :junk-allowed t))
           (changeset (when num (find-changeset (getf repo :id) num))))
      (unless changeset (return-from changeset-page (not-found)))
      (let* ((author (find-user-by-id (getf changeset :author-id)))
             (reviews-raw (list-reviews (getf changeset :id)))
             (concerns-all (list-concerns (getf changeset :id)))
             (reviews (mapcar
                       (lambda (r)
                         (append r
                          (list :is-stale (review-is-stale-p r changeset)
                                :concerns (remove-if-not
                                           (lambda (c) (= (getf c :review-id) (getf r :id)))
                                           concerns-all))))
                       reviews-raw))
             (eligibility (compute-merge-eligibility changeset repo))
             (can-merge (and (changeset-mergeable-p eligibility)
                             *current-user-id*
                             (equal (repo-member-role (getf repo :id) *current-user-id*)
                                    "admin")))
             (stack (find-stack-by-id (getf changeset :stack-id)))
             (stack-items (when stack (list-stack-changesets (getf stack :id)))))
        (html-response
         (view-changeset :owner-name owner :repo repo :changeset changeset
                         :author author :reviews reviews
                         :eligibility eligibility :can-merge can-merge
                         :stack stack :stack-items stack-items))))))

(easy-routes:defroute submit-review
    ("/:owner/:repo-name/changesets/:number/review" :method :post) ()
  (when (require-login)
    (let* ((repo (find-repo owner repo-name))
           (num (parse-integer number :junk-allowed t))
           (changeset (when (and repo num) (find-changeset (getf repo :id) num))))
      (unless changeset (return-from submit-review (not-found)))
      (let* ((state (hunchentoot:post-parameter "state"))
             (body (hunchentoot:post-parameter "body"))
             (concern-text (hunchentoot:post-parameter "concern_text"))
             (review (create-review
                      :changeset-id (getf changeset :id)
                      :reviewer-id *current-user-id*
                      :state state
                      :body (unless (uiop:emptyp body) body)
                      :changeset-version (getf changeset :version))))
        (when (and (equal state "approve_with_concerns")
                   (not (uiop:emptyp concern-text)))
          (create-concern :review-id (getf review :id)
                          :changeset-id (getf changeset :id)
                          :author-id *current-user-id*
                          :body concern-text))
        (log-event "review.submitted"
                   :user-id *current-user-id*
                   :repo-id (getf repo :id)
                   :entity-type "review"
                   :entity-id (getf review :id))
        (hunchentoot:redirect
         (format nil "/~A/~A/changesets/~A" owner repo-name number))))))

(easy-routes:defroute resolve-concern-submit
    ("/:owner/:repo-name/concerns/:concern-id/resolve" :method :post) ()
  (when (require-login)
    (let* ((cid (parse-integer concern-id :junk-allowed t))
           (concern (when cid (find-concern-by-id cid))))
      (when concern
        (let* ((repo (find-repo owner repo-name))
               (role (when repo (repo-member-role (getf repo :id) *current-user-id*))))
          (when (or (= (getf concern :author-id) *current-user-id*)
                    (equal role "admin"))
            (resolve-concern cid *current-user-id*))))
      (let ((changeset (when concern (find-changeset-by-id (getf concern :changeset-id)))))
        (hunchentoot:redirect
         (if changeset
             (format nil "/~A/~A/changesets/~A" owner repo-name (getf changeset :number))
             (format nil "/~A/~A" owner repo-name)))))))

(easy-routes:defroute merge-changeset-submit
    ("/:owner/:repo-name/changesets/:number/merge" :method :post) ()
  (when (require-login)
    (let* ((repo (find-repo owner repo-name))
           (num (parse-integer number :junk-allowed t))
           (changeset (when (and repo num) (find-changeset (getf repo :id) num))))
      (unless changeset (return-from merge-changeset-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from merge-changeset-submit "Forbidden"))
      (let ((eligibility (compute-merge-eligibility changeset repo)))
        (unless (changeset-mergeable-p eligibility)
          (setf (hunchentoot:return-code*) 422)
          (return-from merge-changeset-submit "Changeset is not mergeable")))
      (merge-changeset (getf changeset :id))
      (log-event "changeset.merged"
                 :user-id *current-user-id*
                 :repo-id (getf repo :id)
                 :entity-type "changeset"
                 :entity-id (getf changeset :id))
      (hunchentoot:redirect
       (format nil "/~A/~A/changesets/~A" owner repo-name number)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Issues

(easy-routes:defroute api-list-issues
    ("/api/v1/repos/:owner/:repo-name/issues" :method :get) ()
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from api-list-issues (json-error "not found" :status 404)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (json-response issues))))

(easy-routes:defroute api-create-issue
    ("/api/v1/repos/:owner/:repo-name/issues" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-issue (json-error "unauthorized" :status 401)))
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from api-create-issue (json-error "not found" :status 404)))
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
  (let ((repo (find-repo owner repo-name)))
    (unless repo (return-from api-get-issue (json-error "not found" :status 404)))
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
  "Initialize a bare git repository on disk."
  (let ((path (repo-disk-path owner repo-name)))
    (ensure-directories-exist path)
    (uiop:run-program (list "git" "init" "--bare" (namestring path))
                       :output :string :error-output :string)
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
