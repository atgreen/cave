;;; server.lisp — HTTP server, routing, and request handling
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Server infrastructure

(defvar *shutdown-cv* (bt2:make-condition-variable))
(defvar *server-lock* (bt2:make-lock))
(defvar *acceptor* nil)

(defclass cave-acceptor (easy-routes:easy-routes-acceptor)
  ()
  (:documentation "Cave HTTP acceptor with per-request auth."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor cave-acceptor) request)
  "Wrap every request with a pooled DB connection, auth context, and metrics."
  (let ((method (hunchentoot:request-method request))
        (start (get-internal-real-time)))
    (bt2:with-lock-held (*metrics-lock*)
      (incf *active-requests*))
    (unwind-protect
         (postmodern:with-connection *db-spec*
           (let ((*current-user* nil)
                 (*current-user-id* nil))
             (authenticate-request)
             ;; Normalize trailing slashes — easy-routes doesn't match "/foo/" to "/foo".
             ;; Skip git smart-HTTP paths and static files which can carry meaningful
             ;; trailing characters.
             (let ((uri (hunchentoot:script-name request)))
               (when (and (eq method :get)
                          (> (length uri) 1)
                          (char= (char uri (1- (length uri))) #\/)
                          (not (search ".git/" uri))
                          (not (uiop:string-prefix-p "/static/" uri)))
                 (let* ((trimmed (string-right-trim "/" uri))
                        (qs (hunchentoot:query-string request))
                        (target (if (and qs (plusp (length qs)))
                                    (format nil "~A?~A" trimmed qs)
                                    trimmed)))
                   ;; Path-only Location so the browser preserves the original
                   ;; scheme — avoids an http→https extra hop behind Caddy.
                   (setf (hunchentoot:header-out :location) target
                         (hunchentoot:return-code*) 301)
                   (return-from hunchentoot:acceptor-dispatch-request "")))
               ;; Intercept git smart HTTP before easy-routes dispatch
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
                 (return-from hunchentoot:acceptor-dispatch-request nil))
               ;; Page-view tracking — log a row for GET hits on repo subpaths
               ;; (skip POSTs, static, hooks, smart-HTTP). Cheap and fire-and-forget.
               (when (eq method :get)
                 (handler-case (maybe-log-page-view uri request)
                   (error () nil))))
             (call-next-method)))
      (let* ((elapsed (/ (- (get-internal-real-time) start)
                         (float internal-time-units-per-second 1.0d0)))
             (status (hunchentoot:return-code*)))
        (bt2:with-lock-held (*metrics-lock*)
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

(defun visitor-ip-hash (request)
  "SHA-256 hash of remote IP salted with :secret-key. Raw IP never stored."
  (let* ((fwd (hunchentoot:header-in :x-forwarded-for request))
         (ip (or (and fwd (let ((c (position #\, fwd)))
                            (string-trim " " (if c (subseq fwd 0 c) fwd))))
                 (hunchentoot:remote-addr request)))
         (salt (or (config-value :secret-key) "cave"))
         (bytes (flexi-streams:string-to-octets
                 (concatenate 'string ip ":" salt)
                 :external-format :utf-8)))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha256 bytes))))

(defun referer-host (request)
  "Just the hostname from the Referer header — drop scheme/path/query.
   NIL when no referer, or when the referer is this same host."
  (let ((r (hunchentoot:header-in :referer request)))
    (when (and r (plusp (length r)))
      (let ((stripped (cond
                        ((uiop:string-prefix-p "https://" r) (subseq r 8))
                        ((uiop:string-prefix-p "http://" r)  (subseq r 7))
                        (t r))))
        (let* ((slash (position #\/ stripped))
               (host (if slash (subseq stripped 0 slash) stripped))
               (colon (position #\: host))
               (host (if colon (subseq host 0 colon) host))
               (self (and (config-value :base-url)
                          (let ((bu (string-right-trim "/" (config-value :base-url))))
                            (cond
                              ((uiop:string-prefix-p "https://" bu) (subseq bu 8))
                              ((uiop:string-prefix-p "http://" bu)  (subseq bu 7))
                              (t bu))))))
          (unless (and self (string-equal host self)) host))))))

(defun maybe-log-page-view (uri request)
  "Log a page view if URI looks like a repo subpath (owner/repo[/...])."
  ;; Skip uninteresting / internal URIs early
  (when (or (uiop:string-prefix-p "/-/" uri)
            (uiop:string-prefix-p "/static/" uri)
            (uiop:string-prefix-p "/u/" uri)
            (uiop:string-prefix-p "/o/" uri)
            (search ".git/" uri)
            (search "/-/internal/" uri))
    (return-from maybe-log-page-view nil))
  (let* ((trimmed (string-trim "/" uri))
         (parts (uiop:split-string trimmed :separator '(#\/))))
    ;; Need at least owner/repo
    (when (and (>= (length parts) 2)
               (plusp (length (first parts)))
               (plusp (length (second parts))))
      (let ((repo (find-repo (first parts) (second parts))))
        (when (and repo (repo-visible-p repo))
          (log-page-view (getf repo :id)
                         :ip-hash (visitor-ip-hash request)
                         :user-id *current-user-id*
                         :referer-host (referer-host request)))))))

(defun zero-sha-p (sha)
  (and sha (every (lambda (c) (char= c #\0)) sha)))

(defun push-commit-count (disk-path old new)
  "How many commits this push adds. NIL on failure."
  (multiple-value-bind (out _err exit)
      (if (zero-sha-p old)
          (git-run disk-path "rev-list" "--count" new)
          (git-run disk-path "rev-list" "--count" (format nil "~A..~A" old new)))
    (declare (ignore _err))
    (when (zerop exit) (parse-integer out :junk-allowed t))))

(defun push-tip-subject (disk-path new)
  "First line of NEW commit's message. NIL if NEW is a deletion or unreadable."
  (unless (zero-sha-p new)
    (multiple-value-bind (out _err exit)
        (git-run disk-path "log" "-1" "--format=%s" new)
      (declare (ignore _err))
      (when (zerop exit) (string-trim '(#\Space #\Newline #\Tab) out)))))

(defun build-push-metadata (disk-path ref old new)
  "Hash-table jzon will serialize as a JSON object for the event's metadata column."
  (let ((md (make-hash-table :test 'equal))
        (created (zero-sha-p old))
        (deleted (zero-sha-p new)))
    (setf (gethash "ref" md) ref
          (gethash "old" md) old
          (gethash "new" md) new)
    (when created (setf (gethash "created" md) t))
    (when deleted (setf (gethash "deleted" md) t))
    (unless deleted
      (let ((count (push-commit-count disk-path old new)))
        (when count (setf (gethash "count" md) count))))
    (let ((tip (push-tip-subject disk-path new)))
      (when tip (setf (gethash "tip" md) tip)))
    md))

(defun ensure-allowed-signers-file ()
  "Materialize an allowed_signers file from every SSH key in cave's DB.
Cached in /tmp, rewritten when the registered-key fingerprint set changes.
Returns the path."
  (let* ((path (merge-pathnames "cave-allowed-signers" (uiop:temporary-directory)))
         (keys (all-ssh-keys-with-user))
         (entries (loop for k in keys
                        collect (list (or (getf k :email)
                                          (getf k :username)
                                          (format nil "user-~A" (getf k :user-id)))
                                      (getf k :public-key)))))
    (write-allowed-signers entries path)
    path))

(defun verify-pushed-commits (owner-name repo disk-path refs)
  "For each ref update, verify newly-introduced commits' signatures and cache
the results. Skips deletes and zero-sha boundaries."
  (declare (ignore owner-name))
  (let* ((shas (loop for r in refs
                     when (and (not (zero-sha-p (getf r :new))))
                       append
                       (let ((range (if (zero-sha-p (getf r :old))
                                        (list (getf r :new))
                                        (multiple-value-bind (out _err exit)
                                            (git-run disk-path "rev-list"
                                                     (format nil "~A..~A"
                                                             (getf r :old) (getf r :new)))
                                          (declare (ignore _err))
                                          (if (zerop exit)
                                              (remove-if #'uiop:emptyp
                                                         (uiop:split-string
                                                          out :separator '(#\Newline)))
                                              nil)))))
                         range)))
         (shas (remove-duplicates shas :test #'equal)))
    (when shas
      (let ((signers (ensure-allowed-signers-file))
            (key->user (let ((h (make-hash-table :test 'equal)))
                         (dolist (k (all-ssh-keys-with-user))
                           (setf (gethash (getf k :fingerprint) h) (getf k :user-id)))
                         h)))
        (dolist (sha shas)
          (multiple-value-bind (signed scheme)
              (git-commit-signature-info disk-path sha)
            (cond
              ((not signed)
               (record-commit-signature :repo-id (getf repo :id)
                                        :commit-sha sha :verified nil :scheme nil))
              ((eq scheme :ssh)
               (let* ((verified (git-verify-commit disk-path sha signers))
                      (fp (git-commit-signature-key disk-path sha)))
                 (record-commit-signature :repo-id (getf repo :id)
                                          :commit-sha sha :verified verified
                                          :scheme "ssh" :fingerprint fp
                                          :signer-user-id (gethash fp key->user))))
              (t
               ;; GPG: we don't verify (no GPG key store yet) — record as unsigned
               (record-commit-signature :repo-id (getf repo :id)
                                        :commit-sha sha :verified nil
                                        :scheme "gpg")))))))))

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
    (let ((actor (let ((a (hunchentoot:get-parameter "actor")))
                   (when (and a (plusp (length a)))
                     (parse-integer a :junk-allowed t)))))
      ;; Parse refs from POST body (one per line: oldsha newsha refname)
      (let ((body (hunchentoot:raw-post-data :force-text t))
            (refs nil))
        (dolist (line (uiop:split-string body :separator '(#\Newline)))
          (let ((parts (uiop:split-string line :separator '(#\Space))))
            (when (>= (length parts) 3)
              (push (list :old (first parts) :new (second parts) :ref (third parts))
                    refs))))
        (when refs
          (touch-repo-pushed-at (getf repo :id)))
        ;; Log a rich git.push event per ref + schedule automations
        (let ((disk-path (repo-disk-path owner repo-name)))
          (dolist (r refs)
            (log-event "git.push"
                       :user-id actor
                       :repo-id (getf repo :id)
                       :metadata (build-push-metadata disk-path
                                                      (getf r :ref)
                                                      (getf r :old)
                                                      (getf r :new)))
            (schedule-automations (getf repo :id) "post_receive"
                                  :commit-sha (getf r :new)
                                  :ref (getf r :ref))
            (handler-case
                (parse-and-schedule-workflows (getf repo :id) "post_receive"
                                              :commit-sha (getf r :new)
                                              :ref (getf r :ref))
              (error (e)
                (llog:error "Workflow scheduling failed" :error (princ-to-string e))))))
        ;; Verify any signed commits in the pushed range, cache results
        (handler-case (verify-pushed-commits owner repo (repo-disk-path owner repo-name) refs)
          (error (e)
            (llog:warn "Signature verification failed" :error (princ-to-string e))))
        ;; Invalidate Chamber cache for this repo
        (chamber-invalidate-repo owner repo-name)
        (when (multi-chamber-p)
          (broadcast-invalidate-cache owner repo-name))
        ;; Trigger Zoekt reindexing
        (zoekt-index-repo owner repo-name)
        ;; Fire webhooks
        (dolist (r refs)
          (fire-webhooks (getf repo :id) "push"
                         `(("ref" . ,(getf r :ref))
                           ("after" . ,(getf r :new))
                           ("before" . ,(getf r :old))
                           ("repository" . (("owner" . ,owner)
                                            ("name" . ,repo-name))))))))
    "ok"))

;; ----------------------------------------------------------------------------
;; Push lock-bracket: SSH pushes acquire/release Chamber write locks via HTTP

(defvar *active-push-tokens* (make-hash-table :test 'equal))
(defvar *active-push-tokens-lock* (bt2:make-lock :name "push-tokens"))

(defun start-push-lock-reaper ()
  "Background thread that reaps orphaned push locks (SSH disconnect, timeout)."
  (bt2:make-thread
   (lambda ()
     (loop
       (sleep 60)
       (let ((now (get-universal-time))
             (max-age 600)
             (expired nil))
         (bt2:with-lock-held (*active-push-tokens-lock*)
           (maphash (lambda (token entry)
                      (when (> (- now (getf entry :time)) max-age)
                        (push (cons token entry) expired)))
                    *active-push-tokens*)
           (dolist (pair expired)
             (let ((token (car pair))
                   (entry (cdr pair)))
               (llog:warn "Reaping orphaned push lock"
                          :repo (format nil "~A/~A" (getf entry :owner) (getf entry :repo))
                          :age-seconds (- now (getf entry :time)))
               (chamber-invalidate-repo (getf entry :owner) (getf entry :repo))
               (when (multi-chamber-p)
                 (broadcast-invalidate-cache (getf entry :owner) (getf entry :repo)))
               (handler-case (bt2:signal-semaphore (getf entry :sema)) (error () nil))
               (handler-case (bt2:signal-semaphore *chamber-semaphore*) (error () nil))
               (remhash token *active-push-tokens*)))))))
   :name "push-lock-reaper"))

(easy-routes:defroute internal-push-acquire
    ("/-/internal/push/acquire/:owner/:repo-name" :method :post) ()
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-push-acquire "Forbidden"))
  (ensure-chamber-semaphore)
  (unless (bt2:wait-on-semaphore *chamber-semaphore* :timeout 30)
    (setf (hunchentoot:return-code*) 503)
    (return-from internal-push-acquire "Busy"))
  (let ((sema (get-repo-write-sema (format nil "~A/~A" owner repo-name))))
    (unless (bt2:wait-on-semaphore sema :timeout 30)
      (bt2:signal-semaphore *chamber-semaphore*)
      (setf (hunchentoot:return-code*) 503)
      (return-from internal-push-acquire "Busy"))
    (let ((token (fuuid:make-v4-string)))
      (bt2:with-lock-held (*active-push-tokens-lock*)
        (setf (gethash token *active-push-tokens*)
              (list :owner owner :repo repo-name
                    :time (get-universal-time) :sema sema)))
      (llog:info "Push lock acquired" :repo (format nil "~A/~A" owner repo-name))
      token)))

(easy-routes:defroute internal-push-release
    ("/-/internal/push/release/:owner/:repo-name" :method :post) ()
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-push-release "Forbidden"))
  (let* ((token (string-trim '(#\Space #\Newline #\Return)
                              (hunchentoot:raw-post-data :force-text t)))
         (entry (bt2:with-lock-held (*active-push-tokens-lock*)
                  (gethash token *active-push-tokens*))))
    (unless entry
      (setf (hunchentoot:return-code*) 404)
      (return-from internal-push-release "Unknown token"))
    (chamber-invalidate-repo owner repo-name)
    (when (multi-chamber-p)
      (broadcast-invalidate-cache owner repo-name))
    (handler-case (bt2:signal-semaphore (getf entry :sema)) (error () nil))
    (handler-case (bt2:signal-semaphore *chamber-semaphore*) (error () nil))
    (bt2:with-lock-held (*active-push-tokens-lock*)
      (remhash token *active-push-tokens*))
    (llog:info "Push lock released" :repo (format nil "~A/~A" owner repo-name))
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
          ;; Approval gate. Pending users see a friendly waiting page and
          ;; no session is established; rejected users get a final notice.
          (let ((status (getf user :approval-status)))
            (cond
              ((string= status "pending")
               (return-from oidc-callback
                 (html-response (view-account-pending :username (getf user :username)))))
              ((string= status "rejected")
               (setf (hunchentoot:return-code*) 403)
               (return-from oidc-callback
                 (html-response (view-account-rejected :username (getf user :username)))))))
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
      (html-response
       (view-public-landing :repos (list-public-repos :limit 50)
                            :events (list-recent-public-events :limit 20)))))

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
                               :pending-users (list-pending-users)
                               :runners (list-runners)))))

(easy-routes:defroute admin-create-runner-token ("/-/admin/runners/token" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-create-runner-token "Forbidden"))
    (let ((token (create-registration-token :created-by-id *current-user-id*)))
      (html-response (view-admin :users (list-users :active-only nil)
                                 :pending-users (list-pending-users)
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

(easy-routes:defroute admin-approve-user ("/-/admin/users/:user-id/approve" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-approve-user "Forbidden"))
    (let ((uid (parse-integer user-id :junk-allowed t)))
      (when uid (set-user-approval uid "approved")))
    (hunchentoot:redirect "/-/admin")))

(easy-routes:defroute admin-reject-user ("/-/admin/users/:user-id/reject" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-reject-user "Forbidden"))
    (let ((uid (parse-integer user-id :junk-allowed t)))
      (when uid (set-user-approval uid "rejected")))
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

(easy-routes:defroute download-cli ("/-/downloads/cave" :method :get) ()
  (let ((path (cli-download-path)))
    (unless path
      (setf (hunchentoot:return-code*) 404)
      (return-from download-cli "cave CLI is not installed on this host"))
    (setf (hunchentoot:header-out "Content-Disposition")
          "attachment; filename=\"cave\"")
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
    (let* ((empty (chamber-is-empty owner repo-name))
           (default-branch (unless empty (or (chamber-get-default-branch owner repo-name) "main")))
           (readme-entry (unless empty (chamber-find-readme owner repo-name :ref default-branch)))
           (raw-base-url (when readme-entry
                           (format nil "~A/~A/~A/raw/~A?path="
                                   (config-value :base-url "http://localhost:8080")
                                   owner repo-name
                                   (or default-branch "HEAD"))))
           ;; Cheap pre-check: lookup the README's blob sha via get-blob-info,
           ;; then consult the rendered-HTML cache before we ever read or render.
           (readme-info (when readme-entry
                          (chamber-get-blob-info owner repo-name default-branch
                                                 (getf readme-entry :name))))
           (cache-key (when readme-info
                        (cons (getf readme-info :hash) raw-base-url)))
           (cached-html (when cache-key (readme-cache-get cache-key)))
           (readme-html
            (or cached-html
                (let ((content (when readme-entry
                                 (chamber-get-blob owner repo-name default-branch
                                                   (getf readme-entry :name)))))
                  (when content
                    (let ((html (if (search ".md" (string-downcase
                                                   (getf readme-entry :name)))
                                    (render-markdown content :raw-base-url raw-base-url)
                                    (format nil "<pre>~A</pre>"
                                            (spinneret::escape-string content)))))
                      (when cache-key (readme-cache-put cache-key html))
                      html))))))
      (html-response
       (view-repo :owner-name owner :repo repo :empty empty
                  :default-branch default-branch
                  :readme-html readme-html
                  :readme-filename (when readme-entry (getf readme-entry :name)))))))

;; Code (file browser)
(easy-routes:defroute code-page ("/:owner/:repo-name/code" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from code-page repo))
    (let* ((empty (chamber-is-empty owner repo-name))
           (default-branch (unless empty (or (chamber-get-default-branch owner repo-name) "main")))
           (branches (unless empty (chamber-get-branches owner repo-name)))
           (tags (unless empty (chamber-get-tags owner repo-name)))
           (commit-count (unless empty (chamber-get-commit-count owner repo-name :branch default-branch)))
           (file-tree (unless empty (chamber-get-tree owner repo-name :ref default-branch)))
           (recent-commits (unless empty (chamber-get-log owner repo-name :limit 10))))
      (if empty
          (hunchentoot:redirect (format nil "/~A/~A" owner repo-name))
          (html-response
           (view-code :owner-name owner :repo repo
                      :branches branches :tags tags
                      :default-branch default-branch
                      :commit-count commit-count
                      :recent-commits recent-commits
                      :signatures (commit-signatures-by-sha
                                   (getf repo :id)
                                   (mapcar (lambda (c) (getf c :hash)) recent-commits))
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
                (format out "#!/bin/bash~%exec cave-server run-checks --config /etc/cave.conf --repo ~A/~A~%"
                        username repo-name))
              (uiop:run-program (list "chmod" "+x" (namestring pre-hook))
                                 :ignore-error-status t))
            (let ((post-hook (merge-pathnames "hooks/post-receive" dest-path)))
              (with-open-file (out post-hook :direction :output :if-exists :supersede)
                (format out "#!/bin/bash~%cave-server sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
                        username repo-name)
                (when (string= repo-name "cave-themes")
                  (format out "cave-server sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
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
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (file-tree (chamber-get-tree owner repo-name :ref ref :path path)))
      (html-response
       (view-tree :owner-name owner :repo repo :ref ref
                  :path path :file-tree file-tree)))))

;; Blob (file) viewing
(easy-routes:defroute blob-page ("/:owner/:repo-name/blob/:ref" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from blob-page repo))
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (info (chamber-get-blob-info owner repo-name ref path)))
      ;; If not found, might be a directory — redirect to tree view
      (unless info
        (let ((tree (chamber-get-tree owner repo-name :ref ref :path path)))
          (if tree
              (progn
                (hunchentoot:redirect
                 (format nil "/~A/~A/tree/~A?path=~A" owner repo-name ref path))
                (return-from blob-page nil))
              (return-from blob-page (not-found)))))
      (let* ((file-size (getf info :size))
             (is-binary (getf info :is-binary))
             (content (when (and (not is-binary) (<= file-size (* 2 1024 1024)))
                        (chamber-get-blob owner repo-name ref path)))
             (language (file-language path)))
        (html-response
         (view-blob :owner-name owner :repo repo :ref ref :path path
                    :content content
                    :is-binary is-binary
                    :file-size file-size
                    :language language))))))

(defun raw-mime-type (path)
  "Guess MIME type from file extension."
  (let ((ext (string-downcase (or (pathname-type (pathname path)) ""))))
    (cond
      ((member ext '("png") :test #'string=) "image/png")
      ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
      ((member ext '("gif") :test #'string=) "image/gif")
      ((member ext '("svg") :test #'string=) "image/svg+xml")
      ((member ext '("webp") :test #'string=) "image/webp")
      ((member ext '("ico") :test #'string=) "image/x-icon")
      ((member ext '("pdf") :test #'string=) "application/pdf")
      ((member ext '("zip" "gz" "tar" "bz2" "xz") :test #'string=) "application/octet-stream")
      (t "text/plain; charset=utf-8"))))

;; Raw file blob cache — keyed by git object SHA, content-addressable
(defvar *blob-cache* (make-hash-table :test 'equal)
  "In-memory LRU cache for raw file blobs. Key: git SHA, Value: (content . access-time).")
(defvar *blob-cache-lock* (bt2:make-lock :name "blob-cache"))
(defvar *blob-cache-max-bytes* (* 64 1024 1024) "Max cache size in bytes (64MB).")
(defvar *blob-cache-bytes* 0 "Current cache size in bytes.")

;; Rendered-README cache — content-addressable by (sha + raw-base-url). The base-url
;; participates because render-markdown rewrites relative <img src> using it, so the
;; same README on two different deploys must render to two different strings.
(defvar *readme-cache* (make-hash-table :test 'equal))
(defvar *readme-cache-lock* (bt2:make-lock :name "readme-cache"))
(defparameter *readme-cache-max* 256
  "Max entries in *readme-cache*. Beyond this, we evict at random.")

(defun readme-cache-get (key)
  (bt2:with-lock-held (*readme-cache-lock*)
    (gethash key *readme-cache*)))

(defun readme-cache-put (key html)
  (bt2:with-lock-held (*readme-cache-lock*)
    (when (>= (hash-table-count *readme-cache*) *readme-cache-max*)
      ;; Random eviction — no LRU bookkeeping. Cheap enough at this size.
      (let ((victim (loop for k being the hash-keys of *readme-cache* return k)))
        (when victim (remhash victim *readme-cache*))))
    (setf (gethash key *readme-cache*) html))
  html)

(defun blob-cache-get (sha)
  "Get cached blob by SHA. Returns content or NIL."
  (bt2:with-lock-held (*blob-cache-lock*)
    (let ((entry (gethash sha *blob-cache*)))
      (when entry
        (setf (cdr entry) (get-universal-time))
        (car entry)))))

(defun blob-cache-put (sha content)
  "Cache blob content by SHA. Evicts oldest entries if over size limit."
  (let ((size (if (stringp content) (length content)
                  (length content))))
    (when (> size (* 4 1024 1024)) ; don't cache blobs > 4MB
      (return-from blob-cache-put content))
    (bt2:with-lock-held (*blob-cache-lock*)
      ;; Evict oldest entries if needed
      (loop while (> (+ *blob-cache-bytes* size) *blob-cache-max-bytes*)
            do (let ((oldest-key nil) (oldest-time (get-universal-time)))
                 (maphash (lambda (k v)
                            (when (< (cdr v) oldest-time)
                              (setf oldest-key k oldest-time (cdr v))))
                          *blob-cache*)
                 (if oldest-key
                     (let ((old (gethash oldest-key *blob-cache*)))
                       (decf *blob-cache-bytes*
                             (if (stringp (car old)) (length (car old)) (length (car old))))
                       (remhash oldest-key *blob-cache*))
                     (return))))
      (setf (gethash sha *blob-cache*) (cons content (get-universal-time)))
      (incf *blob-cache-bytes* size)))
  content)

;; Raw file content
(easy-routes:defroute raw-page ("/:owner/:repo-name/raw/:ref" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from raw-page repo))
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (mime (raw-mime-type path))
           (info (chamber-get-blob-info owner repo-name ref path)))
      (unless info (return-from raw-page (not-found)))
      (let ((sha (getf info :hash)))
        (setf (hunchentoot:content-type*) mime)
        (setf (hunchentoot:header-out :etag) (format nil "\"~A\"" sha))
        (setf (hunchentoot:header-out :cache-control) "public, max-age=300")
        ;; 304 Not Modified if browser has current version
        (let ((if-none-match (hunchentoot:header-in* :if-none-match)))
          (when (and if-none-match (string= if-none-match (format nil "\"~A\"" sha)))
            (setf (hunchentoot:return-code*) 304)
            (return-from raw-page "")))
        ;; Check server-side blob cache
        (let ((cached (blob-cache-get sha)))
          (when cached
            (if (stringp cached)
                (return-from raw-page cached)
                ;; Binary cached data: write directly to output stream
                (progn
                  (setf (hunchentoot:content-length*) (length cached))
                  (let ((out (hunchentoot:send-headers)))
                    (write-sequence cached out)
                    (finish-output out))
                  (return-from raw-page nil)))))
        ;; Cache miss — read via Chamber
        (if (uiop:string-prefix-p "text/" mime)
            ;; Text: return as string (Hunchentoot handles natively)
            (let ((content (chamber-get-blob owner repo-name ref path)))
              (unless content (return-from raw-page (not-found)))
              (blob-cache-put sha content))
            ;; Binary: read directly via git (bypass gRPC for large binary blobs)
            ;; and write directly to output stream (Hunchentoot pattern)
            (let ((content (git-blob-bytes (repo-disk-path owner repo-name) ref path)))
              (unless content (return-from raw-page (not-found)))
              (blob-cache-put sha content)
              (setf (hunchentoot:content-length*) (length content))
              (let ((out (hunchentoot:send-headers)))
                (write-sequence content out)
                (finish-output out))))))))

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
         (let ((result (chamber-get-commit owner repo-name clean-hash)))
           (unless result (return-from commit-page (not-found)))
           (html-response
            (view-commit :owner-name owner :repo repo
                         :commit (getf result :commit)
                         :signature (find-commit-signature (getf repo :id) clean-hash)
                         :trailers (git-commit-trailers disk-path clean-hash)
                         :diff-raw (getf result :diff)))))))))

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

(easy-routes:defroute pulse-page ("/:owner/:repo-name/pulse" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from pulse-page repo))
    ;; Pulse is owner/member-only — it exposes referrers, visitor counts,
    ;; and per-contributor activity that the public doesn't need to see.
    (unless (and *current-user-id*
                 (repo-member-role (getf repo :id) *current-user-id*))
      (return-from pulse-page (not-found)))
    (let ((repo-id (getf repo :id))
          (days 14))
      (html-response
       (view-pulse :owner-name owner :repo repo
                   :days days
                   :event-counts (repo-event-counts-by-day repo-id :days days)
                   :contributors (repo-top-contributors repo-id :days days :limit 5)
                   :views (repo-page-views-by-day repo-id :days days)
                   :unique-visitors (repo-unique-visitors-by-day repo-id :days days)
                   :referrers (repo-top-referrers repo-id :days days :limit 10))))))

;; ----------------------------------------------------------------------------
;; Routes: Releases

(defparameter *release-asset-max-bytes* (* 100 1024 1024)
  "Per-asset upload cap. 100 MB.")

(defun release-asset-dir (repo-id release-id)
  "Absolute path to a release's asset directory. Created on demand."
  (let ((dir (merge-pathnames (format nil "releases/~A/~A/" repo-id release-id)
                              (data-dir))))
    (ensure-directories-exist dir)
    dir))

(defun sanitize-asset-filename (name)
  "Strip path components and disallowed chars from an uploaded filename."
  (let* ((bare (file-namestring (or name "asset")))
         (clean (with-output-to-string (s)
                  (loop for c across bare
                        do (write-char
                            (if (or (alphanumericp c)
                                    (find c ".-_+" :test #'char=))
                                c #\_)
                            s)))))
    (if (zerop (length clean)) "asset" clean)))

(defun member-of-repo-p (repo)
  (and *current-user-id* (repo-member-role (getf repo :id) *current-user-id*)))

(easy-routes:defroute releases-page ("/:owner/:repo-name/releases" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from releases-page repo))
    (let* ((releases (list-releases (getf repo :id)))
           (assets-by-release
            (let ((h (make-hash-table)))
              (dolist (r releases)
                (setf (gethash (getf r :id) h)
                      (list-release-assets (getf r :id))))
              h)))
      (html-response
       (view-releases :owner-name owner :repo repo
                      :releases releases
                      :assets-by-release assets-by-release
                      :can-create (and (member-of-repo-p repo) t))))))

(easy-routes:defroute new-release-page ("/:owner/:repo-name/releases/new" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from new-release-page repo))
    (unless (member-of-repo-p repo)
      (return-from new-release-page (not-found)))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (existing-tags (git-tags disk-path)))
      (html-response
       (view-new-release :owner-name owner :repo repo :existing-tags existing-tags)))))

(easy-routes:defroute create-release-submit ("/:owner/:repo-name/releases/new" :method :post) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from create-release-submit repo))
    (unless (member-of-repo-p repo)
      (return-from create-release-submit (not-found)))
    (let* ((tag-name (string-trim '(#\Space) (or (hunchentoot:post-parameter "tag_name") "")))
           (release-name (hunchentoot:post-parameter "name"))
           (body (or (hunchentoot:post-parameter "body") ""))
           (is-prerelease (equal (hunchentoot:post-parameter "is_prerelease") "1"))
           (disk-path (repo-disk-path owner repo-name))
           (default-branch (or (chamber-get-default-branch owner repo-name) "main")))
      (when (zerop (length tag-name))
        (return-from create-release-submit
          (html-response (view-new-release :owner-name owner :repo repo
                                            :existing-tags (git-tags disk-path)
                                            :error "Tag name is required."))))
      (when (find-release-by-tag (getf repo :id) tag-name)
        (return-from create-release-submit
          (html-response (view-new-release :owner-name owner :repo repo
                                            :existing-tags (git-tags disk-path)
                                            :error (format nil "Release ~A already exists." tag-name)))))
      ;; If the tag doesn't exist in git, create it at HEAD of the default branch.
      (unless (git-tag-exists-p disk-path tag-name)
        (unless (git-create-tag disk-path tag-name default-branch
                                :message (or release-name tag-name))
          (return-from create-release-submit
            (html-response (view-new-release :owner-name owner :repo repo
                                              :existing-tags (git-tags disk-path)
                                              :error (format nil "Could not create git tag ~A." tag-name))))))
      (create-release :repo-id (getf repo :id)
                      :tag-name tag-name
                      :name release-name
                      :body body
                      :is-prerelease is-prerelease
                      :created-by *current-user-id*)
      (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag-name)))))

(easy-routes:defroute release-detail-page ("/:owner/:repo-name/releases/:tag" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from release-detail-page repo))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from release-detail-page (not-found)))
      (html-response
       (view-release :owner-name owner :repo repo
                     :release release
                     :assets (list-release-assets (getf release :id))
                     :can-edit (and (member-of-repo-p repo) t))))))

(easy-routes:defroute delete-release-submit
    ("/:owner/:repo-name/releases/:tag/delete" :method :post) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from delete-release-submit repo))
    (unless (member-of-repo-p repo)
      (return-from delete-release-submit (not-found)))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (when release
        ;; Wipe assets on disk before the DB cascades the rows.
        (let ((dir (release-asset-dir (getf repo :id) (getf release :id))))
          (handler-case (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
            (error () nil)))
        (delete-release (getf release :id))))
    (hunchentoot:redirect (format nil "/~A/~A/releases" owner repo-name))))

(easy-routes:defroute upload-release-asset
    ("/:owner/:repo-name/releases/:tag/upload" :method :post) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from upload-release-asset repo))
    (unless (member-of-repo-p repo)
      (return-from upload-release-asset (not-found)))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from upload-release-asset (not-found)))
      ;; Reject early on Content-Length if available.
      (let ((cl (hunchentoot:header-in* :content-length)))
        (when (and cl (> (parse-integer cl :junk-allowed t) *release-asset-max-bytes*))
          (setf (hunchentoot:return-code*) 413)
          (return-from upload-release-asset "Asset too large.")))
      (let ((upload (hunchentoot:post-parameter "asset")))
        (unless (consp upload)
          (return-from upload-release-asset
            (progn (setf (hunchentoot:return-code*) 400) "No file in upload.")))
        (let* ((temp-path (first upload))
               (orig-name (second upload))
               (content-type (third upload))
               (clean-name (sanitize-asset-filename orig-name))
               (size (with-open-file (s temp-path :element-type '(unsigned-byte 8))
                       (file-length s))))
          (when (> size *release-asset-max-bytes*)
            (ignore-errors (delete-file temp-path))
            (setf (hunchentoot:return-code*) 413)
            (return-from upload-release-asset "Asset too large."))
          (when (find-release-asset-by-name (getf release :id) clean-name)
            (ignore-errors (delete-file temp-path))
            (setf (hunchentoot:return-code*) 409)
            (return-from upload-release-asset "An asset with that name already exists."))
          (let* ((dir (release-asset-dir (getf repo :id) (getf release :id)))
                 (dest (merge-pathnames clean-name dir)))
            (uiop:rename-file-overwriting-target temp-path dest)
            (create-release-asset :release-id (getf release :id)
                                  :name clean-name
                                  :content-type content-type
                                  :size size
                                  :storage-path (namestring dest)
                                  :uploaded-by *current-user-id*))))
      (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag)))))

(easy-routes:defroute delete-release-asset-submit
    ("/:owner/:repo-name/releases/:tag/assets/:asset-id/delete" :method :post) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from delete-release-asset-submit repo))
    (unless (member-of-repo-p repo)
      (return-from delete-release-asset-submit (not-found)))
    (let* ((aid (parse-integer asset-id :junk-allowed t))
           (asset (when aid (find-release-asset-by-id aid))))
      (when asset
        (ignore-errors (delete-file (getf asset :storage-path)))
        (delete-release-asset aid)))
    (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag))))

(easy-routes:defroute release-asset-download
    ("/:owner/:repo-name/releases/download/:tag/:filename" :method :get) ()
  (let ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found)))
    (unless repo (return-from release-asset-download repo))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from release-asset-download (not-found)))
      (let ((asset (find-release-asset-by-name (getf release :id) filename)))
        (unless (and asset (probe-file (getf asset :storage-path)))
          (return-from release-asset-download (not-found)))
        (increment-asset-download-count (getf asset :id))
        (setf (hunchentoot:content-type*) (or (getf asset :content-type)
                                              "application/octet-stream"))
        (setf (hunchentoot:header-out :content-disposition)
              (format nil "attachment; filename=\"~A\"" (getf asset :name)))
        (setf (hunchentoot:header-out :content-length) (princ-to-string (getf asset :size)))
        (let ((out (hunchentoot:send-headers)))
          (with-open-file (in (getf asset :storage-path) :element-type '(unsigned-byte 8))
            (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
              (loop for n = (read-sequence buf in)
                    while (plusp n)
                    do (write-sequence buf out :end n))))
          (finish-output out))
        nil))))

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
              (let ((token (unless (uiop:emptyp auth-token) auth-token)))
                (multiple-value-bind (ok err)
                    (chamber-pull-mirror owner repo-name remote-url token)
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
      (let* ((branches (chamber-get-branches owner repo-name))
             (default-branch (or (chamber-get-default-branch owner repo-name) "main")))
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
             (source (getf pr :source-branch))
             (target (getf pr :target-branch))
             (diff-raw (chamber-get-diff-merge-base owner repo-name target source))
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
      (let* ((source (getf pr :source-branch))
             (target (getf pr :target-branch))
             (strategy (hunchentoot:post-parameter "strategy"))
             (merged (chamber-merge-branch owner repo-name source target
                                           :squash (equal strategy "squash"))))
        (unless merged
          (setf (hunchentoot:return-code*) 409)
          (return-from merge-pull-request-submit "Merge failed — conflicts?")))
      (merge-pull-request (getf pr :id))
      ;; Auto-delete source branch
      (when (getf repo :auto-delete-branch)
        (chamber-delete-branch owner repo-name (getf pr :source-branch)))
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
;; Routes: API v1 — Repos

(easy-routes:defroute api-create-personal-repo
    ("/api/v1/user/repos" :method :post) ()
  "Create a personal repo for the authenticated user. Mirrors the HTML
   /-/new-repo flow: mode is one of 'empty' (default), 'import' (one-shot
   clone of remote URL), or 'mirror' (import + ongoing pull mirror)."
  (unless *current-user-id*
    (return-from api-create-personal-repo (json-error "unauthorized" :status 401)))
  (let* ((body-text (hunchentoot:raw-post-data :force-text t))
         (json (com.inuoe.jzon:parse (or body-text "{}")))
         (raw-name (gethash "name" json))
         (description (gethash "description" json))
         (is-private (gethash "private" json))
         (mode (or (gethash "mode" json) "empty"))
         (url (gethash "url" json))
         (auth-token (gethash "auth_token" json))
         (interval (or (gethash "mirror_interval_minutes" json) 60))
         (username (getf *current-user* :username))
         ;; Auto-derive name from URL when importing/mirroring without one.
         (name (cond ((and raw-name (not (eq raw-name 'null)) (not (uiop:emptyp raw-name)))
                      raw-name)
                     ((and (member mode '("import" "mirror") :test #'equal)
                           url (not (eq url 'null)) (not (uiop:emptyp url)))
                      (repo-name-from-url url))
                     (t nil))))
    (unless (and name (not (uiop:emptyp name)))
      (return-from api-create-personal-repo (json-error "name required")))
    (unless (member mode '("empty" "import" "mirror") :test #'equal)
      (return-from api-create-personal-repo
        (json-error "mode must be empty, import, or mirror")))
    (when (and (member mode '("import" "mirror") :test #'equal)
               (or (null url) (eq url 'null) (uiop:emptyp url)))
      (return-from api-create-personal-repo
        (json-error "url required for import/mirror mode")))
    (handler-case
        (let ((repo (create-repo :owner-id *current-user-id*
                                 :name name
                                 :description (unless (eq description 'null) description)
                                 :is-private (and is-private (not (eq is-private 'null))))))
          (cond
            ((equal mode "import")
             (import-repo-from-url username name url
                                   :auth-token (unless (or (null auth-token)
                                                            (eq auth-token 'null)
                                                            (uiop:emptyp auth-token))
                                                 auth-token)))
            ((equal mode "mirror")
             (import-repo-from-url username name url
                                   :auth-token (unless (or (null auth-token)
                                                            (eq auth-token 'null)
                                                            (uiop:emptyp auth-token))
                                                 auth-token))
             (create-mirror :repo-id (getf repo :id)
                            :direction "pull"
                            :remote-url url
                            :auth-token (unless (or (null auth-token)
                                                     (eq auth-token 'null)
                                                     (uiop:emptyp auth-token))
                                          auth-token)
                            :interval-minutes interval))
            (t (init-bare-repo username name)))
          (log-event "repo.created" :user-id *current-user-id*
                                    :repo-id (getf repo :id)
                                    :metadata (format nil "{\"mode\": \"~A\"}" mode))
          (json-response (find-repo username name) :status 201))
      (error (e)
        (json-error (format nil "~A" e) :status 400)))))

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
         ;; Counted as a clone for Pulse stats. Anon HTTP clones have no user-id.
         (log-event "git.clone" :user-id *current-user-id* :repo-id (getf repo :id))
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
      (format out "#!/bin/bash~%exec cave-server run-checks --config /etc/cave.conf --repo ~A/~A~%"
              owner repo-name))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Post-receive hook (calls back into running Cave server)
  (let ((hook-path (merge-pathnames "hooks/post-receive" path)))
    (with-open-file (out hook-path :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%# Pipe ref updates to the running Cave server~%tee >(curl -sf -X POST --data-binary @- \"http://localhost:~A/-/internal/hook/post-receive/~A/~A?actor=${CAVE_PUSH_USER_ID:-}\") >/dev/null~%"
              (config-value :http-port 8080) owner repo-name)
      (format out "cave-server sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
              owner repo-name)
      (when (string= repo-name "cave-themes")
        (format out "cave-server sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
                owner)))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Ensure the cave user owns the repo
  (uiop:run-program (list "chown" "-R" "cave:cave" (namestring path))
                     :output :string :error-output :string :ignore-error-status t))

(defun reinstall-all-hooks ()
  "Rewrite pre-receive/post-receive on every existing repo. Idempotent.
   Called from server startup so the hook script always matches the current binary."
  (let ((count 0)
        (failed 0))
    (dolist (repo (list-all-repos))
      (let* ((owner (getf repo :owner-name))
             (name (getf repo :name))
             (path (repo-disk-path owner name)))
        (cond
          ((not (probe-file path))
           (llog:warn "Skipping hook reinstall: repo missing on disk"
                      :owner owner :repo name))
          (t
           (handler-case
               (progn (install-repo-hooks path owner name) (incf count))
             (error (e)
               (incf failed)
               (llog:warn "Hook reinstall failed"
                          :owner owner :repo name :error (princ-to-string e))))))))
    (llog:info "Reinstalled repo hooks" :ok count :failed failed)))

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
  (multiple-value-bind (success-p err)
      (chamber-clone-from-url owner repo-name url :auth-token auth-token)
    (unless success-p
      (error "Clone failed: ~A" err))
    (let ((path (repo-disk-path owner repo-name)))
      (install-repo-hooks path owner repo-name))
    (zoekt-index-repo owner repo-name)
    (llog:info "Imported repo from URL" :owner owner :repo repo-name :url url)))

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
