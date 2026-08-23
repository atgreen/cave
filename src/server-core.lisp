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

(defmethod hunchentoot:acceptor-ssl-p ((acceptor cave-acceptor))
  "TLS is terminated at the reverse proxy (Caddy); cave's own acceptor speaks
   plain HTTP on loopback, so Hunchentoot would otherwise treat every request as
   insecure. That makes HUNCHENTOOT:REDIRECT absolutize a relative target to an
   http:// URL (misc.lisp REDIRECT defaults PROTOCOL to (if (ssl-p) :https :http)),
   so e.g. merging a PR bounced the browser to http://.../pulls/N. Trust the
   proxy's X-Forwarded-Proto header instead: the acceptor binds only to
   127.0.0.1, so nothing but the proxy can set it. This also makes Secure-cookie
   logic correct behind TLS. Falls back to the default (NIL) for direct HTTP
   (local dev), so nothing changes there."
  (or (and (boundp 'hunchentoot:*request*)
           hunchentoot:*request*
           (let ((proto (hunchentoot:header-in :x-forwarded-proto hunchentoot:*request*)))
             (and proto (string-equal proto "https"))))
      (call-next-method)))

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
               ;; Embedded Usher OIDC provider — serve its endpoints before
               ;; cave's own routes.
               (when (and *usher-dispatch* (usher-endpoint-p uri))
                 (return-from hunchentoot:acceptor-dispatch-request
                   (dispatch-usher request)))
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

(defun universal-to-rfc3339 (ut)
  "Format a CL universal-time integer as an RFC3339 UTC timestamp string."
  (multiple-value-bind (sec min hr day mon yr) (decode-universal-time ut 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mon day hr min sec)))

(defun plist-to-hash-table (plist)
  "Convert a plist to a hash table with lowercase string keys for JSON output.
SQL NULL (:null) becomes JSON null; universal-time integers in *_at fields are
emitted as RFC3339 UTC strings so the public API speaks standard timestamps."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key val) on plist by #'cddr
          for k = (string-downcase (substitute #\_ #\- (symbol-name key)))
          do (setf (gethash k ht)
                   (cond
                     ((eq val :null) 'null)
                     ;; *_at columns hold universal time; 2208988800 = 1970 in
                     ;; that epoch, so anything larger is a real timestamp.
                     ((and (integerp val)
                           (>= (length k) 3)
                           (string= "_at" k :start2 (- (length k) 3))
                           (> val 2208988800))
                      (universal-to-rfc3339 val))
                     (t val))))
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
  "Return NEXT-URL when it is a safe in-app redirect target, otherwise \"/\".
   Only a same-origin absolute path is allowed: a leading '/', not '//' (a
   scheme-relative URL to another host), and no backslash (browsers normalize
   '\\' to '/', so '/\\evil.com' would otherwise resolve to '//evil.com'), and
   no CR/LF (header injection)."
  (if (and next-url
           (> (length next-url) 0)
           (char= (char next-url 0) #\/)
           (or (= (length next-url) 1)
               (not (char= (char next-url 1) #\/)))
           (not (find #\\ next-url))
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

(defmacro %with-repo-admin ((repo owner repo-name fail-form) &body body)
  "Bind REPO from OWNER/REPO-NAME, requiring login + repo admin; else short-circuit."
  `(when (require-login)
     (let ((,repo (find-repo ,owner ,repo-name)))
       (unless ,repo (return-from ,fail-form (not-found)))
       (unless (equal (repo-member-role (getf ,repo :id) *current-user-id*) "admin")
         (setf (hunchentoot:return-code*) 403)
         (return-from ,fail-form "Forbidden"))
       ,@body)))

