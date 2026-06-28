;;; auth.lisp — Authentication: passwords, sessions, API tokens
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; --- Utilities ---

(defmacro when-let (bindings &body body)
  "Like LET but only execute BODY if the first binding is non-nil.
   Usage: (when-let ((var expr)) body...)"
  (let ((var (caar bindings))
        (expr (cadar bindings)))
    `(let ((,var ,expr))
       (when ,var ,@body))))

;;; --- OIDC (Keycloak) ---

(defun generate-oidc-state ()
  "Generate a random state parameter for OIDC CSRF protection."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun generate-oidc-verifier ()
  "Generate a PKCE code verifier (RFC 7636). Hex digits are all unreserved."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))

(defun oidc-code-challenge (verifier)
  "S256 PKCE challenge for VERIFIER: unpadded base64url of its SHA-256."
  (let* ((digest (ironclad:digest-sequence
                  :sha256 (ironclad:ascii-string-to-byte-array verifier)))
         (b64 (cl-base64:usb8-array-to-base64-string digest)))
    (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ b64)))))

(defun oidc-authorization-url (state &key force-login code-challenge nonce)
  "Build the OIDC authorization redirect URL (browser-facing).
   If FORCE-LOGIN is T, forces re-authentication (for sudo mode)."
  (format nil "~A/authorize?response_type=code&client_id=~A&redirect_uri=~A&state=~A&scope=openid%20profile%20email&code_challenge=~A&code_challenge_method=S256&nonce=~A~@[&max_age=0~]"
          (config-value :oidc-issuer)
          (config-value :oidc-client-id)
          (hunchentoot:url-encode (oidc-redirect-uri))
          state
          (hunchentoot:url-encode (or code-challenge ""))
          (hunchentoot:url-encode (or nonce ""))
          force-login))

(defun exchange-oidc-code (code &optional code-verifier)
  "Exchange an OIDC authorization code for tokens. Returns parsed JSON hash-table
   or NIL. Authenticates with client_secret_basic."
  (handler-case
      (let ((response (dex:post
                       (format nil "~A/token"
                               (oidc-issuer-internal))
                       :headers `(("Authorization"
                                   . ,(format nil "Basic ~A"
                                       (cl-base64:string-to-base64-string
                                        (format nil "~A:~A"
                                                (config-value :oidc-client-id)
                                                (or (config-value :oidc-client-secret) ""))))))
                       :content `(("grant_type" . "authorization_code")
                                  ("code" . ,code)
                                  ("redirect_uri" . ,(oidc-redirect-uri))
                                  ,@(when code-verifier
                                      `(("code_verifier" . ,code-verifier)))))))
        (let ((parsed (com.inuoe.jzon:parse response)))
          (llog:info "OIDC token exchange succeeded"
                     :has-access-token (if (gethash "access_token" parsed) "yes" "no"))
          parsed))
    (error (e)
      (llog:error "OIDC token exchange failed" :error (princ-to-string e))
      nil)))

(defun fetch-oidc-userinfo (access-token)
  "Fetch user info from the OIDC userinfo endpoint. Returns hash-table or NIL.
   Tries the internal issuer first, falls back to external."
  (handler-case
      (let ((response (dex:get
                       (format nil "~A/userinfo"
                               (oidc-issuer-internal))
                       :headers `(("Authorization" . ,(format nil "Bearer ~A" access-token))))))
        (com.inuoe.jzon:parse response))
    (error (e)
      (llog:error "OIDC userinfo fetch failed" :error (princ-to-string e))
      nil)))

(defun oidc-user-is-admin-p (userinfo)
  "Check if the OIDC userinfo includes the cave-admin group.
   Usher conveys roles via a top-level `groups` claim (a JSON array)."
  (let ((groups (gethash "groups" userinfo)))
    (when groups
      (find "cave-admin" groups :test #'string=))))

(defun provision-oidc-user (userinfo)
  "Create or update a local user from OIDC claims. Returns user plist."
  (let* ((sub (gethash "sub" userinfo))
         (username (gethash "preferred_username" userinfo))
         (email (gethash "email" userinfo))
         (display-name (or (gethash "name" userinfo) username))
         (is-admin (if (oidc-user-is-admin-p userinfo) t nil))
         (existing (find-user-by-oidc-sub sub)))
    (cond
      ;; Known user — sync profile
      (existing
       (postmodern:execute
        (:update 'cave-users
         :set 'is-admin is-admin
              'email email
              'display-name display-name
              'updated-at (:now)
         :where (:= 'oidc-sub sub)))
       (find-user-by-oidc-sub sub))
      ;; Username match — link existing user to OIDC sub
      ((find-user-by-username username)
       (let ((user (find-user-by-username username)))
         (postmodern:execute
          (:update 'cave-users
           :set 'oidc-sub sub
                'is-admin is-admin
                'email email
                'display-name display-name
                'updated-at (:now)
           :where (:= 'id (getf user :id))))
         (find-user-by-id (getf user :id))))
      ;; New user — create. First user bootstraps as approved so the
      ;; instance has an admin who can approve everyone else; all subsequent
      ;; signups land as 'pending' and wait for an admin to act.
      (t
       (let ((approval (if (zerop (count-users)) "approved" "pending")))
         (postmodern:query
          (:insert-into 'cave-users
           :set 'username username
                'oidc-sub sub
                'email email
                'display-name (or display-name username)
                'is-admin is-admin
                'approval-status approval
           :returning '*)
          :plist))))))

;;; --- Embedded Usher OIDC provider ---
;;; Cave hosts its own OpenID Provider (Usher) in-process, mounted at the root
;;; of this host. Cave is both the provider and the relying party. The endpoints
;;; below are reserved top-level paths on this host.

(defparameter *usher-endpoint-prefixes*
  '("/authorize" "/token" "/userinfo"
    "/.well-known/openid-configuration" "/.well-known/jwks.json"
    "/totp/" "/verify-email" "/reset-password" "/social/")
  "Root paths served by the embedded Usher provider (reserved on this host).")

(defvar *usher-dispatch* nil "Cached embedded-Usher dispatch table.")

(defun usher-endpoint-p (uri)
  "True when URI is served by the embedded Usher provider."
  (some (lambda (p) (or (string= uri p) (uiop:string-prefix-p p uri)))
        *usher-endpoint-prefixes*))

(defun dispatch-usher (request)
  "Run the embedded Usher dispatch table for REQUEST; return the response."
  (loop for d in *usher-dispatch*
        for handler = (funcall d request)
        when handler do (return (funcall handler))))

(defun usher-keys-path ()
  (merge-pathnames "usher-keys.json"
                   (uiop:ensure-directory-pathname (config-value :data-dir))))

(defun ensure-usher-client ()
  "Register (idempotently) the cave OAuth client in the embedded provider,
   from cave's own OIDC config."
  (let ((store (usher:provider-store usher::*provider*)))
    (unless (usher:store-find-client store (config-value :oidc-client-id))
      (usher:store-add-client
       store (usher:make-client :id (config-value :oidc-client-id)
                                :type :confidential
                                :secret-hash (usher:hash-password
                                              (or (config-value :oidc-client-secret) ""))
                                :name "Cave"
                                :redirect-uris (list (oidc-redirect-uri)))))))

(defun init-usher ()
  "Build and install the embedded Usher OIDC provider (idempotent). Migrates the
   usher_* tables in cave's database, loads/persists signing keys, and registers
   the cave client."
  (setf usher:*config*
        (usher:make-default-config
         :issuer (config-value :oidc-issuer)
         :tls '(:mode :none)
         :totp-issuer "Cave"
         :db (list :backend :postgres
                   :host (config-value :db-host)
                   :port (config-value :db-port)
                   :name (config-value :db-name)
                   :user (config-value :db-user)
                   :password (config-value :db-password))))
  (let ((keys (usher:ensure-signing-keys (usher-keys-path))))
    (setf usher::*provider*
          (usher:make-provider :store (usher::configured-store)
                               :signing-keys keys)))
  (ensure-usher-client)
  (setf *usher-dispatch* (usher::make-dispatch-table))
  (llog:info "Embedded Usher OIDC provider ready"
             :issuer (config-value :oidc-issuer)))

(defun usher-migrate-users (&key (force-admins '("atgreen")))
  "Provision Usher accounts from the existing cave_users (username, email,
   display name, is_admin). Accounts in FORCE-ADMINS are granted cave-admin too.
   Skips users that already exist. Returns a list of (username . temp-password)
   for newly created accounts — show these to the operator once."
  (let ((results nil))
    (dolist (row (postmodern:query
                  (:select 'username 'email 'display-name 'is-admin
                   :from 'cave-users)
                  :rows))
      (destructuring-bind (username email display-name is-admin) row
        (let* ((real-email (and email (not (string-equal email "false")) email))
               (admin (or (eq is-admin t)
                          (member username force-admins :test #'string=)))
               (temp (usher:random-token 9)))
          (unless (usher:store-find-user-by-username
                   (usher:provider-store usher::*provider*) username)
            (usher-add-user username temp :email real-email
                            :display-name display-name :admin admin)
            (push (cons username temp) results)))))
    (nreverse results)))

;;; Self-service TOTP (two-factor) — operate on the current cave user's embedded
;;; Usher account, in-process.

(defun usher-current-account ()
  "The embedded-Usher user for the current cave session, or NIL."
  (and *current-user*
       (usher:store-find-user-by-username
        (usher:provider-store usher::*provider*)
        (getf *current-user* :username))))

(defun usher-totp-enabled-p ()
  (let ((u (usher-current-account)))
    (and u (usher:totp-enabled-p (usher:provider-store usher::*provider*) u))))

(defun usher-totp-enroll ()
  "Begin TOTP enrollment for the current user. Returns (values otpauth-uri secret)."
  (let ((u (usher-current-account)))
    (when u (usher:enroll-totp! (usher:provider-store usher::*provider*) u :issuer "Cave"))))

(defun usher-totp-confirm (code)
  "Activate the pending TOTP secret if CODE matches. On success returns a fresh
   list of backup codes (plaintext, show once); NIL on failure."
  (let* ((store (usher:provider-store usher::*provider*)) (u (usher-current-account)))
    (when (and u (usher:confirm-totp! store u code))
      (usher:generate-backup-codes store u :count 10))))

(defun usher-totp-disable ()
  "Disable TOTP (and clear backup codes) for the current user."
  (let* ((store (usher:provider-store usher::*provider*)) (u (usher-current-account)))
    (when u
      (usher:store-cred-del store (usher:user-subject u) "totp")
      (usher:store-cred-del store (usher:user-subject u) "totp-pending")
      (usher:store-cred-del store (usher:user-subject u) "backup-code")
      t)))

(defun usher-backup-codes-regenerate ()
  "Regenerate single-use backup codes for the current user; returns the codes."
  (let* ((store (usher:provider-store usher::*provider*)) (u (usher-current-account)))
    (when u (usher:generate-backup-codes store u :count 10))))

(defun totp-qr-data-uri (otpauth-uri)
  "An inline PNG data: URI of the QR code for OTPAUTH-URI."
  (let ((bytes (flexi-streams:with-output-to-sequence (s)
                 (cl-qrencode:encode-png-stream otpauth-uri s :version 8 :level :level-m))))
    (format nil "data:image/png;base64,~A"
            (cl-base64:usb8-array-to-base64-string bytes))))

(defun usher-set-password (username new-password)
  "Set USERNAME's embedded-Usher password. Returns T on success, NIL if no such
   user. Used by the self-service change-password flow."
  (let* ((store (usher:provider-store usher::*provider*))
         (user (usher:store-find-user-by-username store username)))
    (when user
      (setf (usher:user-password-hash user) (usher:hash-password new-password))
      (usher:store-add-user store user)
      t)))

(defun usher-add-user (username password &key email display-name admin)
  "Provision (or update) a local Usher user; optionally grant cave-admin.
   For manually migrating accounts off Keycloak."
  (let* ((store (usher:provider-store usher::*provider*))
         (user (or (usher:store-find-user-by-username store username)
                   (let ((u (usher:make-user :subject (usher:random-uuid)
                                             :username username
                                             :name (or display-name username)
                                             :email email :email-verified (and email t)
                                             :status "active"
                                             :password-hash (usher:hash-password password))))
                     (usher:store-add-user store u)
                     u))))
    (when admin (usher:add-user-group store user "cave-admin"))
    (llog:info "Provisioned Usher user" :username username :admin (and admin t))
    user))

;;; --- Sudo mode (step-up authentication) ---

(defparameter *sudo-timeout-seconds* 300 "Sudo mode lasts 5 minutes.")

(defun sudo-active-p ()
  "Check if the current user has recently re-authenticated (sudo mode)."
  (let ((sudo-cookie (hunchentoot:cookie-in "cave_sudo")))
    (when sudo-cookie
      (handler-case
          (let ((timestamp (parse-integer sudo-cookie :junk-allowed t)))
            (and timestamp
                 (< (- (get-universal-time) timestamp) *sudo-timeout-seconds*)))
        (error () nil)))))

(defun set-sudo-cookie ()
  "Set the sudo cookie to current time."
  (hunchentoot:set-cookie "cave_sudo"
                          :value (princ-to-string (get-universal-time))
                          :path "/"
                          :http-only t
                          :max-age *sudo-timeout-seconds*))

;;; --- API tokens ---

(defun generate-api-token ()
  "Generate a new API token. Returns (VALUES token-string token-hash token-prefix)."
  (let* ((raw (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))
         (token (format nil "cave_~A" raw))
         (hash (sha256-hex token))
         (prefix (subseq token 0 8)))
    (values token hash prefix)))

(defun sha256-hex (string)
  "Return the hex SHA-256 of STRING."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
                             (flexi-streams:string-to-octets string
                                                             :external-format :utf-8))))

;;; --- Session management ---

(defvar *session-duration-hours* 168  "Session TTL in hours (1 week).")

(defun create-session (user-id)
  "Create a new session for USER-ID. Returns the session token."
  (let ((token (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))
    (postmodern:execute
     (:insert-into 'cave-sessions
      :set 'user-id user-id
           'session-token token
           'expires-at (:+ (:now)
                           (:raw (format nil "INTERVAL '~A hours'"
                                         *session-duration-hours*)))))
    token))

(defun validate-session (token)
  "Validate a session token. Returns the user-id or NIL."
  (when token
    (let ((row (postmodern:query
                (:select 'user-id :from 'cave-sessions
                 :where (:and (:= 'session-token token)
                              (:> 'expires-at (:now))))
                :row)))
      (when row (first row)))))

(defun delete-session (token)
  "Delete a session by token."
  (when token
    (postmodern:execute
     (:delete-from 'cave-sessions
      :where (:= 'session-token token)))))

(defun cleanup-expired-sessions ()
  "Remove all expired sessions."
  (postmodern:execute
   (:delete-from 'cave-sessions
    :where (:<= 'expires-at (:now)))))

;;; --- Token-based API auth ---

(defun validate-api-token (token-string)
  "Validate an API token string. Returns the user-id or NIL."
  (when (and token-string (> (length token-string) 8))
    (let* ((hash (sha256-hex token-string))
           (row (postmodern:query
                 (:select 'user-id :from 'cave-api-tokens
                  :where (:= 'token-hash hash))
                 :row)))
      (when row
        ;; Update last_used_at
        (postmodern:execute
         (:update 'cave-api-tokens
          :set 'last-used-at (:now)
          :where (:= 'token-hash hash)))
        (first row)))))

;;; --- Current user (request context) ---

(defvar *current-user* nil
  "The currently authenticated user (a plist from cave_users), bound per-request.")

(defvar *current-user-id* nil
  "The currently authenticated user's ID, bound per-request.")

(defun authenticate-request ()
  "Attempt to authenticate the current Hunchentoot request.
   Checks session cookie first, then Authorization header.
   Sets *current-user-id* and *current-user* if successful."
  ;; Try session cookie
  (let ((session-token (hunchentoot:cookie-in "cave_session")))
    (when-let ((user-id (validate-session session-token)))
      (setf *current-user-id* user-id)
      (setf *current-user* (find-user-by-id user-id))
      (return-from authenticate-request *current-user*)))
  ;; Try Bearer token
  (let ((auth-header (hunchentoot:header-in* "authorization")))
    (when (and auth-header
               (>= (length auth-header) 7)
               (string-equal "Bearer " (subseq auth-header 0 7)))
      (let ((token (subseq auth-header 7)))
        (when-let ((user-id (validate-api-token token)))
          (setf *current-user-id* user-id)
          (setf *current-user* (find-user-by-id user-id))
          (return-from authenticate-request *current-user*)))))
  nil)
