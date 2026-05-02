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

;;; --- Password hashing (bcrypt) ---

(defun hash-password (plaintext)
  "Hash a plaintext password using bcrypt. Returns the hash string."
  (bcrypt:encode (bcrypt:make-password plaintext)))

(defun check-password (plaintext hash)
  "Verify PLAINTEXT against a bcrypt HASH. Returns T on match."
  (bcrypt:password= plaintext hash))

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
  ;; Try query param token (for HTTP clone)
  (let ((token (hunchentoot:get-parameter "token")))
    (when-let ((user-id (validate-api-token token)))
      (setf *current-user-id* user-id)
      (setf *current-user* (find-user-by-id user-id))
      (return-from authenticate-request *current-user*)))
  nil)

