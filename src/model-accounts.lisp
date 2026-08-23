;;; model.lisp — Domain model queries and CRUD
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; We use postmodern's s-sql for queries and return plists.
;;; No ORM — just functions that return data.

;;; ========================== USERS ==========================

(defun create-user (&key username display-name email oidc-sub (is-admin nil))
  "Create a new user. Returns the user plist."
  (postmodern:query
   (:insert-into 'cave-users
    :set 'username username
         'display-name (or display-name username)
         'email email
         'oidc-sub oidc-sub
         'is-admin is-admin
    :returning '*)
   :plist))

(defun find-user-by-id (id)
  "Find a user by ID."
  (postmodern:query
   (:select '* :from 'cave-users :where (:= 'id id))
   :plist))

(defun find-user-by-username (username)
  "Find a user by username."
  (postmodern:query
   (:select '* :from 'cave-users :where (:= 'username username))
   :plist))

(defun find-user-by-oidc-sub (oidc-sub)
  "Find a user by OIDC subject identifier."
  (when oidc-sub
    (postmodern:query
     (:select '* :from 'cave-users :where (:= 'oidc-sub oidc-sub))
     :plist)))

(defun list-users (&key (active-only t))
  "List all users."
  (if active-only
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-users :where (:= 'is-active t))
        'username)
       :plists)
      (postmodern:query
       (:order-by (:select '* :from 'cave-users) 'username)
       :plists)))

(defun count-users ()
  "Total number of rows in cave-users. Used by the OIDC provisioner to
   bootstrap the first user as approved when admin approval is required."
  (or (postmodern:query
       (:select (:count '*) :from 'cave-users)
       :single)
      0))

(defun list-pending-users ()
  "Self-registered users awaiting admin approval, oldest first."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-users :where (:= 'approval-status "pending"))
    'created-at)
   :plists))

(defun set-user-approval (user-id status)
  "Set approval_status for a user to one of 'approved', 'pending', 'rejected'.
   On rejection, also kill any sessions the user may have established."
  (postmodern:execute
   (:update 'cave-users
    :set 'approval-status status 'updated-at (:now)
    :where (:= 'id user-id)))
  (when (string= status "rejected")
    (postmodern:execute
     (:delete-from 'cave-sessions :where (:= 'user-id user-id)))))

;;; ========================== SSH KEYS ==========================

(defun add-ssh-key (user-id name public-key)
  "Add an SSH public key for a user. Returns the key plist."
  (let ((fingerprint (compute-ssh-fingerprint public-key)))
    (postmodern:query
     (:insert-into 'cave-ssh-keys
      :set 'user-id user-id
           'name name
           'public-key public-key
           'fingerprint fingerprint
      :returning '*)
     :plist)))

(defun compute-ssh-fingerprint (public-key-string)
  "Compute the SHA-256 fingerprint of an SSH public key."
  ;; SSH public keys are: type base64-data [comment]
  ;; We hash the base64-decoded key data
  (let* ((parts (uiop:split-string public-key-string :separator " "))
         (b64-data (second parts)))
    (if b64-data
        (let* ((decoded (cl-base64:base64-string-to-usb8-array b64-data))
               (hash (ironclad:digest-sequence :sha256 decoded)))
          (format nil "SHA256:~A"
                  (cl-base64:usb8-array-to-base64-string hash)))
        (error "Invalid SSH public key format"))))

(defun list-ssh-keys (user-id)
  "List SSH keys for a user."
  (postmodern:query
   (:select '* :from 'cave-ssh-keys :where (:= 'user-id user-id))
   :plists))

(defun find-ssh-key-by-id (key-id)
  "Find an SSH key record by ID."
  (postmodern:query
   (:select '* :from 'cave-ssh-keys :where (:= 'id key-id))
   :plist))

(defun all-active-ssh-keys ()
  "List all SSH keys belonging to active users."
  (postmodern:query
   (:select 'cave-ssh-keys.*
    :from 'cave-ssh-keys
    :inner-join 'cave-users
    :on (:= 'cave-ssh-keys.user-id 'cave-users.id)
    :where (:= 'cave-users.is-active t))
   :plists))

(defun generate-ssh-keypair (user-id key-name)
  "Generate an ed25519 SSH keypair. Stores the public key in the DB.
   Returns (VALUES private-key-string key-record)."
  (let ((tmpdir (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "keygen-~A/" (get-universal-time))
                                  (data-dir "tmp")))))
    (ensure-directories-exist tmpdir)
    (let ((keyfile (merge-pathnames "key" tmpdir)))
      (unwind-protect
           (progn
             (uiop:run-program
              (list "ssh-keygen" "-t" "ed25519" "-f" (namestring keyfile)
                    "-N" "" "-q" "-C" (format nil "~A@cave" key-name))
              :output :string :error-output :string)
             (let* ((public-key (string-trim '(#\Newline #\Return)
                                             (uiop:read-file-string
                                              (merge-pathnames "key.pub" tmpdir))))
                    (private-key (uiop:read-file-string keyfile))
                    (record (add-ssh-key user-id key-name public-key)))
               (values private-key record)))
        ;; Always clean up private key from disk
        (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore)))))

(defun delete-ssh-key (key-id user-id)
  "Delete an SSH key (must belong to user)."
  (postmodern:execute
   (:delete-from 'cave-ssh-keys
    :where (:and (:= 'id key-id) (:= 'user-id user-id)))))

;;; ========================== GPG KEYS ==========================

(defun gpg-key-fingerprint (armored-public-key)
  "Parse ARMORED-PUBLIC-KEY with gpg and return its primary-key fingerprint
   (uppercase hex string), or NIL if it cannot be parsed. Uses a throwaway
   homedir so root's keyring is never touched and nothing is imported."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "gpgparse-~A/" (get-universal-time))
                               (data-dir "tmp")))))
    (ensure-directories-exist dir)
    (uiop:run-program (list "chmod" "700" (namestring dir)) :ignore-error-status t)
    (unwind-protect
         (multiple-value-bind (out _err exit)
             (with-input-from-string (in armored-public-key)
               (uiop:run-program
                (list "gpg" "--homedir" (namestring dir) "--batch" "--with-colons"
                      "--import-options" "show-only" "--import")
                :input in :output '(:string) :error-output '(:string)
                :ignore-error-status t))
           (declare (ignore _err))
           (when (zerop exit)
             ;; First "fpr:" record is the primary key; fingerprint is field 10.
             (loop for line in (uiop:split-string out :separator '(#\Newline))
                   when (uiop:string-prefix-p "fpr:" line)
                     do (return (nth 9 (uiop:split-string line :separator '(#\:)))))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(defun add-gpg-key (user-id name public-key)
  "Add a GPG public key for a user. Returns the key plist.
   Signals an error if the key cannot be parsed."
  (let ((fpr (gpg-key-fingerprint public-key)))
    (unless fpr
      (error "Could not parse GPG public key. Paste an ASCII-armored public key ~
              (the output of `gpg --armor --export <keyid>`)."))
    (postmodern:query
     (:insert-into 'cave-gpg-keys
      :set 'user-id user-id
           'name name
           'public-key public-key
           'key-id fpr
      :returning '*)
     :plist)))

(defun list-gpg-keys (user-id)
  "List GPG keys for a user."
  (postmodern:query
   (:select '* :from 'cave-gpg-keys :where (:= 'user-id user-id))
   :plists))

(defun delete-gpg-key (key-id user-id)
  "Delete a GPG key (must belong to user)."
  (postmodern:execute
   (:delete-from 'cave-gpg-keys
    :where (:and (:= 'id key-id) (:= 'user-id user-id)))))

;;; ========================== API TOKENS ==========================

(defun create-api-token (user-id name)
  "Create a new API token. Returns (VALUES token-string token-record)."
  (multiple-value-bind (token hash prefix) (generate-api-token)
    (let ((record (postmodern:query
                   (:insert-into 'cave-api-tokens
                    :set 'user-id user-id
                         'name name
                         'token-hash hash
                         'token-prefix prefix
                    :returning '*)
                   :plist)))
      (values token record))))

(defun list-api-tokens (user-id)
  "List API tokens for a user (without hashes)."
  (postmodern:query
   (:select 'id 'name 'token-prefix 'last-used-at 'created-at
    :from 'cave-api-tokens
    :where (:= 'user-id user-id))
   :plists))

(defun delete-api-token (token-id user-id)
  "Delete an API token (must belong to user)."
  (postmodern:execute
   (:delete-from 'cave-api-tokens
    :where (:and (:= 'id token-id) (:= 'user-id user-id)))))

;;; ========================== ORGS ==========================

(defun valid-resource-name-p (name)
  "True when NAME is safe to use as a single on-disk path component (a repo,
   org, or user name). Allows alphanumerics, dot, underscore and hyphen;
   rejects empty/overlong names, a leading dot or hyphen, any '..' sequence,
   and anything outside the set — notably '/' — so it can never traverse out
   of the repo storage root via REPO-DISK-PATH."
  (and (stringp name)
       (<= 1 (length name) 100)
       (not (search ".." name))
       (not (member (char name 0) '(#\. #\-)))
       (every (lambda (c)
                (or (alphanumericp c)
                    (member c '(#\. #\_ #\-))))
              name)))

(defun ensure-valid-resource-name (name)
  "Signal an error unless NAME is a valid repo/org/user name. Returns NAME."
  (unless (valid-resource-name-p name)
    (error "Invalid name ~S — names may contain only letters, digits, '.', '_' and '-', and may not start with '.' or '-' or contain '..'." name))
  name)

(defun create-org (&key name display-name description creator-id)
  "Create a new org and add creator as admin."
  (ensure-valid-resource-name name)
  (let ((org (postmodern:query
              (:insert-into 'cave-orgs
               :set 'name name
                    'display-name (or display-name name)
                    'description description
               :returning '*)
              :plist)))
    ;; Add creator as org admin
    (postmodern:execute
     (:insert-into 'cave-org-members
      :set 'org-id (getf org :id)
           'user-id creator-id
           'role "admin"))
    org))

(defun find-org-by-name (name)
  "Find an org by name."
  (postmodern:query
   (:select '* :from 'cave-orgs :where (:= 'name name))
   :plist))

(defun list-user-orgs (user-id)
  "List orgs a user belongs to."
  (postmodern:query
   (:order-by
    (:select 'cave-orgs.* 'cave-org-members.role
     :from 'cave-org-members
     :inner-join 'cave-orgs
     :on (:= 'cave-org-members.org-id 'cave-orgs.id)
     :where (:= 'cave-org-members.user-id user-id))
    'cave-orgs.name)
   :plists))

(defun org-member-role (org-id user-id)
  "Get a user's role in an org. Returns role string or NIL."
  (postmodern:query
   (:select 'role :from 'cave-org-members
    :where (:and (:= 'org-id org-id) (:= 'user-id user-id)))
   :single))

(defun list-org-members (org-id)
  "List all members of an org with usernames."
  (postmodern:query
   (:order-by
    (:select 'cave-org-members.* 'cave-users.username 'cave-users.email
     :from 'cave-org-members
     :inner-join 'cave-users :on (:= 'cave-org-members.user-id 'cave-users.id)
     :where (:= 'cave-org-members.org-id org-id))
    'cave-users.username)
   :plists))

(defun add-org-member (org-id user-id &key (role "member"))
  "Add a member to an org."
  (postmodern:execute
   (:insert-into 'cave-org-members
    :set 'org-id org-id 'user-id user-id 'role role)))

(defun remove-org-member (org-id user-id)
  "Remove a member from an org."
  (postmodern:execute
   (:delete-from 'cave-org-members
    :where (:and (:= 'org-id org-id) (:= 'user-id user-id)))))

