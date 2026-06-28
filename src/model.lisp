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

(defun deactivate-user (user-id)
  "Deactivate a user account."
  (postmodern:execute
   (:update 'cave-users
    :set 'is-active nil 'updated-at (:now)
    :where (:= 'id user-id)))
  ;; Invalidate all sessions
  (postmodern:execute
   (:delete-from 'cave-sessions :where (:= 'user-id user-id))))

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

(defun find-user-by-ssh-key (public-key)
  "Find a user by their SSH public key. Returns user plist or NIL."
  (let ((fingerprint (compute-ssh-fingerprint public-key)))
    (let ((row (postmodern:query
                (:select 'cave-users.*
                 :from 'cave-ssh-keys
                 :inner-join 'cave-users
                 :on (:= 'cave-ssh-keys.user-id 'cave-users.id)
                 :where (:and (:= 'cave-ssh-keys.fingerprint fingerprint)
                              (:= 'cave-users.is-active t)))
                :plist)))
      row)))

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

(defun find-gpg-key-by-id (key-id)
  "Find a GPG key record by ID."
  (postmodern:query
   (:select '* :from 'cave-gpg-keys :where (:= 'id key-id))
   :plist))

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

;;; ========================== REPOS ==========================

(defun create-repo (&key org-id owner-id name description (is-private nil))
  "Create a new repo. Must have exactly one of ORG-ID or OWNER-ID."
  (ensure-valid-resource-name name)
  (let ((repo (postmodern:query
               (:insert-into 'cave-repos
                :set 'org-id (or org-id :null)
                     'owner-id (or owner-id :null)
                     'name name
                     'description description
                     'is-private is-private
                :returning '*)
               :plist)))
    ;; Auto-assign to chamber node if multi-chamber is active
    (let ((nodes (config-value :chamber-nodes)))
      (when (and repo nodes (> (length nodes) 1))
        (handler-case (ensure-repo-assigned (getf repo :id))
          (error (e)
            (llog:warn "Failed to assign repo to chamber node"
                       :repo-id (getf repo :id) :error (princ-to-string e))))))
    repo))

(defun find-repo (owner-name repo-name)
  "Find a repo by owner (org or user) name and repo name."
  ;; Try org first
  (let ((result (postmodern:query
                 (:select 'cave-repos.*
                  :from 'cave-repos
                  :inner-join 'cave-orgs
                  :on (:= 'cave-repos.org-id 'cave-orgs.id)
                  :where (:and (:= 'cave-orgs.name owner-name)
                               (:= 'cave-repos.name repo-name)))
                 :plist)))
    (when result (return-from find-repo result)))
  ;; Try user
  (postmodern:query
   (:select 'cave-repos.*
    :from 'cave-repos
    :inner-join 'cave-users
    :on (:= 'cave-repos.owner-id 'cave-users.id)
    :where (:and (:= 'cave-users.username owner-name)
                 (:= 'cave-repos.name repo-name)))
   :plist))

(defun find-repo-by-id (repo-id)
  "Find a repo by ID."
  (postmodern:query
   (:select '* :from 'cave-repos :where (:= 'id repo-id))
   :plist))

(defun repo-owner-name (repo)
  "Get the owner name (org name or username) for a repo."
  (if (and (getf repo :org-id)
           (not (eq (getf repo :org-id) :null)))
      (let ((org (postmodern:query
                  (:select 'name :from 'cave-orgs
                   :where (:= 'id (getf repo :org-id)))
                  :single)))
        org)
      (postmodern:query
       (:select 'username :from 'cave-users
        :where (:= 'id (getf repo :owner-id)))
       :single)))

(defun list-org-repos (org-id &key include-private)
  "List repos in an org."
  (if include-private
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos :where (:= 'org-id org-id))
        'name)
       :plists)
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos
         :where (:and (:= 'org-id org-id) (:= 'is-private nil)))
        'name)
       :plists)))

(defun list-public-repos (&key (limit 50))
  "Public repos across the whole instance, newest first. For the anon landing."
  (postmodern:query
   (:limit
    (:order-by
     (:select 'cave-repos.*
              (:as (:coalesce 'cave-orgs.name 'cave-users.username) 'owner-name)
      :from 'cave-repos
      :left-join 'cave-orgs :on (:= 'cave-repos.org-id 'cave-orgs.id)
      :left-join 'cave-users :on (:= 'cave-repos.owner-id 'cave-users.id)
      :where (:= 'cave-repos.is-private nil))
     (:desc (:coalesce 'cave-repos.last-pushed-at 'cave-repos.updated-at)))
    limit)
   :plists))

(defun list-recent-public-events (&key (limit 20))
  "Recent events on public repos. For the anon landing's activity feed.
   Clones are excluded — they're noise in a feed and counted in Pulse instead."
  (postmodern:query
   (:limit
    (:order-by
     (:select 'cave-events.* (:as 'cave-users.username 'actor)
              (:as 'cave-repos.name 'repo-name)
      :from 'cave-events
      :left-join 'cave-users :on (:= 'cave-events.user-id 'cave-users.id)
      :inner-join 'cave-repos :on (:= 'cave-events.repo-id 'cave-repos.id)
      :where (:and (:= 'cave-repos.is-private nil)
                   (:!= 'cave-events.event-type "git.clone")))
     (:desc 'cave-events.created-at))
    limit)
   :plists))

(defun list-all-repos ()
  "Every repo in the system, plist form. Used by startup sweeps."
  (postmodern:query
   (:order-by
    (:select 'cave-repos.*
             (:as (:coalesce 'cave-orgs.name 'cave-users.username) 'owner-name)
     :from 'cave-repos
     :left-join 'cave-orgs :on (:= 'cave-repos.org-id 'cave-orgs.id)
     :left-join 'cave-users :on (:= 'cave-repos.owner-id 'cave-users.id))
    'cave-repos.id)
   :plists))

(defun list-user-repos (user-id &key include-private)
  "List repos owned by a user."
  (if include-private
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos :where (:= 'owner-id user-id))
        'name)
       :plists)
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos
         :where (:and (:= 'owner-id user-id) (:= 'is-private nil)))
        'name)
       :plists)))

(defun repo-member-role (repo-id user-id)
  "Get a user's role in a repo. Returns role string or NIL.
   Repo owner is implicit admin. Org admin is implicit repo admin."
  ;; Direct repo membership
  (let ((direct (postmodern:query
                 (:select 'role :from 'cave-repo-members
                  :where (:and (:= 'repo-id repo-id) (:= 'user-id user-id)))
                 :single)))
    (when direct (return-from repo-member-role direct)))
  (let ((repo (find-repo-by-id repo-id)))
    (when repo
      ;; User-owned repo: owner is implicit admin
      (when (and (getf repo :owner-id)
                 (not (eq (getf repo :owner-id) :null))
                 (= (getf repo :owner-id) user-id))
        (return-from repo-member-role "admin"))
      ;; Org repo: org admin is implicit repo admin
      (when (and (getf repo :org-id)
                 (not (eq (getf repo :org-id) :null)))
        (let ((org-role (org-member-role (getf repo :org-id) user-id)))
          (when (equal org-role "admin")
            (return-from repo-member-role "admin"))))))
  nil)

(defun repo-reviewer-p (repo-id user-id)
  "True when USER-ID currently has a repo role allowed to submit reviews."
  (member (repo-member-role repo-id user-id)
          '("reviewer" "writer" "admin")
          :test #'equal))

(defun add-repo-member (repo-id user-id &key (role "writer"))
  "Add a member to a repo."
  (postmodern:execute
   (:insert-into 'cave-repo-members
    :set 'repo-id repo-id 'user-id user-id 'role role)))

(defun list-repo-members (repo-id)
  "List all members of a repo with usernames."
  (postmodern:query
   (:order-by
    (:select 'cave-repo-members.* 'cave-users.username
     :from 'cave-repo-members
     :inner-join 'cave-users :on (:= 'cave-repo-members.user-id 'cave-users.id)
     :where (:= 'cave-repo-members.repo-id repo-id))
    'cave-users.username)
   :plists))

(defun remove-repo-member (repo-id user-id)
  "Remove a member from a repo."
  (postmodern:execute
   (:delete-from 'cave-repo-members
    :where (:and (:= 'repo-id repo-id) (:= 'user-id user-id)))))

(defun archive-repo (repo-id &key (archived t))
  "Archive or unarchive a repo."
  (postmodern:execute
   (:update 'cave-repos
    :set 'is-archived archived 'updated-at (:now)
    :where (:= 'id repo-id))))

(defun touch-repo-pushed-at (repo-id)
  "Bump last_pushed_at on a repo. Called from the post-receive hook."
  (postmodern:execute
   (:update 'cave-repos
    :set 'last-pushed-at (:now)
    :where (:= 'id repo-id))))

(defun delete-repo (repo-id)
  "Delete a repo from the database. Caller must also remove disk files."
  (postmodern:execute
   (:delete-from 'cave-repos :where (:= 'id repo-id))))

(defun update-repo-settings (repo-id &key required-approvals allow-self-approval
                                          allow-stale-approvals concerns-count-as-approval
                                          block-on-request-changes auto-delete-branch)
  "Update merge policy settings for a repo."
  (postmodern:execute
   (:update 'cave-repos
    :set 'required-approvals required-approvals
         'allow-self-approval allow-self-approval
         'allow-stale-approvals allow-stale-approvals
         'concerns-count-as-approval concerns-count-as-approval
         'block-on-request-changes block-on-request-changes
         'auto-delete-branch auto-delete-branch
         'updated-at (:now)
    :where (:= 'id repo-id))))

;;; ========================== AUTOMATIONS ==========================

(defun create-automation-definition (&key repo-id name trigger command
                                          runner-labels timeout-seconds
                                          concurrency-key (source "ui"))
  "Create an automation definition."
  (postmodern:query
   (:insert-into 'cave-automation-definitions
    :set 'repo-id repo-id 'name name 'trigger trigger 'command command
         'runner-labels (or runner-labels "")
         'timeout-seconds (or timeout-seconds 60)
         'concurrency-key (or concurrency-key :null)
         'source source
    :returning '*)
   :plist))

(defun list-automation-definitions (repo-id &key trigger)
  "List automation definitions for a repo, optionally filtered by trigger."
  (if trigger
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-automation-definitions
         :where (:and (:= 'repo-id repo-id) (:= 'trigger trigger) (:= 'enabled t)))
        'name)
       :plists)
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-automation-definitions
         :where (:= 'repo-id repo-id))
        'name)
       :plists)))

(defun delete-automation-definition (def-id repo-id)
  "Delete an automation definition."
  (postmodern:execute
   (:delete-from 'cave-automation-definitions
    :where (:and (:= 'id def-id) (:= 'repo-id repo-id)))))

;;; ========================== AUTOMATION RUNS ==========================

(defun create-automation-run (&key repo-id definition-id definition-name
                                    trigger-event commit-sha ref triggered-by-id)
  "Create an automation run (queued)."
  (postmodern:query
   (:insert-into 'cave-automation-runs
    :set 'repo-id repo-id
         'definition-id (or definition-id :null)
         'definition-name definition-name
         'trigger-event trigger-event
         'commit-sha (or commit-sha :null)
         'ref (or ref :null)
         'triggered-by-id (or triggered-by-id :null)
    :returning '*)
   :plist))

(defun list-automation-runs (repo-id &key (limit 30))
  "List recent automation runs for a repo."
  (postmodern:query
   (:limit
    (:order-by
     (:select '* :from 'cave-automation-runs
      :where (:= 'repo-id repo-id))
     (:desc 'created-at))
    limit)
   :plists))

(defun find-automation-run (run-id)
  "Find an automation run by ID."
  (postmodern:query
   (:select '* :from 'cave-automation-runs :where (:= 'id run-id))
   :plist))

(defun update-run-status (run-id status &key runner-id)
  "Update an automation run's status."
  (cond
    ((equal status "running")
     (postmodern:execute
      (:update 'cave-automation-runs
       :set 'status status 'started-at (:now)
            'runner-id (or runner-id :null)
       :where (:= 'id run-id))))
    ((member status '("success" "failure" "cancelled" "timed_out") :test #'equal)
     (postmodern:execute
      (:update 'cave-automation-runs
       :set 'status status 'finished-at (:now)
       :where (:= 'id run-id))))
    (t
     (postmodern:execute
      (:update 'cave-automation-runs
       :set 'status status
       :where (:= 'id run-id))))))

(defun update-run-status-for-runner (run-id runner-id status)
  "Update an automation run only if it is assigned to RUNNER-ID."
  (cond
    ((equal status "running")
     (postmodern:query
      (:update 'cave-automation-runs
       :set 'status status 'started-at (:now)
       :where (:and (:= 'id run-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))
    ((member status '("success" "failure" "cancelled" "timed_out") :test #'equal)
     (postmodern:query
      (:update 'cave-automation-runs
       :set 'status status 'finished-at (:now)
       :where (:and (:= 'id run-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))
    (t
     (postmodern:query
      (:update 'cave-automation-runs
       :set 'status status
       :where (:and (:= 'id run-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))))

(defun append-run-log (run-id chunk)
  "Append a log chunk to an automation run."
  (postmodern:execute
   (:update 'cave-automation-runs
    :set 'log (:|| 'log chunk)
    :where (:= 'id run-id))))

(defun append-run-log-for-runner (run-id runner-id chunk)
  "Append a log chunk only if the automation run is assigned to RUNNER-ID."
  (postmodern:query
   (:update 'cave-automation-runs
    :set 'log (:|| 'log chunk)
    :where (:and (:= 'id run-id) (:= 'runner-id runner-id))
    :returning '*)
   :plist))

(defun fetch-queued-run (runner-id runner-labels runner-scope runner-scope-id)
  "Atomically fetch and assign a queued run to a runner.
   Respects one-task-per-runner policy. Returns run plist or NIL."
  ;; Check if runner already has an active task
  (let ((active (postmodern:query
                 (:select 'id :from 'cave-automation-runs
                  :where (:and (:= 'runner-id runner-id)
                               (:in 'status (:set "assigned" "running"))))
                 :single)))
    (when active (return-from fetch-queued-run nil)))
  ;; Fetch oldest queued run matching runner scope
  ;; instance runners: any run; org runners: repos in their org; repo runners: their repo only
  (let ((run (cond
               ((equal runner-scope "repo")
                (postmodern:query
                 (:limit (:order-by
                           (:select '* :from 'cave-automation-runs
                            :where (:and (:= 'status "queued")
                                         (:= 'repo-id runner-scope-id)))
                           'created-at) 1)
                 :plist))
               ((equal runner-scope "org")
                (postmodern:query
                 (:limit (:order-by
                           (:select '* :from 'cave-automation-runs
                            :where (:and (:= 'status "queued")
                                         (:in 'repo-id
                                              (:select 'id :from 'cave-repos
                                               :where (:= 'org-id runner-scope-id)))))
                           'created-at) 1)
                 :plist))
               ((equal runner-scope "user")
                (postmodern:query
                 (:limit (:order-by
                           (:select '* :from 'cave-automation-runs
                            :where (:and (:= 'status "queued")
                                         (:in 'repo-id
                                              (:select 'id :from 'cave-repos
                                               :where (:= 'owner-id runner-scope-id)))))
                           'created-at) 1)
                 :plist))
               (t
                (postmodern:query
                 (:limit (:order-by
                           (:select '* :from 'cave-automation-runs
                            :where (:= 'status "queued"))
                           'created-at) 1)
                 :plist)))))
    (when run
      ;; Atomically assign it
      (let ((updated (postmodern:query
                      (:update 'cave-automation-runs
                       :set 'status "assigned" 'runner-id runner-id
                       :where (:and (:= 'id (getf run :id))
                                    (:= 'status "queued"))
                       :returning '*)
                      :plist)))
        updated))))

;;; ========================== RUNNERS ==========================

(defun register-runner (&key name labels ephemeral scope scope-id)
  "Register a new runner. Returns the runner plist with auth token."
  (let ((token (format nil "cavr_~A"
                       (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))))
    (postmodern:query
     (:insert-into 'cave-runners
      :set 'name name
           'labels (or labels "")
           'ephemeral (if ephemeral t nil)
           'scope (or scope "instance")
           'scope-id (or scope-id :null)
           'auth-token token
           'status "online"
           'last-seen-at (:now)
      :returning '*)
     :plist)))

(defun authenticate-runner (auth-token)
  "Validate a runner auth token. Returns runner plist or NIL."
  (when auth-token
    (postmodern:query
     (:select '* :from 'cave-runners
      :where (:and (:= 'auth-token auth-token)
                   (:not (:= 'status "disabled"))))
     :plist)))

(defun update-runner-heartbeat (runner-id &key labels)
  "Update runner last-seen and optionally labels."
  (if labels
      (postmodern:execute
       (:update 'cave-runners
        :set 'last-seen-at (:now) 'status "online" 'labels labels
        :where (:= 'id runner-id)))
      (postmodern:execute
       (:update 'cave-runners
        :set 'last-seen-at (:now) 'status "online"
        :where (:= 'id runner-id)))))

(defun list-runners (&key scope scope-id)
  "List runners, optionally filtered by scope and scope-id."
  (if scope
      (postmodern:query
       (:order-by (:select '* :from 'cave-runners
                   :where (:and (:= 'scope scope)
                                (:= 'scope-id scope-id)))
                  'name)
       :plists)
      (postmodern:query
       (:order-by (:select '* :from 'cave-runners) 'name)
       :plists)))

(defun disable-runner (runner-id)
  "Disable a runner."
  (postmodern:execute
   (:update 'cave-runners :set 'status "disabled" :where (:= 'id runner-id))))

(defun delete-runner (runner-id)
  "Delete a runner."
  (postmodern:execute
   (:delete-from 'cave-runners :where (:= 'id runner-id))))

(defun cleanup-stale-ephemeral-runners ()
  "Delete ephemeral runners that haven't heartbeated in 2 minutes."
  (postmodern:execute
   (:delete-from 'cave-runners
    :where (:and (:= 'ephemeral t)
                 (:or (:is-null 'last-seen-at)
                      (:<= 'last-seen-at
                           (:- (:now) (:raw "INTERVAL '2 minutes'"))))))))

(defun cleanup-offline-runners ()
  "Mark runners as offline if no heartbeat in 60 seconds. Delete stale ephemeral ones."
  (postmodern:execute
   (:update 'cave-runners
    :set 'status "offline"
    :where (:and (:= 'status "online")
                 (:<= 'last-seen-at
                      (:- (:now) (:raw "INTERVAL '60 seconds'"))))))
  (cleanup-stale-ephemeral-runners)
  ;; Delete offline runners (they re-register on reconnect)
  (postmodern:execute
   (:delete-from 'cave-runners :where (:= 'status "offline")))
  ;; Reset workflow jobs assigned to offline runners
  (postmodern:execute
   (:update 'cave-workflow-jobs
    :set 'status "queued" 'runner-id :null
    :where (:and (:= 'status "assigned")
                 (:in 'runner-id (:select 'id :from 'cave-runners
                                  :where (:= 'status "offline"))))))
  ;; Reset automation runs assigned to offline runners
  (postmodern:execute
   (:update 'cave-automation-runs
    :set 'status "queued" 'runner-id :null
    :where (:and (:= 'status "assigned")
                 (:in 'runner-id (:select 'id :from 'cave-runners
                                  :where (:= 'status "offline")))))))

(defun create-registration-token (&key scope scope-id created-by-id)
  "Create a runner registration token."
  (let ((token (format nil "cavrt_~A"
                       (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))))
    (postmodern:query
     (:insert-into 'cave-runner-registration-tokens
      :set 'token token
           'scope (or scope "instance")
           'scope-id (or scope-id :null)
           'created-by-id (or created-by-id :null)
           'expires-at (:+ (:now) (:raw "'24 hours'"))
      :returning '*)
     :plist)))

(defun validate-registration-token (token)
  "Validate a registration token without consuming it. Returns the token record
   or NIL. Prefer CONSUME-REGISTRATION-TOKEN on the registration path."
  (when token
    (postmodern:query
     (:select '* :from 'cave-runner-registration-tokens
      :where (:and (:= 'token token)
                   (:or (:is-null 'expires-at)
                        (:> 'expires-at (:now)))))
     :plist)))

(defun consume-registration-token (token)
  "Atomically validate and CONSUME a registration token: the row is deleted and
   returned in a single statement, so a token registers exactly one runner
   (single-use) even under concurrent RegisterRunner calls. Returns the token
   record, or NIL if the token is unknown, expired, or already used."
  (when token
    (postmodern:query
     (:delete-from 'cave-runner-registration-tokens
      :where (:and (:= 'token token)
                   (:or (:is-null 'expires-at)
                        (:> 'expires-at (:now))))
      :returning '*)
     :plist)))

;;; ========================== WORKFLOWS ==========================

(defun reap-stale-workflow-jobs (&key (max-minutes 120))
  "Fail workflow jobs (and finalize their runs as failed) that have been
   running/assigned past MAX-MINUTES, or whose assigned runner is gone/offline
   (runners get a new id on restart, so a dead runner's jobs never report).
   Without this a runner that dies mid-job leaves the run 'running' forever,
   which blocks merges on required checks. Returns the number of runs reaped."
  (let ((rows (postmodern:query
               "SELECT j.id AS jid, j.workflow_run_id AS rid
                FROM cave_workflow_jobs j
                JOIN cave_workflow_runs w ON w.id = j.workflow_run_id
                LEFT JOIN cave_runners r ON r.id = j.runner_id
                WHERE j.status IN ('running','assigned')
                  AND (w.started_at < now() - make_interval(mins => $1::int)
                       OR r.id IS NULL
                       OR r.status <> 'online'
                       OR r.last_seen_at < now() - interval '3 minutes')"
               max-minutes :plists))
        (runs '()))
    (dolist (row rows)
      (update-job-status (getf row :jid) "failure")
      (pushnew (getf row :rid) runs))
    (dolist (rid runs)
      (update-workflow-run-status rid "failure"))
    (length runs)))

(defun create-workflow-run (&key repo-id workflow-name workflow-file trigger-event
                                 commit-sha ref triggered-by-id)
  "Create a workflow run."
  (postmodern:query
   (:insert-into 'cave-workflow-runs
    :set 'repo-id repo-id
         'workflow-name workflow-name
         'workflow-file workflow-file
         'trigger-event trigger-event
         'commit-sha (or commit-sha :null)
         'ref (or ref :null)
         'triggered-by-id (or triggered-by-id :null)
    :returning '*)
   :plist))

(defun list-workflow-runs (repo-id &key (limit 30))
  "List recent workflow runs for a repo."
  (postmodern:query
   (:limit (:order-by (:select '* :from 'cave-workflow-runs
                        :where (:= 'repo-id repo-id))
                       (:desc 'created-at))
           limit)
   :plists))

(defun find-workflow-run (run-id)
  "Find a workflow run by ID."
  (postmodern:query
   (:select '* :from 'cave-workflow-runs :where (:= 'id run-id))
   :plist))

(defun update-workflow-run-status (run-id status)
  "Update workflow run status."
  (cond
    ((equal status "running")
     (postmodern:execute
      (:update 'cave-workflow-runs
       :set 'status status 'started-at (:now)
       :where (:= 'id run-id))))
    ((member status '("success" "failure" "cancelled") :test #'equal)
     (postmodern:execute
      (:update 'cave-workflow-runs
       :set 'status status 'finished-at (:now)
       :where (:= 'id run-id))))
    (t
     (postmodern:execute
      (:update 'cave-workflow-runs
       :set 'status status
       :where (:= 'id run-id))))))

(defun create-workflow-job (&key workflow-run-id name image needs runs-on
                                (timeout-seconds 0) continue-on-error privileged)
  "Create a workflow job. NEEDS is a list of job name strings.
   RUNS-ON is a list of label strings the runner must have.
   TIMEOUT-SECONDS is the max job duration (0 means use default).
   CONTINUE-ON-ERROR when true prevents dependent jobs from being skipped on failure.
   PRIVILEGED when true runs the job container with --privileged (nested containers)."
  (let ((needs-str (if needs (format nil "~{~A~^,~}" needs) ""))
        (runs-on-str (if runs-on (format nil "~{~A~^,~}" runs-on) "")))
    (postmodern:query
     (:insert-into 'cave-workflow-jobs
      :set 'workflow-run-id workflow-run-id
           'name name
           'image image
           'needs needs-str
           'runs-on runs-on-str
           'timeout-seconds (or timeout-seconds 0)
           'continue-on-error (if continue-on-error t nil)
           'privileged (if privileged t nil)
           'status (if needs "blocked" "queued")
      :returning '*)
     :plist)))

(defun list-workflow-jobs (workflow-run-id)
  "List all jobs for a workflow run."
  (postmodern:query
   (:order-by (:select '* :from 'cave-workflow-jobs
                :where (:= 'workflow-run-id workflow-run-id))
              'created-at)
   :plists))

(defun update-job-status (job-id status &key runner-id)
  "Update a workflow job's status."
  (cond
    ((equal status "running")
     (postmodern:execute
      (:update 'cave-workflow-jobs
       :set 'status status 'started-at (:now)
            'runner-id (or runner-id :null)
       :where (:= 'id job-id))))
    ((member status '("success" "failure" "cancelled" "skipped") :test #'equal)
     (postmodern:execute
      (:update 'cave-workflow-jobs
       :set 'status status 'finished-at (:now)
       :where (:= 'id job-id))))
    (t
     (postmodern:execute
      (:update 'cave-workflow-jobs
       :set 'status status
       :where (:= 'id job-id))))))

(defun update-job-status-for-runner (job-id runner-id status)
  "Update a workflow job only if it is assigned to RUNNER-ID."
  (cond
    ((equal status "running")
     (postmodern:query
      (:update 'cave-workflow-jobs
       :set 'status status 'started-at (:now)
       :where (:and (:= 'id job-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))
    ((member status '("success" "failure" "cancelled" "skipped") :test #'equal)
     (postmodern:query
      (:update 'cave-workflow-jobs
       :set 'status status 'finished-at (:now)
       :where (:and (:= 'id job-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))
    (t
     (postmodern:query
      (:update 'cave-workflow-jobs
       :set 'status status
       :where (:and (:= 'id job-id) (:= 'runner-id runner-id))
       :returning '*)
      :plist))))

(defun fetch-queued-workflow-job (runner-id runner-labels runner-scope runner-scope-id)
  "Fetch a queued workflow job whose dependencies are met.
   Respects runner scope, label matching, and one-task-per-runner policy.
   RUNNER-LABELS is the runner's comma-separated label string."
  ;; Check if runner already has an active task (automation or workflow)
  (let ((active-auto (postmodern:query
                      (:select 'id :from 'cave-automation-runs
                       :where (:and (:= 'runner-id runner-id)
                                    (:in 'status (:set "assigned" "running"))))
                      :single))
        (active-job (postmodern:query
                     (:select 'id :from 'cave-workflow-jobs
                      :where (:and (:= 'runner-id runner-id)
                                   (:in 'status (:set "assigned" "running"))))
                     :single)))
    (when (or active-auto active-job)
      (return-from fetch-queued-workflow-job nil)))
  ;; Find a queued job with all dependencies met and labels matched.
  ;; Label matching: if runs_on is empty, any runner can take it.
  ;; If runs_on is set, every required label must appear in the runner's labels.
  (let* ((safe-labels (if (and runner-labels (not (equal runner-labels "")))
                         (postmodern:sql-escape-string runner-labels)
                         "''"))
         (job (postmodern:query
               (format nil "SELECT wj.* FROM cave_workflow_jobs wj ~
                 WHERE wj.status = 'queued' ~
                   AND NOT EXISTS ( ~
                     SELECT 1 FROM cave_workflow_jobs dep ~
                     WHERE dep.workflow_run_id = wj.workflow_run_id ~
                       AND dep.name = ANY(string_to_array(wj.needs, ',')) ~
                       AND dep.status != 'success') ~
                   AND (wj.runs_on = '' OR string_to_array(wj.runs_on, ',') <@ string_to_array(~A, ',')) ~
                   ~A ~
                 ORDER BY wj.created_at LIMIT 1"
                       safe-labels
                       (cond
                         ((equal runner-scope "repo")
                          (format nil "AND wj.workflow_run_id IN (SELECT id FROM cave_workflow_runs WHERE repo_id = ~A)"
                                  runner-scope-id))
                         ((equal runner-scope "org")
                          (format nil "AND wj.workflow_run_id IN (SELECT id FROM cave_workflow_runs WHERE repo_id IN (SELECT id FROM cave_repos WHERE org_id = ~A))"
                                  runner-scope-id))
                         ((equal runner-scope "user")
                          (format nil "AND wj.workflow_run_id IN (SELECT id FROM cave_workflow_runs WHERE repo_id IN (SELECT id FROM cave_repos WHERE owner_id = ~A))"
                                  runner-scope-id))
                         (t "")))
               :plist)))
    (when job
      ;; Atomically assign
      (postmodern:query
       (:update 'cave-workflow-jobs
        :set 'status "assigned" 'runner-id runner-id
        :where (:and (:= 'id (getf job :id))
                     (:= 'status "queued"))
        :returning '*)
       :plist))))

(defun requeue-workflow-job (job-id)
  "Return an ASSIGNED job to the queue — used when delivering the task to the
   runner failed (dead stream). Otherwise the job is wedged 'assigned' to a
   runner that never received it, which also blocks that runner forever via the
   one-task-per-runner check."
  (postmodern:execute
   (:update 'cave-workflow-jobs
    :set 'status "queued" 'runner-id :null
    :where (:and (:= 'id job-id) (:= 'status "assigned")))))

(defun requeue-automation-run (run-id)
  "Return an ASSIGNED automation run to the queue on failed task delivery."
  (postmodern:execute
   (:update 'cave-automation-runs
    :set 'status "queued" 'runner-id :null
    :where (:and (:= 'id run-id) (:= 'status "assigned")))))

(defun create-workflow-step (&key job-id step-order name command (timeout-seconds 0) continue-on-error)
  "Create a workflow step. TIMEOUT-SECONDS is the max step duration (0 means no limit).
   CONTINUE-ON-ERROR when true allows the job to proceed even if this step fails."
  (postmodern:query
   (:insert-into 'cave-workflow-steps
    :set 'job-id job-id
         'step-order step-order
         'name (or name :null)
         'command command
         'timeout-seconds (or timeout-seconds 0)
         'continue-on-error (if continue-on-error t nil)
    :returning '*)
   :plist))

(defun list-workflow-steps (job-id)
  "List all steps for a job, ordered."
  (postmodern:query
   (:order-by (:select '* :from 'cave-workflow-steps
                :where (:= 'job-id job-id))
              'step-order)
   :plists))

(defun update-step-status (step-id status &key exit-code)
  "Update a workflow step's status."
  (cond
    ((equal status "running")
     (postmodern:execute
      (:update 'cave-workflow-steps
       :set 'status status 'started-at (:now)
       :where (:= 'id step-id))))
    ((member status '("success" "failure" "skipped") :test #'equal)
     (postmodern:execute
      (:update 'cave-workflow-steps
       :set 'status status 'finished-at (:now)
            'exit-code (or exit-code :null)
       :where (:= 'id step-id))))
    (t
     (postmodern:execute
      (:update 'cave-workflow-steps
       :set 'status status
       :where (:= 'id step-id))))))

(defun update-step-status-for-runner (step-id runner-id status &key exit-code)
  "Update a workflow step only if its job is assigned to RUNNER-ID."
  (cond
    ((equal status "running")
     (postmodern:query
      (:update 'cave-workflow-steps
       :set 'status status 'started-at (:now)
       :where (:and (:= 'id step-id)
                    (:in 'job-id
                         (:select 'id :from 'cave-workflow-jobs
                          :where (:= 'runner-id runner-id))))
       :returning '*)
      :plist))
    ((member status '("success" "failure" "skipped") :test #'equal)
     (postmodern:query
      (:update 'cave-workflow-steps
       :set 'status status 'finished-at (:now)
            'exit-code (or exit-code :null)
       :where (:and (:= 'id step-id)
                    (:in 'job-id
                         (:select 'id :from 'cave-workflow-jobs
                          :where (:= 'runner-id runner-id))))
       :returning '*)
      :plist))
    (t
     (postmodern:query
      (:update 'cave-workflow-steps
       :set 'status status
       :where (:and (:= 'id step-id)
                    (:in 'job-id
                         (:select 'id :from 'cave-workflow-jobs
                          :where (:= 'runner-id runner-id))))
       :returning '*)
      :plist))))

(defun append-step-log (step-id chunk)
  "Append log text to a workflow step."
  (postmodern:execute
   (:update 'cave-workflow-steps
    :set 'log (:|| 'log chunk)
    :where (:= 'id step-id))))

(defun append-step-log-for-runner (step-id runner-id chunk)
  "Append step log text only if the step's job is assigned to RUNNER-ID."
  (postmodern:query
   (:update 'cave-workflow-steps
    :set 'log (:|| 'log chunk)
    :where (:and (:= 'id step-id)
                 (:in 'job-id
                      (:select 'id :from 'cave-workflow-jobs
                       :where (:= 'runner-id runner-id))))
    :returning '*)
   :plist))

;;; ========================== WEBHOOKS ==========================

(defun create-webhook (&key repo-id url secret events)
  "Create a webhook config."
  (postmodern:query
   (:insert-into 'cave-webhooks
    :set 'repo-id repo-id 'url url
         'secret (or secret :null)
         'events (or events "push,pull_request,issue")
    :returning '*)
   :plist))

(defun list-webhooks (repo-id)
  "List all webhooks for a repo."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-webhooks :where (:= 'repo-id repo-id))
    'created-at)
   :plists))

(defun delete-webhook (webhook-id repo-id)
  "Delete a webhook."
  (postmodern:execute
   (:delete-from 'cave-webhooks
    :where (:and (:= 'id webhook-id) (:= 'repo-id repo-id)))))

(defun update-webhook-status (webhook-id status &optional error)
  "Update last delivery status."
  (postmodern:execute
   (:update 'cave-webhooks
    :set 'last-status status 'last-error (or error :null)
    :where (:= 'id webhook-id))))

(defun list-repo-webhooks-for-event (repo-id event)
  "List enabled webhooks for a repo that subscribe to EVENT."
  (postmodern:query
   (:select '* :from 'cave-webhooks
    :where (:and (:= 'repo-id repo-id)
                 (:= 'enabled t)
                 (:like 'events (format nil "%~A%" event))))
   :plists))

;;; ========================== COMMIT STATUSES ==========================

(defun set-commit-status (&key repo-id commit-sha state context description target-url)
  "Set or update a commit status. Upserts by (repo_id, commit_sha, context)."
  (let ((existing (postmodern:query
                   (:select 'id :from 'cave-commit-statuses
                    :where (:and (:= 'repo-id repo-id)
                                 (:= 'commit-sha commit-sha)
                                 (:= 'context (or context "default"))))
                   :single)))
    (if existing
        (progn
          (postmodern:execute
           (:update 'cave-commit-statuses
            :set 'state state
                 'description (or description :null)
                 'target-url (or target-url :null)
                 'updated-at (:now)
            :where (:= 'id existing)))
          (postmodern:query
           (:select '* :from 'cave-commit-statuses :where (:= 'id existing))
           :plist))
        (postmodern:query
         (:insert-into 'cave-commit-statuses
          :set 'repo-id repo-id
               'commit-sha commit-sha
               'state state
               'context (or context "default")
               'description (or description :null)
               'target-url (or target-url :null)
          :returning '*)
         :plist))))

(defun list-commit-statuses (repo-id commit-sha)
  "List all statuses for a commit."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-commit-statuses
     :where (:and (:= 'repo-id repo-id) (:= 'commit-sha commit-sha)))
    'context)
   :plists))

(defun combined-commit-status (repo-id commit-sha)
  "Get the combined status for a commit. Returns :success, :pending, :failure, or NIL."
  (let ((statuses (list-commit-statuses repo-id commit-sha)))
    (cond
      ((null statuses) nil)
      ((every (lambda (s) (equal (getf s :state) "success")) statuses) :success)
      ((some (lambda (s) (member (getf s :state) '("failure" "error") :test #'equal)) statuses) :failure)
      (t :pending))))

;;; ========================== USER THEMES ==========================

(defun set-user-theme (user-id theme-name)
  "Set the active theme for a user."
  (postmodern:execute
   (:update 'cave-users :set 'theme theme-name :where (:= 'id user-id))))

(defun list-user-themes (user-id)
  "List custom themes for a user."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-user-themes :where (:= 'user-id user-id))
    'name)
   :plists))

(defun upsert-user-theme (user-id name definition)
  "Create or update a custom theme for a user."
  (let ((existing (postmodern:query
                   (:select 'id :from 'cave-user-themes
                    :where (:and (:= 'user-id user-id) (:= 'name name)))
                   :single)))
    (if existing
        (postmodern:execute
         (:update 'cave-user-themes
          :set 'definition definition 'updated-at (:now)
          :where (:= 'id existing)))
        (postmodern:execute
         (:insert-into 'cave-user-themes
          :set 'user-id user-id 'name name 'definition definition)))))

(defun get-user-theme-css (user-id theme-name)
  "Get a custom theme's CSS definition. Returns string or NIL."
  (postmodern:query
   (:select 'definition :from 'cave-user-themes
    :where (:and (:= 'user-id user-id) (:= 'name theme-name)))
   :single))

;;; ========================== MIRRORS ==========================

(defun create-mirror (&key repo-id direction remote-url auth-token interval-minutes)
  "Create a mirror config."
  (postmodern:query
   (:insert-into 'cave-repo-mirrors
    :set 'repo-id repo-id
         'direction direction
         'remote-url remote-url
         'auth-token (or auth-token :null)
         'interval-minutes (or interval-minutes 60)
    :returning '*)
   :plist))

(defun list-mirrors (repo-id)
  "List all mirrors for a repo."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-repo-mirrors
     :where (:= 'repo-id repo-id))
    'direction 'created-at)
   :plists))

(defun pull-mirror-repo-p (repo-id)
  "True when REPO-ID is a pull-mirror of an upstream. Its bare repo is kept in
   sync with `git fetch --prune +refs/*:refs/*`, which deletes any local-only
   ref — so a cave-bot fix branch is pruned within a sync cycle and the fix
   could never land anyway. Used to skip auto-fix PR creation on such repos."
  (plusp (or (postmodern:query
              (:select (:count '*) :from 'cave-repo-mirrors
               :where (:and (:= 'repo-id repo-id) (:= 'direction "pull")))
              :single)
             0)))

(defun delete-mirror (mirror-id repo-id)
  "Delete a mirror config."
  (postmodern:execute
   (:delete-from 'cave-repo-mirrors
    :where (:and (:= 'id mirror-id) (:= 'repo-id repo-id)))))

(defun list-due-pull-mirrors ()
  "List pull mirrors that are due for sync."
  (postmodern:query
   (:select 'cave-repo-mirrors.* 'cave-repos.name
            (:as (:raw "COALESCE((SELECT username FROM cave_users WHERE id = cave_repos.owner_id), (SELECT name FROM cave_orgs WHERE id = cave_repos.org_id))") 'owner-name)
    :from 'cave-repo-mirrors
    :inner-join 'cave-repos :on (:= 'cave-repo-mirrors.repo-id 'cave-repos.id)
    :where (:and (:= 'cave-repo-mirrors.direction "pull")
                 (:= 'cave-repo-mirrors.enabled t)
                 (:or (:is-null 'cave-repo-mirrors.last-sync-at)
                      (:<= 'cave-repo-mirrors.last-sync-at
                           (:- (:now) (:raw "make_interval(mins => cave_repo_mirrors.interval_minutes)"))))))
   :plists))

(defun update-mirror-sync (mirror-id &key error)
  "Update last sync time and optionally record an error."
  (postmodern:execute
   (:update 'cave-repo-mirrors
    :set 'last-sync-at (:now)
         'last-error (or error :null)
    :where (:= 'id mirror-id))))

;;; ========================== CHECK CONFIGS ==========================

(defun list-check-configs (repo-id)
  "List all check configs for a repo."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-check-configs
     :where (:= 'repo-id repo-id))
    'name)
   :plists))

(defun create-check-config (&key repo-id name command timeout-seconds)
  "Create a check config."
  (postmodern:query
   (:insert-into 'cave-check-configs
    :set 'repo-id repo-id
         'name name
         'command command
         'timeout-seconds (or timeout-seconds 60)
    :returning '*)
   :plist))

(defun delete-check-config (config-id repo-id)
  "Delete a check config."
  (postmodern:execute
   (:delete-from 'cave-check-configs
    :where (:and (:= 'id config-id) (:= 'repo-id repo-id)))))

;;; ========================== REPO NUMBERS ==========================

(defun next-repo-number (repo-id)
  "Atomically get and increment the next number for a repo (shared by issues and pull requests)."
  (let ((result (postmodern:query
                 (:update 'cave-repos
                  :set 'next-number (:+ 'next-number 1)
                  :where (:= 'id repo-id)
                  :returning 'next-number)
                 :single)))
    ;; Returns the NEW value, so the assigned number is result - 1
    (1- result)))

;;; ========================== ISSUES ==========================

(defun create-issue (&key repo-id author-id title body)
  "Create a new issue."
  (let ((number (next-repo-number repo-id)))
    (postmodern:query
     (:insert-into 'cave-issues
      :set 'repo-id repo-id
           'number number
           'author-id author-id
           'title title
           'body (or body :null)
      :returning '*)
     :plist)))

(defun find-issue (repo-id number)
  "Find an issue by repo and number."
  (postmodern:query
   (:select '* :from 'cave-issues
    :where (:and (:= 'repo-id repo-id) (:= 'number number)))
   :plist))

(defun claim-scheduled-task (name interval-seconds)
  "Atomically claim periodic task NAME if it hasn't run within INTERVAL-SECONDS.
   Stamps last_run_at and returns T for the one caller that should run it now
   (so even multiple app instances never double-run), else NIL."
  (postmodern:execute
   "INSERT INTO cave_scheduler_runs (task_name) VALUES ($1) ON CONFLICT DO NOTHING"
   name)
  (and (postmodern:query
        "UPDATE cave_scheduler_runs SET last_run_at = NOW()
         WHERE task_name = $1
           AND (last_run_at IS NULL OR last_run_at < NOW() - make_interval(secs => $2::int))
         RETURNING task_name"
        name interval-seconds :single)
       t))

(defun count-open-issues (repo-id)
  "Number of open issues in REPO-ID."
  (or (postmodern:query
       "SELECT count(*) FROM cave_issues WHERE repo_id = $1 AND status = 'open'"
       repo-id :single)
      0))

(defun count-open-changesets (repo-id)
  "Number of open (not merged, not closed) pull requests in REPO-ID."
  (or (postmodern:query
       "SELECT count(*) FROM cave_changesets
        WHERE repo_id = $1 AND NOT is_merged AND NOT is_closed"
       repo-id :single)
      0))

(defun list-issues (repo-id &key (status "open") (limit 50) (offset 0))
  "List issues with optional status filter."
  (if status
      (postmodern:query
       (:limit
        (:order-by
         (:select '* :from 'cave-issues
          :where (:and (:= 'repo-id repo-id) (:= 'status status)))
         (:desc 'created-at))
        limit offset)
       :plists)
      (postmodern:query
       (:limit
        (:order-by
         (:select '* :from 'cave-issues
          :where (:= 'repo-id repo-id))
         (:desc 'created-at))
        limit offset)
       :plists)))

(defun update-issue (issue-id &key title body status)
  "Update an issue."
  (when title
    (postmodern:execute
     (:update 'cave-issues :set 'title title 'updated-at (:now)
      :where (:= 'id issue-id))))
  (when body
    (postmodern:execute
     (:update 'cave-issues :set 'body body 'updated-at (:now)
      :where (:= 'id issue-id))))
  (when status
    (if (equal status "closed")
        (postmodern:execute
         (:update 'cave-issues
          :set 'status status 'closed-at (:now) 'updated-at (:now)
          :where (:= 'id issue-id)))
        (postmodern:execute
         (:update 'cave-issues
          :set 'status status 'updated-at (:now)
          :where (:= 'id issue-id))))))

;;; ========================== ISSUE COMMENTS ==========================

(defun create-issue-comment (&key issue-id author-id body)
  "Create a comment on an issue. Returns the comment plist."
  (postmodern:query
   (:insert-into 'cave-issue-comments
    :set 'issue-id issue-id
         'author-id author-id
         'body body
    :returning '*)
   :plist))

(defun list-issue-comments (issue-id)
  "List all comments on an issue, oldest first, with author usernames."
  (postmodern:query
   (:order-by
    (:select 'cave-issue-comments.* 'cave-users.username 'cave-users.email
     :from 'cave-issue-comments
     :inner-join 'cave-users :on (:= 'cave-issue-comments.author-id 'cave-users.id)
     :where (:= 'cave-issue-comments.issue-id issue-id))
    'cave-issue-comments.created-at)
   :plists))

;;; ========================== DIFF COMMENTS ==========================

(defun create-diff-comment (&key changeset-id author-id file-path line-number side body)
  "Create an inline diff comment. Returns the comment plist."
  (postmodern:query
   (:insert-into 'cave-diff-comments
    :set 'changeset-id changeset-id
         'author-id author-id
         'file-path file-path
         'line-number line-number
         'side (or side "new")
         'body body
    :returning '*)
   :plist))

(defun list-diff-comments (changeset-id)
  "List all inline diff comments for a PR, with author usernames."
  (postmodern:query
   (:order-by
    (:select 'cave-diff-comments.* 'cave-users.username
     :from 'cave-diff-comments
     :inner-join 'cave-users :on (:= 'cave-diff-comments.author-id 'cave-users.id)
     :where (:= 'cave-diff-comments.changeset-id changeset-id))
    'cave-diff-comments.file-path
    'cave-diff-comments.line-number
    'cave-diff-comments.created-at)
   :plists))

(defun group-diff-comments (comments)
  "Group diff comments into a hash table keyed by \"file:line:side\"."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (c comments)
      (let ((key (format nil "~A:~A:~A"
                         (getf c :file-path)
                         (getf c :line-number)
                         (getf c :side))))
        (push c (gethash key table))))
    ;; Reverse each list so comments are in chronological order
    (maphash (lambda (k v) (setf (gethash k table) (nreverse v))) table)
    table))

;;; ========================== PULL REQUESTS ==========================

(defun create-pull-request (&key repo-id author-id source-branch target-branch head-commit
                               stack-id stack-order)
  "Create a new pull request."
  (let ((number (next-repo-number repo-id)))
    (postmodern:query
     (:insert-into 'cave-changesets
      :set 'repo-id repo-id
           'number number
           'author-id author-id
           'source-branch source-branch
           'target-branch target-branch
           'head-commit head-commit
           'stack-id (or stack-id :null)
           'stack-order (or stack-order :null)
      :returning '*)
     :plist)))

(defun find-pull-request (repo-id number)
  "Find a pull request by repo and number."
  (postmodern:query
   (:select '* :from 'cave-changesets
    :where (:and (:= 'repo-id repo-id) (:= 'number number)))
   :plist))

(defun find-pull-request-by-id (changeset-id)
  "Find a pull request by ID."
  (postmodern:query
   (:select '* :from 'cave-changesets :where (:= 'id changeset-id))
   :plist))

(defun find-pull-request-by-branch (repo-id source-branch)
  "Find an open pull request for a source branch."
  (postmodern:query
   (:select '* :from 'cave-changesets
    :where (:and (:= 'repo-id repo-id)
                 (:= 'source-branch source-branch)
                 (:= 'is-merged nil)
                 (:= 'is-closed nil)))
   :plist))

(defun list-pull-requests (repo-id &key (status "open") (limit 50) (offset 0))
  "List pull requests. Status: open, merged, closed, or nil for all."
  (cond
    ((equal status "open")
     (postmodern:query
      (:limit (:order-by
               (:select '* :from 'cave-changesets
                :where (:and (:= 'repo-id repo-id)
                             (:= 'is-merged nil) (:= 'is-closed nil)))
               (:desc 'created-at))
              limit offset)
      :plists))
    ((equal status "merged")
     (postmodern:query
      (:limit (:order-by
               (:select '* :from 'cave-changesets
                :where (:and (:= 'repo-id repo-id) (:= 'is-merged t)))
               (:desc 'merged-at))
              limit offset)
      :plists))
    ((equal status "closed")
     (postmodern:query
      (:limit (:order-by
               (:select '* :from 'cave-changesets
                :where (:and (:= 'repo-id repo-id) (:= 'is-closed t)))
               (:desc 'closed-at))
              limit offset)
      :plists))
    (t
     (postmodern:query
      (:limit (:order-by
               (:select '* :from 'cave-changesets
                :where (:= 'repo-id repo-id))
               (:desc 'created-at))
              limit offset)
      :plists))))

(defun update-pull-request-head (changeset-id head-commit)
  "Update the head commit and bump version."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'head-commit head-commit
         'version (:+ 'version 1)
         'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun close-pull-request (changeset-id)
  "Mark a pull request as closed."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-closed t 'closed-at (:now) 'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun merge-pull-request (changeset-id)
  "Mark a pull request as merged."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-merged t 'merged-at (:now) 'updated-at (:now)
    :where (:= 'id changeset-id))))

;;; ========================== STACKS ==========================

(defun create-stack (&key repo-id name base-branch)
  "Create a stack."
  (postmodern:query
   (:insert-into 'cave-stacks
    :set 'repo-id repo-id 'name name 'base-branch base-branch
    :returning '*)
   :plist))

(defun find-stack (repo-id name)
  "Find a stack by name."
  (postmodern:query
   (:select '* :from 'cave-stacks
    :where (:and (:= 'repo-id repo-id) (:= 'name name)))
   :plist))

(defun find-stack-by-id (stack-id)
  "Find a stack by ID."
  (when stack-id
    (postmodern:query
     (:select '* :from 'cave-stacks :where (:= 'id stack-id))
     :plist)))

(defun list-stack-pull-requests (stack-id)
  "List all pull requests in a stack, ordered by stack_order."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-changesets :where (:= 'stack-id stack-id))
    'stack-order)
   :plists))

;;; ========================== REVIEWS ==========================

(defun create-review (&key changeset-id reviewer-id state body changeset-version)
  "Create a review."
  (postmodern:query
   (:insert-into 'cave-reviews
    :set 'changeset-id changeset-id
         'reviewer-id reviewer-id
         'state state
         'body (or body :null)
         'changeset-version changeset-version
    :returning '*)
   :plist))

(defun list-reviews (changeset-id)
  "List all reviews for a changeset, newest first."
  (postmodern:query
   (:order-by
    (:select 'cave-reviews.* (:as 'cave-users.username 'reviewer-username) (:as 'cave-users.email 'reviewer-email)
     :from 'cave-reviews
     :inner-join 'cave-users :on (:= 'cave-reviews.reviewer-id 'cave-users.id)
     :where (:= 'cave-reviews.changeset-id changeset-id))
    (:desc 'cave-reviews.created-at))
   :plists))

(defun review-is-stale-p (review pr)
  "A review is stale if its version doesn't match the current changeset version."
  (/= (getf review :changeset-version) (getf pr :version)))

;;; ========================== CONCERNS ==========================

(defun create-concern (&key review-id changeset-id author-id body)
  "Create a concern."
  (postmodern:query
   (:insert-into 'cave-concerns
    :set 'review-id review-id
         'changeset-id changeset-id
         'author-id author-id
         'body body
    :returning '*)
   :plist))

(defun list-concerns (changeset-id)
  "List all concerns for a changeset."
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-concerns :where (:= 'changeset-id changeset-id))
    'created-at)
   :plists))

(defun resolve-concern (concern-id resolver-id)
  "Resolve a concern."
  (postmodern:execute
   (:update 'cave-concerns
    :set 'status "resolved" 'resolved-by-id resolver-id 'resolved-at (:now)
    :where (:= 'id concern-id))))

(defun find-concern-by-id (concern-id)
  "Find a concern by ID."
  (postmodern:query
   (:select '* :from 'cave-concerns :where (:= 'id concern-id))
   :plist))

(defun count-open-concerns (changeset-id)
  "Count open concerns for a changeset."
  (or (postmodern:query
       (:select (:count '*) :from 'cave-concerns
        :where (:and (:= 'changeset-id changeset-id) (:= 'status "open")))
       :single)
      0))

;;; ========================== MERGE ELIGIBILITY ==========================

(defun compute-merge-eligibility (pr repo)
  "Compute merge eligibility rules. Returns a list of (:description ... :pass ...)."
  (let* ((cs-id (getf pr :id))
         (repo-id (getf repo :id))
         (version (getf pr :version))
         (reviews (list-reviews cs-id))
         (rules nil))

    ;; Rule 1: Not closed/merged
    (push (list :description "Pull request is open"
                :pass (and (not (getf pr :is-merged))
                           (not (getf pr :is-closed))))
          rules)

    ;; Rule 1b: No merge conflicts with the target branch. Detected in-memory
    ;; with git merge-tree (no worktree) so the PR page can warn — and the merge
    ;; button can block — BEFORE a merge is attempted. The conflicting file list
    ;; rides along on the rule so the view can render it.
    (let* ((source (getf pr :source-branch))
           (target (getf pr :target-branch))
           (disk (when (and (not (getf pr :is-merged)) (not (getf pr :is-closed)))
                   (ignore-errors (repo-disk-path (repo-owner-name repo)
                                                  (getf repo :name))))))
      (when disk
        (multiple-value-bind (conflict-p files)
            (git-merge-conflicts disk target source)
          (push (list :kind :conflicts
                      :description (cond ((not conflict-p) "No merge conflicts")
                                         (files (format nil "Conflicts with ~A (~D file~:P)"
                                                        target (length files)))
                                         (t (format nil "Conflicts with ~A" target)))
                      :pass (not conflict-p)
                      :conflict-files files)
                rules))))

    ;; Rule 2: Target branch exists (simplified — always true for now)
    (push (list :description "Target branch exists"
                :pass t)
          rules)

    ;; Rule 3: No concurrent lock (simplified)
    (push (list :description "No concurrent merge in progress"
                :pass t)
          rules)

    ;; Rule 4: Required approvals
    (let* ((allow-stale (getf repo :allow-stale-approvals))
           (concerns-count (getf repo :concerns-count-as-approval))
           (allow-self (getf repo :allow-self-approval))
           (required (getf repo :required-approvals))
           (approval-count
             (loop for r in reviews
                   when (and (or (equal (getf r :state) "approve")
                                 (and concerns-count
                                      (equal (getf r :state) "approve_with_concerns")))
                             (repo-reviewer-p repo-id (getf r :reviewer-id))
                             (or allow-stale
                                 (= (getf r :changeset-version) version))
                             (or allow-self
                                 (/= (getf r :reviewer-id)
                                      (getf pr :author-id))))
                   count r)))
      (push (list :description (format nil "Approvals: ~A/~A required"
                                       approval-count required)
                  :pass (>= approval-count required))
            rules))

    ;; Rule 6: No blocking request-changes
    (when (getf repo :block-on-request-changes)
      (let ((blocking
              (loop for r in reviews
                    when (and (equal (getf r :state) "request_changes")
                              (= (getf r :changeset-version) version))
                    count r)))
        (push (list :description (if (zerop blocking)
                                     "No outstanding change requests"
                                     (format nil "~A change request~:P blocking" blocking))
                    :pass (zerop blocking))
              rules)))

    ;; Rule 7: Zero unresolved concerns
    (when (getf repo :require-zero-unresolved-concerns)
      (let ((open-concerns (count-open-concerns cs-id)))
        (push (list :description (if (zerop open-concerns)
                                     "No unresolved concerns"
                                     (format nil "~A unresolved concern~:P" open-concerns))
                    :pass (zerop open-concerns))
              rules)))

    ;; Rule 8: Required checks. Combine external commit statuses and cave
    ;; workflow runs for the PR's *head commit*. Block on failed, pending, or
    ;; missing results. Staleness is handled implicitly: results recorded
    ;; against an older sha won't match the current head, so they read as
    ;; missing and block until the new head reports.
    (when (getf repo :required-checks-pass)
      (let* ((head (getf pr :head-commit))
             (statuses (and head (list-commit-statuses repo-id head)))
             (build (and head (speculative-build-status repo-id head)))
             (failed (append
                      (loop for s in statuses
                            when (member (getf s :state) '("failure" "error")
                                         :test #'equal)
                            collect (or (getf s :context) "status"))
                      (when (eq build :failure) (list "cave workflow"))))
             (pending (append
                       (loop for s in statuses
                             when (equal (getf s :state) "pending")
                             collect (or (getf s :context) "status"))
                       (when (eq build :pending) (list "cave workflow"))))
             (present (or statuses (and build (not (eq build :none))))))
        (push
         (cond
           ((null head)
            (list :description "No head commit recorded to evaluate checks"
                  :pass nil))
           (failed
            (list :description (format nil "Checks failing: ~{~A~^, ~}" failed)
                  :pass nil))
           (pending
            (list :description (format nil "Checks pending: ~{~A~^, ~}" pending)
                  :pass nil))
           (present
            (list :description
                  (format nil "All required checks passed (~A context~:P)"
                          (+ (length statuses) (if (eq build :success) 1 0)))
                  :pass t))
           ;; No status or run reported for this head. Per the chosen policy
           ;; (GitHub empty-required-set), an absent check is not a blocker —
           ;; only checks that exist and fail/pend block. A green check that
           ;; later goes missing therefore passes; failing CI never merges.
           (t
            (list :description "No checks reported for this commit"
                  :pass t)))
         rules)))

    (nreverse rules)))

(defun pull-request-mergeable-p (eligibility)
  "Return T if all eligibility rules pass."
  (every (lambda (rule) (getf rule :pass)) eligibility))

;;; ========================== EVENTS ==========================

(defun log-event (event-type &key user-id repo-id entity-type entity-id metadata)
  "Record an event in the cave_events table."
  (postmodern:execute
   (:insert-into 'cave-events
    :set 'event-type event-type
         'user-id (or user-id :null)
         'repo-id (or repo-id :null)
         'entity-type (or entity-type :null)
         'entity-id (or entity-id :null)
         'metadata (if metadata
                       (com.inuoe.jzon:stringify metadata)
                       :null))))

(defun list-recent-events (&key repo-id (limit 30))
  "List recent events, optionally filtered by repo.
   Clones are excluded — they're noise in a feed and counted in Pulse instead."
  (if repo-id
      (postmodern:query
       (:limit
        (:order-by
         (:select 'cave-events.* (:as 'cave-users.username 'actor)
          :from 'cave-events
          :left-join 'cave-users :on (:= 'cave-events.user-id 'cave-users.id)
          :where (:and (:= 'cave-events.repo-id repo-id)
                       (:!= 'cave-events.event-type "git.clone")))
         (:desc 'cave-events.created-at))
        limit)
       :plists)
      (postmodern:query
       (:limit
        (:order-by
         (:select 'cave-events.* (:as 'cave-users.username 'actor)
                  (:as 'cave-repos.name 'repo-name)
          :from 'cave-events
          :left-join 'cave-users :on (:= 'cave-events.user-id 'cave-users.id)
          :left-join 'cave-repos :on (:= 'cave-events.repo-id 'cave-repos.id)
          :where (:!= 'cave-events.event-type "git.clone"))
         (:desc 'cave-events.created-at))
        limit)
       :plists)))

(defun log-page-view (repo-id &key ip-hash user-id referer-host)
  "Record a single page view. Cheap insert; aggregated at query time."
  (postmodern:execute
   (:insert-into 'cave-page-views
    :set 'repo-id repo-id
         'ip-hash (or ip-hash :null)
         'user-id (or user-id :null)
         'referer-host (or referer-host :null))))

(defun repo-page-views-by-day (repo-id &key (days 14))
  "Total views per day for a repo. Returns list of plists (:day :count)."
  (postmodern:query
   (format nil "SELECT to_char(date_trunc('day', viewed_at), 'YYYY-MM-DD') AS day, ~
                       COUNT(*)::int AS count ~
                FROM cave_page_views ~
                WHERE repo_id = $1 AND viewed_at >= NOW() - INTERVAL '~D days' ~
                GROUP BY day ORDER BY day ASC" days)
   repo-id :plists))

(defun repo-unique-visitors-by-day (repo-id &key (days 14))
  "Unique visitors per day (distinct ip_hash, falling back to user_id when no hash)."
  (postmodern:query
   (format nil "SELECT to_char(date_trunc('day', viewed_at), 'YYYY-MM-DD') AS day, ~
                       COUNT(DISTINCT COALESCE(ip_hash, user_id::text))::int AS count ~
                FROM cave_page_views ~
                WHERE repo_id = $1 AND viewed_at >= NOW() - INTERVAL '~D days' ~
                GROUP BY day ORDER BY day ASC" days)
   repo-id :plists))

(defun repo-top-referrers (repo-id &key (days 14) (limit 10))
  "Most-common referer hostnames driving traffic to a repo."
  (postmodern:query
   (format nil "SELECT referer_host AS host, COUNT(*)::int AS count ~
                FROM cave_page_views ~
                WHERE repo_id = $1 AND viewed_at >= NOW() - INTERVAL '~D days' ~
                  AND referer_host IS NOT NULL AND referer_host <> '' ~
                GROUP BY referer_host ORDER BY count DESC LIMIT $2" days)
   repo-id limit :plists))

(defun repo-event-counts-by-day (repo-id &key (days 14))
  "Return list of plists (:day yyyy-mm-dd :type event-type :count n) over
the trailing DAYS days for a single repo. Used to render the Pulse chart."
  (postmodern:query
   (format nil "SELECT to_char(date_trunc('day', created_at), 'YYYY-MM-DD') AS day, ~
                       event_type AS type, ~
                       COUNT(*)::int AS count ~
                FROM cave_events ~
                WHERE repo_id = $1 ~
                  AND created_at >= NOW() - INTERVAL '~D days' ~
                GROUP BY day, event_type ~
                ORDER BY day ASC" days)
   repo-id :plists))

(defun repo-top-contributors (repo-id &key (days 14) (limit 5))
  "Top contributors to a repo in the last DAYS days, ordered by event count."
  (postmodern:query
   (format nil "SELECT u.username, COUNT(*)::int AS count ~
                FROM cave_events e ~
                JOIN cave_users u ON u.id = e.user_id ~
                WHERE e.repo_id = $1 ~
                  AND e.created_at >= NOW() - INTERVAL '~D days' ~
                GROUP BY u.username ~
                ORDER BY count DESC ~
                LIMIT $2" days)
   repo-id limit :plists))

;;; ========================== COMMIT SIGNATURES ==========================

(defun all-ssh-keys-with-user ()
  "All registered SSH keys joined with the owning user's email + username.
   Used to build the allowed_signers file for git verify-commit."
  (postmodern:query
   (:select 'cave-users.email 'cave-users.username
            'cave-ssh-keys.public-key 'cave-ssh-keys.fingerprint
            (:as 'cave-ssh-keys.user-id 'user-id)
    :from 'cave-ssh-keys
    :inner-join 'cave-users :on (:= 'cave-ssh-keys.user-id 'cave-users.id))
   :plists))

(defun all-gpg-keys-with-user ()
  "All registered GPG keys joined with the owning user's email + username.
   Used to build the ephemeral keyring for git verify-commit and to map a
   signing fingerprint back to its user."
  (postmodern:query
   (:select 'cave-users.email 'cave-users.username
            'cave-gpg-keys.public-key 'cave-gpg-keys.key-id
            (:as 'cave-gpg-keys.user-id 'user-id)
    :from 'cave-gpg-keys
    :inner-join 'cave-users :on (:= 'cave-gpg-keys.user-id 'cave-users.id))
   :plists))

(defun record-commit-signature (&key repo-id commit-sha verified scheme fingerprint signer-user-id)
  "Upsert a signature verification result. Idempotent on (repo_id, commit_sha)."
  (postmodern:execute
   "INSERT INTO cave_commit_signatures
       (repo_id, commit_sha, verified, scheme, fingerprint, signer_user_id)
    VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (repo_id, commit_sha) DO UPDATE
       SET verified = EXCLUDED.verified,
           scheme = EXCLUDED.scheme,
           fingerprint = EXCLUDED.fingerprint,
           signer_user_id = EXCLUDED.signer_user_id"
   repo-id commit-sha verified
   (or scheme :null) (or fingerprint :null) (or signer-user-id :null)))

(defun find-commit-signature (repo-id commit-sha)
  (postmodern:query
   (:select '* :from 'cave-commit-signatures
    :where (:and (:= 'repo-id repo-id) (:= 'commit-sha commit-sha)))
   :plist))

(defun commit-signatures-by-sha (repo-id shas)
  "Bulk lookup. Returns hash-table sha → signature plist for shas that have one."
  (let ((h (make-hash-table :test 'equal)))
    (when shas
      (let ((rows (postmodern:query
                   "SELECT * FROM cave_commit_signatures
                    WHERE repo_id = $1 AND commit_sha = ANY($2)"
                   repo-id (coerce shas 'vector) :plists)))
        (dolist (r rows)
          (setf (gethash (getf r :commit-sha) h) r))))
    h))

;;; ========================== RELEASES ==========================

(defun create-release (&key repo-id tag-name name body is-prerelease is-draft created-by)
  "Create a release row. Returns its id."
  (postmodern:query
   (:insert-into 'cave-releases
    :set 'repo-id repo-id
         'tag-name tag-name
         'name (or name tag-name)
         'body (or body "")
         'is-prerelease (or is-prerelease nil)
         'is-draft (or is-draft nil)
         'created-by (or created-by :null)
    :returning 'id)
   :single))

(defun list-releases (repo-id &key (limit 50))
  "Releases for a repo, newest first."
  (postmodern:query
   (:limit
    (:order-by
     (:select 'r.* (:as 'u.username 'author)
      :from (:as 'cave-releases 'r)
      :left-join (:as 'cave-users 'u) :on (:= 'r.created-by 'u.id)
      :where (:= 'r.repo-id repo-id))
     (:desc 'r.published-at))
    limit)
   :plists))

(defun find-release-by-tag (repo-id tag-name)
  "Look up a single release. Returns plist or NIL."
  (postmodern:query
   (:select 'r.* (:as 'u.username 'author)
    :from (:as 'cave-releases 'r)
    :left-join (:as 'cave-users 'u) :on (:= 'r.created-by 'u.id)
    :where (:and (:= 'r.repo-id repo-id) (:= 'r.tag-name tag-name)))
   :plist))

(defun delete-release (release-id)
  (postmodern:execute (:delete-from 'cave-releases :where (:= 'id release-id))))

(defun create-release-asset (&key release-id name content-type size storage-path uploaded-by)
  (postmodern:query
   (:insert-into 'cave-release-assets
    :set 'release-id release-id
         'name name
         'content-type (or content-type "application/octet-stream")
         'size size
         'storage-path storage-path
         'uploaded-by (or uploaded-by :null)
    :returning 'id)
   :single))

(defun list-release-assets (release-id)
  (postmodern:query
   (:order-by
    (:select '* :from 'cave-release-assets
     :where (:= 'release-id release-id))
    'name)
   :plists))

(defun find-release-asset-by-name (release-id name)
  (postmodern:query
   (:select '* :from 'cave-release-assets
    :where (:and (:= 'release-id release-id) (:= 'name name)))
   :plist))

(defun find-release-asset-by-id (asset-id)
  (postmodern:query
   (:select '* :from 'cave-release-assets :where (:= 'id asset-id))
   :plist))

(defun delete-release-asset (asset-id)
  (postmodern:execute (:delete-from 'cave-release-assets :where (:= 'id asset-id))))

(defun increment-asset-download-count (asset-id)
  (postmodern:execute
   "UPDATE cave_release_assets SET download_count = download_count + 1 WHERE id = $1"
   asset-id))

;;; ========================== CHAMBER NODES ==========================

(defun list-chamber-nodes (&key status)
  "List all chamber nodes, optionally filtered by status."
  (if status
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-chamber-nodes
         :where (:= 'status status))
        'name)
       :plists)
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-chamber-nodes)
        'name)
       :plists)))

(defun find-chamber-node (id)
  (postmodern:query
   (:select '* :from 'cave-chamber-nodes :where (:= 'id id))
   :plist))

(defun find-chamber-node-by-name (name)
  (postmodern:query
   (:select '* :from 'cave-chamber-nodes :where (:= 'name name))
   :plist))

(defun upsert-chamber-node (&key name address)
  "Insert or update a chamber node by name."
  (let ((existing (find-chamber-node-by-name name)))
    (if existing
        (progn
          (postmodern:query
           (:update 'cave-chamber-nodes
            :set 'address address
            :where (:= 'id (getf existing :id))))
          (find-chamber-node-by-name name))
        (postmodern:query
         (:insert-into 'cave-chamber-nodes
          :set 'name name 'address address
          :returning '*)
         :plist))))

(defun update-chamber-node-status (node-id status)
  "Update node health status and last_seen_at."
  (postmodern:query
   (:update 'cave-chamber-nodes
    :set 'status status
         'last-seen-at (:raw "NOW()")
    :where (:= 'id node-id))))

;;; ========================== REPO ASSIGNMENTS ==========================

(defun repo-primary-node (repo-id)
  "Get the primary chamber node for a repo. Returns node plist or NIL."
  (postmodern:query
   (:select 'cave-chamber-nodes.*
    :from 'cave-repo-assignments
    :inner-join 'cave-chamber-nodes
    :on (:= 'cave-repo-assignments.node-id 'cave-chamber-nodes.id)
    :where (:and (:= 'cave-repo-assignments.repo-id repo-id)
                 (:= 'cave-repo-assignments.role "primary")))
   :plist))

(defun repo-secondary-nodes (repo-id)
  "Get secondary chamber nodes for a repo."
  (postmodern:query
   (:select 'cave-chamber-nodes.*
    :from 'cave-repo-assignments
    :inner-join 'cave-chamber-nodes
    :on (:= 'cave-repo-assignments.node-id 'cave-chamber-nodes.id)
    :where (:and (:= 'cave-repo-assignments.repo-id repo-id)
                 (:= 'cave-repo-assignments.role "secondary")))
   :plists))

(defun repo-all-nodes (repo-id)
  "Get all chamber nodes assigned to a repo (primary + secondaries)."
  (postmodern:query
   (:select 'cave-chamber-nodes.* 'cave-repo-assignments.role
    :from 'cave-repo-assignments
    :inner-join 'cave-chamber-nodes
    :on (:= 'cave-repo-assignments.node-id 'cave-chamber-nodes.id)
    :where (:= 'cave-repo-assignments.repo-id repo-id))
   :plists))

(defun repo-healthy-nodes (repo-id)
  "Get healthy nodes assigned to a repo."
  (postmodern:query
   (:select 'cave-chamber-nodes.*
    :from 'cave-repo-assignments
    :inner-join 'cave-chamber-nodes
    :on (:= 'cave-repo-assignments.node-id 'cave-chamber-nodes.id)
    :where (:and (:= 'cave-repo-assignments.repo-id repo-id)
                 (:in 'cave-chamber-nodes.status (:set "healthy" "suspect"))))
   :plists))

(defun assign-repo-to-node (repo-id node-id role)
  "Assign a repo to a chamber node with the given role."
  (postmodern:query
   (:insert-into 'cave-repo-assignments
    :set 'repo-id repo-id 'node-id node-id 'role role
    :returning '*)
   :plist))

(defun bump-repo-generation (repo-id node-id)
  "Increment the generation counter for a repo assignment."
  (postmodern:query
   (:update 'cave-repo-assignments
    :set 'generation (:+ 'generation 1)
    :where (:and (:= 'repo-id repo-id) (:= 'node-id node-id)))))

(defun node-repo-count (node-id)
  "Count repos assigned to a node as primary."
  (or (postmodern:query
       (:select (:count '*)
        :from 'cave-repo-assignments
        :where (:and (:= 'node-id node-id)
                     (:= 'role "primary")))
       :single)
      0))

(defun ensure-repo-assigned (repo-id)
  "Assign repo to least-loaded node if it has no primary assignment.
   Returns the primary node plist."
  (or (repo-primary-node repo-id)
      (let* ((nodes (list-chamber-nodes :status "healthy"))
             (best (when nodes
                     (reduce (lambda (a b)
                               (if (<= (node-repo-count (getf a :id))
                                       (node-repo-count (getf b :id)))
                                   a b))
                             nodes))))
        (when best
          (assign-repo-to-node repo-id (getf best :id) "primary")
          ;; Also assign all other nodes as secondaries
          (dolist (node nodes)
            (unless (= (getf node :id) (getf best :id))
              (assign-repo-to-node repo-id (getf node :id) "secondary")))
          best))))

;;; ---------------------------------------------------------------------------
;;; Dependency updates & security alerts (migrations 46-49).
;;; See docs/design/DESIGN_DEPENDENCY_UPDATES.md. Producers (sync-advisories,
;;; SBOM parse, server endpoints) live elsewhere; this is the query/match layer.
;;; ---------------------------------------------------------------------------

;;; --- Dependency graph (cave_repo_deps) -------------------------------------

(defun next-dep-generation (repo-id ref)
  "Next atomic-replace generation marker for REPO-ID's deps on REF."
  (1+ (postmodern:query
       (:select (:coalesce (:max 'generation) 0)
        :from 'cave-repo-deps
        :where (:and (:= 'repo-id repo-id) (:= 'ref ref)))
       :single)))

(defun upsert-repo-dep (repo-id ref dep generation)
  "Insert or refresh one dependency row, stamping it with GENERATION so the
   sweep keeps it. DEP is a plist: :manifest-path :ecosystem :package-name
   :version :purl :is-direct :scope. Unchanged rows keep their id."
  (postmodern:execute
   "INSERT INTO cave_repo_deps
       (repo_id, ref, manifest_path, ecosystem, package_name, version, purl,
        is_direct, scope, generation, ocicl_project, updated_at)
    VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11, NOW())
    ON CONFLICT (repo_id, ref, manifest_path, purl) DO UPDATE
       SET version = EXCLUDED.version,
           ecosystem = EXCLUDED.ecosystem,
           package_name = EXCLUDED.package_name,
           is_direct = EXCLUDED.is_direct,
           scope = EXCLUDED.scope,
           generation = EXCLUDED.generation,
           ocicl_project = EXCLUDED.ocicl_project,
           updated_at = NOW()"
   repo-id ref (getf dep :manifest-path) (getf dep :ecosystem)
   (getf dep :package-name) (getf dep :version) (getf dep :purl)
   (if (getf dep :is-direct t) t nil)
   (or (getf dep :scope) :null)
   generation
   (or (getf dep :ocicl-project) :null)))

(defun sweep-stale-deps (repo-id ref generation)
  "Delete REPO-ID/REF deps left behind by an older scan generation."
  (postmodern:execute
   (:delete-from 'cave-repo-deps
    :where (:and (:= 'repo-id repo-id) (:= 'ref ref)
                 (:< 'generation generation)))))

(defun list-repo-deps (repo-id &key ref)
  "All deps for REPO-ID, optionally scoped to REF."
  (if ref
      (postmodern:query
       (:order-by (:select '* :from 'cave-repo-deps
                   :where (:and (:= 'repo-id repo-id) (:= 'ref ref)))
                  'ecosystem 'package-name)
       :plists)
      (postmodern:query
       (:order-by (:select '* :from 'cave-repo-deps :where (:= 'repo-id repo-id))
                  'ecosystem 'package-name)
       :plists)))

(defun find-repos-using-package (ecosystem package-name)
  "Org-wide: every repo (with version + ref) depending on PACKAGE-NAME in
   ECOSYSTEM. The query an external bot structurally cannot answer."
  (postmodern:query
   (:order-by
    (:select 'repo-id 'ref 'version 'is-direct
     :from 'cave-repo-deps
     :where (:and (:= 'ecosystem ecosystem) (:= 'package-name package-name)))
    'repo-id)
   :plists))

;;; --- Version comparison & range matching -----------------------------------

(defun %nullish (x)
  "True for both CL NIL and postmodern's :NULL."
  (or (null x) (eq x :null)))

(defun %version-release-parts (version)
  "Return (values release-parts prerelease-p): the integer components of
   VERSION's release portion, and whether a -/+ suffix follows. Strips a
   leading 'v'; non-numeric components map to 0."
  (let* ((v (string-trim " " version))
         (v (if (and (plusp (length v)) (char-equal (char v 0) #\v))
                (subseq v 1) v))
         (cut (position-if (lambda (c) (or (char= c #\-) (char= c #\+))) v))
         (rel (if cut (subseq v 0 cut) v)))
    (values
     (mapcar (lambda (p) (or (parse-integer p :junk-allowed t) 0))
             (uiop:split-string rel :separator "."))
     (and cut t))))

(defun compare-versions (a b)
  "Compare version strings A and B; return -1, 0, or 1. Dotted-numeric ordering
   covers semver release comparison for npm/crates/Go/most ecosystems; a
   prerelease sorts below the same release. Exotic schemes (PEP 440 epochs,
   Maven qualifiers) are out of scope — osv-scanner is the bootstrap matcher."
  (multiple-value-bind (ar ap) (%version-release-parts a)
    (multiple-value-bind (br bp) (%version-release-parts b)
      (loop for x = (pop ar) for y = (pop br)
            while (or x y)
            do (let ((xi (or x 0)) (yi (or y 0)))
                 (cond ((< xi yi) (return-from compare-versions -1))
                       ((> xi yi) (return-from compare-versions 1)))))
      (cond ((and ap (not bp)) -1)
            ((and bp (not ap)) 1)
            (t 0)))))

(defun bump-level (from to)
  "Classify the upgrade FROM -> TO as :patch, :minor, or :major (semver-ish)."
  (let ((fr (%version-release-parts from))
        (tr (%version-release-parts to)))
    (flet ((nth0 (l n) (or (nth n l) 0)))
      (cond ((/= (nth0 fr 0) (nth0 tr 0)) :major)
            ((/= (nth0 fr 1) (nth0 tr 1)) :minor)
            (t :patch)))))

(defun version-in-range-p (version introduced fixed last-affected)
  "OSV range semantics: introduced is inclusive, fixed exclusive, last-affected
   inclusive. A missing upper bound means open-ended."
  (and (or (%nullish introduced) (string= introduced "0")
           (>= (compare-versions version introduced) 0))
       (cond ((not (%nullish fixed))
              (< (compare-versions version fixed) 0))
             ((not (%nullish last-affected))
              (<= (compare-versions version last-affected) 0))
             (t t))))

(defun dep-affected-p (dep affected)
  "True when DEP's version falls within AFFECTED's range. Both are plists.
   GIT-type ranges aren't version-comparable and never match here."
  (and (string= (getf dep :ecosystem) (getf affected :ecosystem))
       (string= (getf dep :package-name) (getf affected :package-name))
       (not (string-equal (or (getf affected :range-type) "") "GIT"))
       (version-in-range-p (getf dep :version)
                           (getf affected :introduced)
                           (getf affected :fixed)
                           (getf affected :last-affected))))

;;; --- Advisories (cave_advisories + cave_advisory_affected) ------------------

(defun upsert-advisory (&key osv-id summary details aliases severity cvss-score
                             refs published-at modified-at withdrawn-at)
  "Insert or update an advisory keyed by OSV-ID, unioning ALIASES. Returns the
   advisory row id."
  (postmodern:query
   "INSERT INTO cave_advisories
       (osv_id, summary, details, aliases, severity, cvss_score, refs,
        published_at, modified_at, withdrawn_at)
    VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,
            $8::timestamptz,$9::timestamptz,$10::timestamptz)
    ON CONFLICT (osv_id) DO UPDATE SET
       summary = EXCLUDED.summary,
       details = EXCLUDED.details,
       aliases = (SELECT COALESCE(array_agg(DISTINCT x), '{}')
                  FROM unnest(cave_advisories.aliases || EXCLUDED.aliases) x),
       severity = EXCLUDED.severity,
       cvss_score = EXCLUDED.cvss_score,
       refs = EXCLUDED.refs,
       published_at = EXCLUDED.published_at,
       modified_at = EXCLUDED.modified_at,
       withdrawn_at = EXCLUDED.withdrawn_at
    RETURNING id"
   osv-id (or summary :null) (or details :null)
   (coerce (or aliases '()) 'vector)
   (or severity :null) (or cvss-score :null) (or refs "[]")
   (or published-at :null) (or modified-at :null) (or withdrawn-at :null)
   :single))

(defun replace-advisory-affected (advisory-id ranges)
  "Replace ADVISORY-ID's affected-range rows. RANGES is a list of plists:
   :ecosystem :package-name :range-type :introduced :fixed :last-affected."
  (postmodern:with-transaction ()
    (postmodern:execute
     (:delete-from 'cave-advisory-affected :where (:= 'advisory-id advisory-id)))
    (dolist (r ranges)
      (postmodern:execute
       "INSERT INTO cave_advisory_affected
           (advisory_id, ecosystem, package_name, range_type,
            introduced, fixed, last_affected, repo)
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8)"
       advisory-id (getf r :ecosystem) (getf r :package-name)
       (or (getf r :range-type) "SEMVER")
       (or (getf r :introduced) :null)
       (or (getf r :fixed) :null)
       (or (getf r :last-affected) :null)
       (or (getf r :repo) :null)))))

;;; --- ocicl project -> upstream repo cache ----------------------------------

(defun list-graph-ocicl-projects ()
  "Distinct ocicl project names present in the dependency graph."
  (mapcar (lambda (r) (getf r :ocicl-project))
          (postmodern:query
           "SELECT DISTINCT ocicl_project FROM cave_repo_deps
            WHERE ecosystem = 'ocicl' AND ocicl_project IS NOT NULL"
           :plists)))

(defun find-ocicl-project (name)
  "The cached (name, source_repo, source_commit, systems) row, or NIL."
  (postmodern:query
   (:select '* :from 'cave-ocicl-projects :where (:= 'name name))
   :plist))

(defun upsert-ocicl-project (name &key source-repo source-commit systems)
  "Cache an ocicl project's resolved upstream repo + commit + system list."
  (postmodern:execute
   "INSERT INTO cave_ocicl_projects (name, source_repo, source_commit, systems, resolved_at)
    VALUES ($1,$2,$3,$4,NOW())
    ON CONFLICT (name) DO UPDATE
       SET source_repo = EXCLUDED.source_repo,
           source_commit = EXCLUDED.source_commit,
           systems = EXCLUDED.systems,
           resolved_at = NOW()"
   name (or source-repo :null) (or source-commit :null)
   (coerce (or systems '()) 'vector)))

(defun advisory-url (osv-id)
  "Canonical web page for an advisory. CL-SEC ids aren't in OSV — they live on
   the cl-sec site (a SPA that deep-links each advisory by URL hash); everything
   else resolves on osv.dev."
  (if (and (stringp osv-id) (uiop:string-prefix-p "CL-SEC-" osv-id))
      (format nil "https://cl-sec.github.io/cl-sec-advisories/#~A" osv-id)
      (format nil "https://osv.dev/vulnerability/~A" osv-id)))

(defun find-advisory (osv-id)
  "The advisory whose osv_id is exactly OSV-ID."
  (postmodern:query
   (:select '* :from 'cave-advisories :where (:= 'osv-id osv-id))
   :plist))

(defun find-advisory-by-alias (id)
  "The canonical advisory whose osv_id equals ID or that lists ID as an alias."
  (postmodern:query
   "SELECT * FROM cave_advisories
    WHERE osv_id = $1 OR $1 = ANY(aliases) LIMIT 1"
   id :plist))

(defun list-affected-for-package (ecosystem package-name)
  "Affected ranges for (ECOSYSTEM, PACKAGE-NAME) from non-withdrawn advisories;
   each row carries its advisory_id."
  (postmodern:query
   "SELECT aa.* FROM cave_advisory_affected aa
    JOIN cave_advisories a ON a.id = aa.advisory_id
    WHERE aa.ecosystem = $1 AND aa.package_name = $2 AND a.withdrawn_at IS NULL
      AND a.cvss_score IS DISTINCT FROM 0"
   ecosystem package-name :plists))

(defun advisory-affected-packages (advisory-id)
  "Distinct (ecosystem, package_name) plists the advisory affects."
  (postmodern:query
   "SELECT DISTINCT ecosystem, package_name FROM cave_advisory_affected
    WHERE advisory_id = $1"
   advisory-id :plists))

;;; --- Suppressions (cave_dep_suppressions) ----------------------------------

(defun create-dep-suppression (&key repo-id ecosystem package-name advisory-id
                                    reason note created-by expires-at)
  "Record durable user intent to suppress an advisory for a package in a repo.
   Idempotent on (repo, ecosystem, package, advisory)."
  (postmodern:execute
   "INSERT INTO cave_dep_suppressions
       (repo_id, ecosystem, package_name, advisory_id, reason, note,
        created_by, expires_at)
    VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
    ON CONFLICT (repo_id, ecosystem, package_name, advisory_id) DO UPDATE
       SET reason = EXCLUDED.reason, note = EXCLUDED.note,
           created_by = EXCLUDED.created_by, expires_at = EXCLUDED.expires_at"
   repo-id ecosystem package-name advisory-id reason
   (or note :null) (or created-by :null) (or expires-at :null)))

(defun find-active-suppression (repo-id ecosystem package-name advisory-id)
  "Suppression row if one exists and has not lapsed, else NIL. Expiry is
   enforced here — the locked decision puts lapse handling in the matcher."
  (postmodern:query
   "SELECT * FROM cave_dep_suppressions
    WHERE repo_id = $1 AND ecosystem = $2 AND package_name = $3
      AND advisory_id = $4 AND (expires_at IS NULL OR expires_at > NOW())
    LIMIT 1"
   repo-id ecosystem package-name advisory-id :plist))

;;; --- Alerts (cave_dep_alerts) — derived, recomputable ----------------------

(defun upsert-dep-alert (&key repo-id dep-id advisory-id state fix-version)
  "Upsert a derived alert. Preserves matcher-external columns (fix_kind,
   fix_pr_id, reachable) on update."
  (postmodern:execute
   "INSERT INTO cave_dep_alerts (repo_id, dep_id, advisory_id, state, fix_version)
    VALUES ($1,$2,$3,$4,$5)
    ON CONFLICT (dep_id, advisory_id) DO UPDATE
       SET state = EXCLUDED.state,
           fix_version = EXCLUDED.fix_version,
           updated_at = NOW()"
   repo-id dep-id advisory-id state (or fix-version :null)))

(defun set-dep-alert-state (alert-id state)
  "Set an alert's lifecycle state (e.g. 'fixed' when it no longer matches)."
  (postmodern:execute
   (:update 'cave-dep-alerts
    :set 'state state 'updated-at (:now)
    :where (:= 'id alert-id))))

(defun list-dep-alerts (repo-id &key state)
  "Alerts for REPO-ID, optionally filtered to a single STATE."
  (if state
      (postmodern:query
       (:select '* :from 'cave-dep-alerts
        :where (:and (:= 'repo-id repo-id) (:= 'state state)))
       :plists)
      (postmodern:query
       (:select '* :from 'cave-dep-alerts :where (:= 'repo-id repo-id))
       :plists)))

(defun %open-or-dismissed-alerts (repo-id)
  "Live alerts (open or dismissed) as (id, dep_id, advisory_id) plists, for
   reconciliation in the matcher."
  (postmodern:query
   "SELECT id, dep_id, advisory_id FROM cave_dep_alerts
    WHERE repo_id = $1 AND state IN ('open','dismissed')"
   repo-id :plists))

;;; --- Matcher ---------------------------------------------------------------

;;; --- GIT-range matching (ocicl deps vs the cl-sec advisory feed) -----------
;;;
;;; cl-sec advisories identify affected software by a source repo + commit range
;;; (introduced..fixed). An ocicl dep resolves to (upstream repo, commit) — the
;;; repo via cave_ocicl_projects, the commit from its version — so a dep is
;;; affected iff its commit is in an advisory's range on the same repo, decided
;;; by commit ancestry against a cached blobless bare clone of that repo.

(defun %normalize-repo-url (url)
  "Canonical key for matching git URLs: lowercase, no scheme/userinfo, no .git,
   no trailing slash. github.com/atgreen/ag-gRPC.git -> github.com/atgreen/ag-grpc."
  (when (stringp url)
    (let* ((u (string-downcase (string-trim '(#\Space) url)))
           (u (cond ((uiop:string-prefix-p "https://" u) (subseq u 8))
                    ((uiop:string-prefix-p "http://" u) (subseq u 7))
                    ((uiop:string-prefix-p "git://" u) (subseq u 6))
                    ((uiop:string-prefix-p "ssh://" u) (subseq u 6))
                    (t u)))
           (u (let ((at (position #\@ u))) (if at (subseq u (1+ at)) u)))
           (u (substitute #\/ #\: u))
           (u (string-right-trim '(#\/) u))
           (u (if (uiop:string-suffix-p u ".git") (subseq u 0 (- (length u) 4)) u)))
      u)))

(defun list-git-affected ()
  "All GIT-range affected rows (advisory not withdrawn, CVSS not 0.0): plists
   with :advisory-id :repo :introduced :fixed."
  (postmodern:query
   "SELECT aa.advisory_id, aa.repo, aa.introduced, aa.fixed
    FROM cave_advisory_affected aa
    JOIN cave_advisories a ON a.id = aa.advisory_id
    WHERE aa.range_type = 'GIT' AND aa.repo IS NOT NULL
      AND a.withdrawn_at IS NULL AND a.cvss_score IS DISTINCT FROM 0"
   :plists))

(defun %git-advisories-by-repo ()
  "Hash of normalized-repo-url -> list of its GIT-range affected rows."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (a (list-git-affected) h)
      (push a (gethash (%normalize-repo-url (getf a :repo)) h)))))

(defun %advisory-repo-cache-path (repo-url)
  "Local bare-clone path for REPO-URL, under data-dir/advisory-repos."
  (let ((safe (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\.))) c #\_))
                   (or (%normalize-repo-url repo-url) "repo"))))
    (merge-pathnames (format nil "~A.git/" safe) (data-dir "advisory-repos"))))

(defun ensure-advisory-repo (repo-url commits)
  "Ensure a local blobless bare clone of REPO-URL with COMMITS present. Returns
   its path, or NIL if it can't be made available."
  (let ((path (%advisory-repo-cache-path repo-url)))
    (handler-case
        (progn
          (unless (probe-file (merge-pathnames "HEAD" path))
            (ensure-directories-exist (data-dir "advisory-repos"))
            (multiple-value-bind (ok err) (git-clone-blobless-bare repo-url (namestring path))
              (unless ok
                (llog:warn "advisory repo clone failed" :url repo-url :error err)
                (return-from ensure-advisory-repo nil))))
          (dolist (c commits)
            (when (and c (stringp c) (not (git-has-commit-p path c)))
              (git-fetch-commit path repo-url c)))
          path)
      (error (e)
        (llog:warn "advisory repo ensure failed" :url repo-url :error (princ-to-string e))
        nil))))

(defun %commit-affected-p (repo-path commit introduced fixed)
  "OSV GIT-range semantics: COMMIT is affected iff INTRODUCED is its ancestor (or
   introduced is the '0' sentinel) and FIXED is NOT its ancestor. Uncertain
   ancestry -> NIL (don't alert on what can't be determined)."
  (let ((after-intro (if (or (null introduced) (equal introduced "0"))
                         t
                         (eq t (git-is-ancestor-p repo-path introduced commit))))
        (has-fix (and fixed (not (%nullish fixed))
                      (eq t (git-is-ancestor-p repo-path fixed commit)))))
    (and after-intro (not has-fix))))

(defun git-affected-for-dep (dep git-adv-by-repo)
  "GIT-range affected rows that apply to ocicl DEP, using GIT-ADV-BY-REPO
   (normalized-repo -> rows). Returns rows (:advisory-id :fixed ...), or NIL."
  (let ((project (getf dep :ocicl-project)))
    (when (and (equal (getf dep :ecosystem) "ocicl") project)
      (let* ((proj (find-ocicl-project project))
             (source-repo (getf proj :source-repo))
             (commit (%ocicl-version-commit (getf dep :version)))
             (advs (and source-repo
                        (gethash (%normalize-repo-url source-repo) git-adv-by-repo))))
        (when (and source-repo commit advs)
          (let ((repo-path (ensure-advisory-repo
                            source-repo
                            (cons commit
                                  (loop for a in advs
                                        append (list (getf a :introduced) (getf a :fixed)))))))
            (when repo-path
              (loop for a in advs
                    when (%commit-affected-p repo-path commit
                                             (getf a :introduced) (getf a :fixed))
                    collect a))))))))

(defun rematch-repo (repo-id &optional ref)
  "Recompute REPO-ID's alerts against the advisory DB. Suppressed matches become
   'dismissed'; live alerts that no longer match become 'fixed'. Covers both
   version-range and GIT-range (ocicl) matches. Returns the match count."
  (let ((deps (list-repo-deps repo-id :ref ref))
        (desired (make-hash-table :test 'equal))
        (git-adv-by-repo (%git-advisories-by-repo))
        (matches 0))
    (flet ((record (dep adv-id fixed)
             (let ((supp (find-active-suppression repo-id (getf dep :ecosystem)
                                                  (getf dep :package-name) adv-id)))
               (setf (gethash (cons (getf dep :id) adv-id) desired) t)
               (incf matches)
               (upsert-dep-alert :repo-id repo-id :dep-id (getf dep :id)
                                 :advisory-id adv-id
                                 :state (if supp "dismissed" "open")
                                 :fix-version (if (%nullish fixed) nil fixed)))))
      (dolist (dep deps)
        (dolist (aff (list-affected-for-package (getf dep :ecosystem)
                                                (getf dep :package-name)))
          (when (dep-affected-p dep aff)
            (record dep (getf aff :advisory-id) (getf aff :fixed))))
        (dolist (aff (git-affected-for-dep dep git-adv-by-repo))
          (record dep (getf aff :advisory-id) (getf aff :fixed))))
      (dolist (al (%open-or-dismissed-alerts repo-id))
        (unless (gethash (cons (getf al :dep-id) (getf al :advisory-id)) desired)
          (set-dep-alert-state (getf al :id) "fixed")))
      matches)))

(defun rematch-ocicl-repos ()
  "Re-match every (repo, ref) carrying ocicl deps. Used after syncing GIT-range
   advisory feeds, which don't map to packages rematch-advisory can target.
   Returns the number of (repo, ref) pairs re-matched."
  (let ((pairs (postmodern:query
                "SELECT DISTINCT repo_id, ref FROM cave_repo_deps WHERE ecosystem = 'ocicl'"
                :plists)))
    (dolist (rr pairs (length pairs))
      (rematch-repo (getf rr :repo-id) (getf rr :ref)))))

(defun rematch-advisory (advisory-id)
  "Re-match the stored graph for every package this advisory affects — the
   native superpower: a freshly synced CVE finds existing deps with no rescan.
   Returns the number of (repo, ref) pairs re-matched."
  (let ((pairs (make-hash-table :test 'equal)))
    (dolist (pkg (advisory-affected-packages advisory-id))
      (dolist (rr (postmodern:query
                   "SELECT DISTINCT repo_id, ref FROM cave_repo_deps
                    WHERE ecosystem = $1 AND package_name = $2"
                   (getf pkg :ecosystem) (getf pkg :package-name) :plists))
        (setf (gethash (cons (getf rr :repo-id) (getf rr :ref)) pairs) t)))
    (maphash (lambda (k v) (declare (ignore v))
               (rematch-repo (car k) (cdr k)))
             pairs)
    (hash-table-count pairs)))

;;; --- Ingest (atomic graph replace + re-match) ------------------------------

(defun ingest-repo-deps (repo-id ref deps)
  "Atomically replace REPO-ID's dependency graph for REF with DEPS (a list of
   plists), then re-match against advisories. Unchanged rows keep their id (so
   their alerts survive); removed rows are swept. Returns the dep count."
  (postmodern:with-transaction ()
    (let ((gen (next-dep-generation repo-id ref)))
      (dolist (dep deps) (upsert-repo-dep repo-id ref dep gen))
      (sweep-stale-deps repo-id ref gen))
    (rematch-repo repo-id ref))
  (length deps))

;;; --- Dashboard support (queries; rendering lives in deps-dashboard.lisp) ----

(defparameter *dependency-bot-username* "cave-bot"
  "Username of the lazily-created system user that authors dependency dashboards.")

(defun ensure-dependency-bot-user ()
  "Find or lazily create the cave-bot user that authors dependency dashboards.
   Returns its id."
  (getf (or (find-user-by-username *dependency-bot-username*)
            (create-user :username *dependency-bot-username* :display-name "Cave"))
        :id))

(defun list-dep-alerts-detailed (repo-id &key (state "open"))
  "Alerts for REPO-ID joined with their dep + advisory, for display."
  (postmodern:query
   "SELECT al.id, al.state, al.fix_version,
           d.ecosystem, d.package_name, d.version,
           a.osv_id, a.summary, a.severity, a.cvss_score
    FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    JOIN cave_advisories a ON a.id = al.advisory_id
    WHERE al.repo_id = $1 AND al.state = $2"
   repo-id state :plists))

(defun find-dep-alert-detailed (alert-id)
  "One alert joined with its dep + advisory, for the fix pipeline."
  (postmodern:query
   "SELECT al.id, al.repo_id, al.advisory_id, al.state, al.fix_version,
           al.fix_kind, al.fix_pr_id,
           d.ecosystem, d.package_name, d.version, d.manifest_path, d.is_direct,
           a.osv_id, a.summary, a.severity
    FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    JOIN cave_advisories a ON a.id = al.advisory_id
    WHERE al.id = $1"
   alert-id :plist))

(defun dep-automerge-candidates ()
  "Open alerts with an open fix PR — inputs for the auto-merge processor."
  (postmodern:query
   "SELECT al.id AS alert_id, al.fix_version,
           d.version, d.ecosystem, d.package_name,
           al.repo_id, c.id AS pr_id, c.number AS pr_number,
           c.source_branch, c.target_branch, c.head_commit
    FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    JOIN cave_changesets c ON c.id = al.fix_pr_id
    WHERE al.state = 'open' AND al.fix_pr_id IS NOT NULL
      AND c.is_merged = FALSE AND c.is_closed = FALSE"
   :plists))

(defun set-alert-fix-kind (alert-id fix-kind)
  "Cache the classified fix kind on an alert."
  (postmodern:execute
   (:update 'cave-dep-alerts :set 'fix-kind fix-kind 'updated-at (:now)
    :where (:= 'id alert-id))))

(defun set-alert-fix-pr (alert-id pr-id)
  "Link the fix PR (changeset id) to an alert."
  (postmodern:execute
   (:update 'cave-dep-alerts :set 'fix-pr-id pr-id 'updated-at (:now)
    :where (:= 'id alert-id))))

;;; --- Dependency fix attempts: speculative build -> PR (Dependabot-style) ---

(defun workflow-runs-for-commit (repo-id commit-sha)
  "All workflow runs for REPO-ID at COMMIT-SHA."
  (postmodern:query
   (:select '* :from 'cave-workflow-runs
    :where (:and (:= 'repo-id repo-id) (:= 'commit-sha commit-sha)))
   :plists))

(defun speculative-build-status (repo-id commit-sha)
  "Combined status of the cave workflow runs at COMMIT-SHA:
   :none (no runs scheduled), :pending, :failure, or :success."
  (let ((runs (workflow-runs-for-commit repo-id commit-sha)))
    (cond
      ((null runs) :none)
      ((some (lambda (r) (member (getf r :status)
                                 '("queued" "assigned" "running") :test #'equal))
             runs) :pending)
      ((some (lambda (r) (member (getf r :status)
                                 '("failed" "failure" "error" "cancelled") :test #'equal))
             runs) :failure)
      ((every (lambda (r) (equal (getf r :status) "success")) runs) :success)
      (t :pending))))

(defun create-dep-fix-attempt (&key alert-id repo-id branch commit-sha (state "building"))
  "Record a fix attempt for ALERT-ID."
  (postmodern:query
   (:insert-into 'cave-dep-fix-attempts
    :set 'alert-id alert-id 'repo-id repo-id 'branch branch
         'commit-sha commit-sha 'state state
    :returning '*)
   :plist))

(defun set-dep-fix-attempt-state (id state &key pr-id detail)
  "Update a fix attempt's state (and optionally its PR / detail)."
  (postmodern:execute
   (:update 'cave-dep-fix-attempts
    :set 'state state 'pr-id (or pr-id :null) 'detail (or detail :null)
         'updated-at (:now)
    :where (:= 'id id))))

(defun dep-fix-attempt-for-alert (alert-id)
  "The fix attempt for ALERT-ID, or NIL."
  (postmodern:query
   (:select '* :from 'cave-dep-fix-attempts :where (:= 'alert-id alert-id))
   :plist))

(defun building-fix-attempts-for-commit (repo-id commit-sha)
  "Fix attempts still 'building' whose speculative build is at COMMIT-SHA."
  (postmodern:query
   (:select '* :from 'cave-dep-fix-attempts
    :where (:and (:= 'repo-id repo-id) (:= 'commit-sha commit-sha)
                 (:= 'state "building")))
   :plists))

(defun open-fixable-alerts-without-attempt ()
  "Open alerts on direct deps that have a fix version and no fix attempt yet.
   Returns plists with :id and :repo-id."
  (postmodern:query
   "SELECT al.id, al.repo_id
    FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    WHERE al.state = 'open'
      AND al.fix_version IS NOT NULL
      AND d.is_direct = TRUE
      AND d.ecosystem <> 'ocicl'
      AND NOT EXISTS (SELECT 1 FROM cave_dep_fix_attempts fa WHERE fa.alert_id = al.id)"
   :plists))

(defun list-open-ocicl-fix-targets (repo-id)
  "Open ocicl alerts in REPO-ID with a GIT-range advisory, plus everything needed
   to bump + verify: the system, project, current version, and the advisory's
   source repo + commit range. One row per (alert, advisory)."
  (postmodern:query
   "SELECT al.id AS alert_id, al.repo_id, d.ref, d.package_name AS system,
           d.ocicl_project AS project, d.version AS cur_version,
           a.osv_id, aa.repo AS adv_repo, aa.introduced, aa.fixed
    FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    JOIN cave_advisories a ON a.id = al.advisory_id
    JOIN cave_advisory_affected aa ON aa.advisory_id = a.id AND aa.range_type = 'GIT'
    WHERE al.state = 'open' AND al.repo_id = $1 AND d.ecosystem = 'ocicl'
      AND d.ocicl_project IS NOT NULL AND al.fix_pr_id IS NULL"
   repo-id :plists))

(defun repos-with-open-ocicl-alerts ()
  "Distinct (repo_id, ref) plists with open ocicl alerts lacking a fix PR."
  (postmodern:query
   "SELECT DISTINCT al.repo_id, d.ref FROM cave_dep_alerts al
    JOIN cave_repo_deps d ON d.id = al.dep_id
    WHERE al.state = 'open' AND d.ecosystem = 'ocicl' AND al.fix_pr_id IS NULL"
   :plists))

(defun repo-deps-fix-in-flight-p (repo-id)
  "True if REPO-ID has a non-terminal deps-fix run (avoid duplicate fix jobs)."
  (postmodern:query
   "SELECT 1 FROM cave_workflow_runs
    WHERE repo_id = $1 AND workflow_name = 'deps-fix'
      AND status NOT IN ('success','failure','cancelled') LIMIT 1"
   repo-id :single))

(defun auto-fix-security-enabled-p (repo-id)
  "Whether to auto-open speculative security fix PRs for REPO-ID. Org repos honor
   the org policy's auto_fix_security (default TRUE); user repos default TRUE."
  (let* ((repo (find-repo-by-id repo-id))
         (org-id (and repo (let ((o (getf repo :org-id))) (unless (eq o :null) o)))))
    (if org-id
        (let ((p (get-org-dep-policy org-id)))
          (if p
              (let ((v (getf p :auto-fix-security)))
                (if (eq v :null) t v))
              t))
        t)))

(defun find-dashboard-issue (repo-id marker)
  "The dependency-dashboard issue for REPO-ID (identified by MARKER in its body),
   or NIL."
  (postmodern:query
   "SELECT * FROM cave_issues WHERE repo_id = $1 AND body LIKE $2 LIMIT 1"
   repo-id (format nil "%~A%" marker) :plist))

(defun get-org-dep-policy (org-id)
  "The org's dependency policy row, or NIL."
  (postmodern:query
   (:select '* :from 'cave-org-dep-policy :where (:= 'org-id org-id))
   :plist))

(defun upsert-org-dep-policy (&key org-id allowed-ecosystems license-allow
                                   license-deny (automerge-ceiling "none")
                                   (security-always-on t) freeze-windows
                                   (auto-fix-security t))
  "Create or update an org's dependency policy."
  (postmodern:execute
   "INSERT INTO cave_org_dep_policy
       (org_id, allowed_ecosystems, license_allow, license_deny,
        automerge_ceiling, security_always_on, freeze_windows,
        auto_fix_security, updated_at)
    VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,$8, NOW())
    ON CONFLICT (org_id) DO UPDATE SET
       allowed_ecosystems = EXCLUDED.allowed_ecosystems,
       license_allow = EXCLUDED.license_allow,
       license_deny = EXCLUDED.license_deny,
       automerge_ceiling = EXCLUDED.automerge_ceiling,
       security_always_on = EXCLUDED.security_always_on,
       freeze_windows = EXCLUDED.freeze_windows,
       auto_fix_security = EXCLUDED.auto_fix_security,
       updated_at = NOW()"
   org-id
   (if allowed-ecosystems (coerce allowed-ecosystems 'vector) :null)
   (if license-allow (coerce license-allow 'vector) :null)
   (if license-deny (coerce license-deny 'vector) :null)
   automerge-ceiling security-always-on (or freeze-windows "[]")
   auto-fix-security))

(defun repos-needing-dashboard-refresh (marker)
  "Repo ids that either have open alerts or already have a dashboard issue."
  (mapcar (lambda (r) (getf r :repo-id))
          (postmodern:query
           "SELECT DISTINCT repo_id FROM cave_dep_alerts WHERE state = 'open'
            UNION
            SELECT repo_id FROM cave_issues WHERE body LIKE $1"
           (format nil "%~A%" marker) :plists)))
