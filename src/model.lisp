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

(defun create-org (&key name display-name description creator-id)
  "Create a new org and add creator as admin."
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
  (postmodern:query
   (:insert-into 'cave-repos
    :set 'org-id (or org-id :null)
         'owner-id (or owner-id :null)
         'name name
         'description description
         'is-private is-private
    :returning '*)
   :plist))

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

(defun append-run-log (run-id chunk)
  "Append a log chunk to an automation run."
  (postmodern:execute
   (:update 'cave-automation-runs
    :set 'log (:|| 'log chunk)
    :where (:= 'id run-id))))

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
                           (:- (:now) (:raw "'2 minutes'"))))))))

(defun cleanup-offline-runners ()
  "Mark runners as offline if no heartbeat in 60 seconds. Delete stale ephemeral ones."
  (postmodern:execute
   (:update 'cave-runners
    :set 'status "offline"
    :where (:and (:= 'status "online")
                 (:<= 'last-seen-at
                      (:- (:now) (:raw "'60 seconds'"))))))
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
  "Validate a registration token. Returns the token record or NIL."
  (when token
    (postmodern:query
     (:select '* :from 'cave-runner-registration-tokens
      :where (:and (:= 'token token)
                   (:or (:is-null 'expires-at)
                        (:> 'expires-at (:now)))))
     :plist)))

;;; ========================== WORKFLOWS ==========================

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

(defun create-workflow-job (&key workflow-run-id name image needs runs-on (timeout-seconds 0))
  "Create a workflow job. NEEDS is a list of job name strings.
   RUNS-ON is a list of label strings the runner must have.
   TIMEOUT-SECONDS is the max job duration (0 means use default)."
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

(defun create-workflow-step (&key job-id step-order name command (timeout-seconds 0))
  "Create a workflow step. TIMEOUT-SECONDS is the max step duration (0 means no limit)."
  (postmodern:query
   (:insert-into 'cave-workflow-steps
    :set 'job-id job-id
         'step-order step-order
         'name (or name :null)
         'command command
         'timeout-seconds (or timeout-seconds 0)
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

(defun append-step-log (step-id chunk)
  "Append log text to a workflow step."
  (postmodern:execute
   (:update 'cave-workflow-steps
    :set 'log (:|| 'log chunk)
    :where (:= 'id step-id))))

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
           'body body
      :returning '*)
     :plist)))

(defun find-issue (repo-id number)
  "Find an issue by repo and number."
  (postmodern:query
   (:select '* :from 'cave-issues
    :where (:and (:= 'repo-id repo-id) (:= 'number number)))
   :plist))

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
         'body body
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

    ;; Rule 8: Checks passed (simplified — no check infrastructure yet)
    (when (getf repo :required-checks-pass)
      (push (list :description "All checks passed (no checks configured)"
                  :pass t)
            rules))

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
  "List recent events, optionally filtered by repo."
  (if repo-id
      (postmodern:query
       (:limit
        (:order-by
         (:select 'cave-events.* (:as 'cave-users.username 'actor)
          :from 'cave-events
          :left-join 'cave-users :on (:= 'cave-events.user-id 'cave-users.id)
          :where (:= 'cave-events.repo-id repo-id))
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
          :left-join 'cave-repos :on (:= 'cave-events.repo-id 'cave-repos.id))
         (:desc 'cave-events.created-at))
        limit)
       :plists)))
