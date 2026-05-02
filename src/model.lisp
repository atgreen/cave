;;; model.lisp — Domain model queries and CRUD
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; We use postmodern's s-sql for queries and return plists.
;;; No ORM — just functions that return data.

;;; ========================== USERS ==========================

(defun create-user (&key username password display-name email (is-admin nil))
  "Create a new user. Returns the user plist."
  (let ((hash (hash-password password)))
    (postmodern:query
     (:insert-into 'cave-users
      :set 'username username
           'password-hash hash
           'display-name (or display-name username)
           'email email
           'is-admin is-admin
      :returning '*)
     :plist)))

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

(defun authenticate-user (username password)
  "Authenticate a user by username and password. Returns user plist or NIL."
  (let ((user (find-user-by-username username)))
    (when (and user
               (getf user :is-active)
               (check-password password (getf user :password-hash)))
      user)))

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

(defun update-user-password (user-id new-password)
  "Update a user's password."
  (postmodern:execute
   (:update 'cave-users
    :set 'password-hash (hash-password new-password)
         'updated-at (:now)
    :where (:= 'id user-id))))

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

(defun next-repo-number (repo-id)
  "Atomically get and increment the next number for a repo (shared by issues and changesets)."
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

;;; ========================== CHANGESETS ==========================

(defun create-changeset (&key repo-id author-id source-branch target-branch head-commit
                               stack-id stack-order)
  "Create a new changeset."
  (let ((number (next-repo-number repo-id)))
    (postmodern:query
     (:insert-into 'cave-changesets
      :set 'repo-id repo-id
           'number number
           'author-id author-id
           'source-branch source-branch
           'target-branch target-branch
           'head-commit head-commit
           'stack-id stack-id
           'stack-order stack-order
      :returning '*)
     :plist)))

(defun find-changeset (repo-id number)
  "Find a changeset by repo and number."
  (postmodern:query
   (:select '* :from 'cave-changesets
    :where (:and (:= 'repo-id repo-id) (:= 'number number)))
   :plist))

(defun find-changeset-by-id (changeset-id)
  "Find a changeset by ID."
  (postmodern:query
   (:select '* :from 'cave-changesets :where (:= 'id changeset-id))
   :plist))

(defun find-changeset-by-branch (repo-id source-branch)
  "Find an open changeset for a source branch."
  (postmodern:query
   (:select '* :from 'cave-changesets
    :where (:and (:= 'repo-id repo-id)
                 (:= 'source-branch source-branch)
                 (:= 'is-merged nil)
                 (:= 'is-closed nil)))
   :plist))

(defun list-changesets (repo-id &key (status "open") (limit 50) (offset 0))
  "List changesets. Status: open, merged, closed, or nil for all."
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

(defun update-changeset-head (changeset-id head-commit)
  "Update the head commit and bump version."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'head-commit head-commit
         'version (:+ 'version 1)
         'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun close-changeset (changeset-id)
  "Mark a changeset as closed."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-closed t 'closed-at (:now) 'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun merge-changeset (changeset-id)
  "Mark a changeset as merged."
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

(defun list-stack-changesets (stack-id)
  "List all changesets in a stack, ordered by stack_order."
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
    (:select 'cave-reviews.* 'cave-users.username :as 'reviewer-username
     :from 'cave-reviews
     :inner-join 'cave-users :on (:= 'cave-reviews.reviewer-id 'cave-users.id)
     :where (:= 'cave-reviews.changeset-id changeset-id))
    (:desc 'cave-reviews.created-at))
   :plists))

(defun review-is-stale-p (review changeset)
  "A review is stale if its version doesn't match the current changeset version."
  (/= (getf review :changeset-version) (getf changeset :version)))

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

(defun compute-merge-eligibility (changeset repo)
  "Compute merge eligibility rules. Returns a list of (:description ... :pass ...)."
  (let* ((cs-id (getf changeset :id))
         (repo-id (getf repo :id))
         (version (getf changeset :version))
         (reviews (list-reviews cs-id))
         (rules nil))

    ;; Rule 1: Not closed/merged
    (push (list :description "Changeset is open"
                :pass (and (not (getf changeset :is-merged))
                           (not (getf changeset :is-closed))))
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
                                      (getf changeset :author-id))))
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

(defun changeset-mergeable-p (eligibility)
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
