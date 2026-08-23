(in-package #:cave)

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
  ;; Pinned issues first (pin_order ASC, NULLs last by Postgres default), then
  ;; newest. Matches GitHub's pinned-on-top behavior.
  (if status
      (postmodern:query
       (:limit
        (:order-by
         (:select '* :from 'cave-issues
          :where (:and (:= 'repo-id repo-id) (:= 'status status)))
         (:asc 'pin-order) (:desc 'created-at))
        limit offset)
       :plists)
      (postmodern:query
       (:limit
        (:order-by
         (:select '* :from 'cave-issues
          :where (:= 'repo-id repo-id))
         (:asc 'pin-order) (:desc 'created-at))
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

;;; ========================== ISSUE LABELS / ASSIGNEES / MILESTONES ====

(defun issue-labels (issue-id)
  "List of label strings on an issue."
  (postmodern:query
   (:order-by (:select 'label :from 'cave-issue-labels
               :where (:= 'issue-id issue-id))
              'label)
   :column))

(defun %dedupe-scoped-labels (labels)
  "Normalize LABELS, then enforce scoped/exclusive semantics: for labels of the
form `scope/value` (e.g. priority/high) only the LAST one in a scope is kept, so
setting priority/high replaces priority/low. Plain labels are unaffected."
  (let ((clean (remove-duplicates
                (remove-if #'uiop:emptyp
                           (mapcar (lambda (s) (string-trim " " s)) labels))
                :test #'equal :from-end t))
        (seen-scopes (make-hash-table :test 'equal))
        (result nil))
    ;; Walk from the end so the last value in each scope wins.
    (dolist (label (reverse clean))
      (let ((slash (position #\/ label)))
        (if (and slash (plusp slash))
            (let ((scope (subseq label 0 slash)))
              (unless (gethash scope seen-scopes)
                (setf (gethash scope seen-scopes) t)
                (push label result)))
            (push label result))))
    result))

(defun set-issue-labels (issue-id labels)
  "Replace an issue's labels with LABELS, honoring scoped/exclusive labels."
  (postmodern:with-transaction ()
    (postmodern:execute
     (:delete-from 'cave-issue-labels :where (:= 'issue-id issue-id)))
    (dolist (label (%dedupe-scoped-labels labels))
      (postmodern:execute
       (:insert-into 'cave-issue-labels :set 'issue-id issue-id 'label label)))))

(defun labels-in-repo (repo-id)
  "Distinct label strings used across a repo's issues (for filters/suggestions)."
  (postmodern:query
   "SELECT DISTINCT l.label FROM cave_issue_labels l
      JOIN cave_issues i ON i.id = l.issue_id
     WHERE i.repo_id = $1 ORDER BY l.label"
   repo-id :column))

(defun issue-assignees (issue-id)
  "Plists (:user-id :username) assigned to an issue."
  (postmodern:query
   (:select (:as 'cave-issue-assignees.user-id 'user-id) 'cave-users.username
    :from 'cave-issue-assignees
    :inner-join 'cave-users :on (:= 'cave-issue-assignees.user-id 'cave-users.id)
    :where (:= 'cave-issue-assignees.issue-id issue-id))
   :plists))

(defun set-issue-assignees (issue-id user-ids)
  "Replace an issue's assignees with USER-IDS (a list of user ids)."
  (postmodern:with-transaction ()
    (postmodern:execute
     (:delete-from 'cave-issue-assignees :where (:= 'issue-id issue-id)))
    (dolist (uid (remove-duplicates user-ids))
      (when uid
        (postmodern:execute
         (:insert-into 'cave-issue-assignees :set 'issue-id issue-id 'user-id uid))))))

(defun set-issue-milestone (issue-id milestone-id)
  "Set (or clear, when MILESTONE-ID is NIL) an issue's milestone."
  (postmodern:execute
   (:update 'cave-issues :set 'milestone-id (or milestone-id :null)
            'updated-at (:now)
    :where (:= 'id issue-id))))

(defun create-milestone (&key repo-id title description due-on)
  "Create a milestone. Returns its plist."
  (postmodern:query
   (:insert-into 'cave-milestones
    :set 'repo-id repo-id 'title title
         'description (or description :null)
         'due-on (or due-on :null)
    :returning '*)
   :plist))

(defun list-milestones (repo-id &key (state "open"))
  "List a repo's milestones. STATE nil lists all."
  (if state
      (postmodern:query
       (:order-by (:select '* :from 'cave-milestones
                   :where (:and (:= 'repo-id repo-id) (:= 'state state)))
                  'title)
       :plists)
      (postmodern:query
       (:order-by (:select '* :from 'cave-milestones :where (:= 'repo-id repo-id))
                  'state 'title)
       :plists)))

(defun find-milestone (milestone-id)
  (postmodern:query
   (:select '* :from 'cave-milestones :where (:= 'id milestone-id)) :plist))

(defun update-milestone (milestone-id &key title description due-on state)
  (when title (postmodern:execute (:update 'cave-milestones :set 'title title :where (:= 'id milestone-id))))
  (when description (postmodern:execute (:update 'cave-milestones :set 'description description :where (:= 'id milestone-id))))
  (when due-on (postmodern:execute (:update 'cave-milestones :set 'due-on due-on :where (:= 'id milestone-id))))
  (when state (postmodern:execute (:update 'cave-milestones :set 'state state :where (:= 'id milestone-id)))))

(defun delete-milestone (milestone-id)
  (postmodern:execute (:delete-from 'cave-milestones :where (:= 'id milestone-id))))

(defun milestone-issue-counts (milestone-id)
  "Return (values open-count closed-count) for a milestone."
  (let ((open (postmodern:query
               (:select (:count '*) :from 'cave-issues
                :where (:and (:= 'milestone-id milestone-id) (:= 'status "open")))
               :single))
        (closed (postmodern:query
                 (:select (:count '*) :from 'cave-issues
                  :where (:and (:= 'milestone-id milestone-id) (:= 'status "closed")))
                 :single)))
    (values (or open 0) (or closed 0))))

;;; ========================== NOTIFICATIONS &amp; WATCHES ==================

(defun create-notification (&key user-id repo-id kind subject link)
  "Insert an in-app notification for a user."
  (postmodern:execute
   (:insert-into 'cave-notifications
    :set 'user-id user-id
         'repo-id (or repo-id :null)
         'kind kind 'subject subject 'link link)))

(defun list-notifications (user-id &key unread-only (limit 50))
  "List a user's notifications, newest first."
  (if unread-only
      (postmodern:query
       (:limit (:order-by (:select '* :from 'cave-notifications
                           :where (:and (:= 'user-id user-id) (:= 'is-read nil)))
                          (:desc 'created-at))
               limit)
       :plists)
      (postmodern:query
       (:limit (:order-by (:select '* :from 'cave-notifications
                           :where (:= 'user-id user-id))
                          (:desc 'created-at))
               limit)
       :plists)))

(defun find-notification (notification-id user-id)
  "Fetch one notification scoped to its owner, or NIL."
  (postmodern:query
   (:select '* :from 'cave-notifications
    :where (:and (:= 'id notification-id) (:= 'user-id user-id)))
   :plist))

(defun count-unread-notifications (user-id)
  "Number of unread notifications for a user."
  (or (postmodern:query
       (:select (:count '*) :from 'cave-notifications
        :where (:and (:= 'user-id user-id) (:= 'is-read nil)))
       :single)
      0))

(defun mark-notification-read (notification-id user-id)
  "Mark one notification read (scoped to its owner)."
  (postmodern:execute
   (:update 'cave-notifications :set 'is-read t
    :where (:and (:= 'id notification-id) (:= 'user-id user-id)))))

(defun mark-all-notifications-read (user-id)
  "Mark all of a user's notifications read."
  (postmodern:execute
   (:update 'cave-notifications :set 'is-read t
    :where (:and (:= 'user-id user-id) (:= 'is-read nil)))))

(defun watch-repo (repo-id user-id)
  "Subscribe a user to a repo's notifications (idempotent)."
  (postmodern:execute
   "INSERT INTO cave_repo_watches (repo_id, user_id) VALUES ($1, $2)
    ON CONFLICT DO NOTHING"
   repo-id user-id))

(defun unwatch-repo (repo-id user-id)
  (postmodern:execute
   (:delete-from 'cave-repo-watches
    :where (:and (:= 'repo-id repo-id) (:= 'user-id user-id)))))

(defun watching-repo-p (repo-id user-id)
  (and user-id
       (postmodern:query
        (:select t :from 'cave-repo-watches
         :where (:and (:= 'repo-id repo-id) (:= 'user-id user-id)))
        :single)))

(defun repo-watcher-ids (repo-id)
  "User ids watching a repo."
  (postmodern:query
   (:select 'user-id :from 'cave-repo-watches :where (:= 'repo-id repo-id))
   :column))

;;; ========================== PINNED ISSUES ===========================

(defun pin-issue (issue-id repo-id)
  "Pin an issue to the end of the repo's pinned list."
  (let ((next (1+ (or (postmodern:query
                       "SELECT COALESCE(MAX(pin_order), 0) FROM cave_issues WHERE repo_id = $1"
                       repo-id :single)
                      0))))
    (postmodern:execute
     (:update 'cave-issues :set 'pin-order next :where (:= 'id issue-id)))))

(defun unpin-issue (issue-id)
  (postmodern:execute
   (:update 'cave-issues :set 'pin-order :null :where (:= 'id issue-id))))

;;; ========================== REACTIONS ===============================

(defun toggle-reaction (target-type target-id user-id emoji)
  "Add EMOJI reaction if absent, remove it if present. Returns :added or :removed."
  (if (postmodern:query
       (:select t :from 'cave-reactions
        :where (:and (:= 'target-type target-type) (:= 'target-id target-id)
                     (:= 'user-id user-id) (:= 'emoji emoji)))
       :single)
      (progn
        (postmodern:execute
         (:delete-from 'cave-reactions
          :where (:and (:= 'target-type target-type) (:= 'target-id target-id)
                       (:= 'user-id user-id) (:= 'emoji emoji))))
        :removed)
      (progn
        (postmodern:execute
         "INSERT INTO cave_reactions (target_type, target_id, user_id, emoji)
          VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING"
         target-type target-id user-id emoji)
        :added)))

(defun list-reactions (target-type target-id &optional user-id)
  "Reaction summary for a target: plists (:emoji :count :mine) ordered by count."
  (postmodern:query
   "SELECT emoji,
           COUNT(*) AS count,
           BOOL_OR(user_id = $3) AS mine
    FROM cave_reactions
    WHERE target_type = $1 AND target_id = $2
    GROUP BY emoji ORDER BY count DESC, emoji"
   target-type target-id (or user-id -1) :plists))

;;; ========================== ISSUE COMMENTS ==========================

(defun issue-comment-counts (issue-ids)
  "Hash table issue-id -> comment count for ISSUE-IDS (one query). Missing keys
mean zero. Used to show the per-row comment count on the issues list."
  (let ((h (make-hash-table)))
    (when issue-ids
      (dolist (row (postmodern:query
                    "SELECT issue_id, count(*) AS n FROM cave_issue_comments
                     WHERE issue_id = ANY($1) GROUP BY issue_id"
                    (coerce issue-ids 'vector) :rows))
        (setf (gethash (first row) h) (second row))))
    h))

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

(defun record-changeset-version (changeset-id version head-commit base-commit)
  "Snapshot a PR round's commit range (idempotent on changeset+version)."
  (postmodern:execute
   "INSERT INTO cave_changeset_versions (changeset_id, version, head_commit, base_commit)
    VALUES ($1, $2, $3, $4) ON CONFLICT (changeset_id, version) DO NOTHING"
   changeset-id version head-commit (or base-commit :null)))

(defun list-changeset-versions (changeset-id)
  "All recorded rounds for a PR, newest version first."
  (postmodern:query
   (:order-by (:select '* :from 'cave-changeset-versions
               :where (:= 'changeset-id changeset-id))
              (:desc 'version))
   :plists))

(defun find-changeset-version (changeset-id version)
  (postmodern:query
   (:select '* :from 'cave-changeset-versions
    :where (:and (:= 'changeset-id changeset-id) (:= 'version version)))
   :plist))

;;; ========================== CI SECRETS ==============================

(defun %secret-key-octets ()
  (ironclad:digest-sequence
   :sha256 (sb-ext:string-to-octets (or (config-value :secret-key) "cave-default-key")
                                    :external-format :utf-8)))

(defun encrypt-secret (plaintext)
  "AES-256-CBC encrypt PLAINTEXT; return base64(iv || ciphertext)."
  (let* ((key (%secret-key-octets))
         (iv (ironclad:make-random-salt 16))
         (cipher (ironclad:make-cipher :aes :key key :mode :cbc
                                       :initialization-vector iv))
         (data (sb-ext:string-to-octets plaintext :external-format :utf-8))
         (padlen (- 16 (mod (length data) 16)))
         (padded (concatenate '(vector (unsigned-byte 8)) data
                              (make-array padlen :element-type '(unsigned-byte 8)
                                                 :initial-element padlen)))
         (out (make-array (length padded) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher padded out)
    (cl-base64:usb8-array-to-base64-string
     (concatenate '(vector (unsigned-byte 8)) iv out))))

(defun decrypt-secret (b64)
  "Inverse of ENCRYPT-SECRET."
  (let* ((blob (cl-base64:base64-string-to-usb8-array b64))
         (iv (subseq blob 0 16))
         (ct (subseq blob 16))
         (cipher (ironclad:make-cipher :aes :key (%secret-key-octets) :mode :cbc
                                       :initialization-vector iv))
         (out (make-array (length ct) :element-type '(unsigned-byte 8))))
    (ironclad:decrypt cipher ct out)
    (let ((padlen (aref out (1- (length out)))))
      (sb-ext:octets-to-string (subseq out 0 (- (length out) padlen))
                               :external-format :utf-8))))

(defun set-secret (scope scope-id name value)
  "Create or replace an encrypted secret for a repo/org scope."
  (postmodern:execute
   "INSERT INTO cave_secrets (scope, scope_id, name, value_encrypted)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (scope, scope_id, name)
    DO UPDATE SET value_encrypted = EXCLUDED.value_encrypted"
   scope scope-id name (encrypt-secret value)))

(defun list-secret-names (scope scope-id)
  "Names only (never values) of a scope's secrets, for the Settings UI."
  (postmodern:query
   (:order-by (:select 'name :from 'cave-secrets
               :where (:and (:= 'scope scope) (:= 'scope-id scope-id)))
              'name)
   :column))

(defun delete-secret (scope scope-id name)
  (postmodern:execute
   (:delete-from 'cave-secrets
    :where (:and (:= 'scope scope) (:= 'scope-id scope-id) (:= 'name name)))))

(defun decrypted-secrets (scope scope-id)
  "Alist (name . value) for a scope, values decrypted. Internal use only."
  (loop for row in (postmodern:query
                    (:select 'name 'value-encrypted :from 'cave-secrets
                     :where (:and (:= 'scope scope) (:= 'scope-id scope-id)))
                    :rows)
        collect (cons (first row) (ignore-errors (decrypt-secret (second row))))))

(defun secrets-for-repo (repo)
  "Merged secrets for a repo's CI job: org secrets (when org-owned), overridden
by repo secrets. Returns an alist (name . value)."
  (let ((merged nil)
        (org-id (getf repo :org-id)))
    (when (and org-id (not (eq org-id :null)))
      (dolist (kv (decrypted-secrets "org" org-id)) (push kv merged)))
    (dolist (kv (decrypted-secrets "repo" (getf repo :id)))
      (setf merged (cons kv (remove (car kv) merged :key #'car :test #'equal))))
    merged))

(defun secrets-env-string (alist)
  "Format (name . value) pairs as newline-joined KEY=VALUE for the runner."
  (with-output-to-string (s)
    (loop for (name . value) in alist
          when value do (format s "~A=~A~%" name value))))

;;; ========================== PROTECTED BRANCHES ======================

(defun add-protected-branch (repo-id pattern &key (block-direct-push t) require-signed-commits)
  "Add or update a branch-protection rule."
  (postmodern:execute
   "INSERT INTO cave_protected_branches (repo_id, pattern, block_direct_push, require_signed_commits)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (repo_id, pattern) DO UPDATE
      SET block_direct_push = EXCLUDED.block_direct_push,
          require_signed_commits = EXCLUDED.require_signed_commits"
   repo-id pattern block-direct-push require-signed-commits))

(defun list-protected-branches (repo-id)
  (postmodern:query
   (:order-by (:select '* :from 'cave-protected-branches :where (:= 'repo-id repo-id))
              'pattern)
   :plists))

(defun delete-protected-branch (id repo-id)
  (postmodern:execute
   (:delete-from 'cave-protected-branches
    :where (:and (:= 'id id) (:= 'repo-id repo-id)))))

(defun %branch-pattern-match-p (pattern branch)
  "Match a protected-branch PATTERN against BRANCH: exact, `prefix/*`, or `*`."
  (cond ((string= pattern "*") t)
        ((string= pattern branch) t)
        ((uiop:string-suffix-p "/*" pattern)
         (uiop:string-prefix-p (subseq pattern 0 (- (length pattern) 1)) branch))
        (t nil)))

(defun branch-protection (repo-id branch)
  "Return the protection rule covering BRANCH, or NIL. First match wins."
  (loop for p in (list-protected-branches repo-id)
        when (%branch-pattern-match-p (getf p :pattern) branch)
          return p))

;;; ========================== DEPLOY KEYS =============================

(defun add-deploy-key (repo-id name public-key &key read-write)
  "Add a deploy (per-repo) SSH key. Returns the key plist."
  (postmodern:query
   (:insert-into 'cave-deploy-keys
    :set 'repo-id repo-id 'name name 'public-key public-key
         'fingerprint (compute-ssh-fingerprint public-key)
         'read-write (if read-write t nil)
    :returning '*)
   :plist))

(defun list-deploy-keys (repo-id)
  (postmodern:query
   (:order-by (:select '* :from 'cave-deploy-keys :where (:= 'repo-id repo-id)) 'name)
   :plists))

(defun delete-deploy-key (id repo-id)
  (postmodern:execute
   (:delete-from 'cave-deploy-keys
    :where (:and (:= 'id id) (:= 'repo-id repo-id)))))

(defun find-deploy-key-by-id (id)
  "Deploy key record by id, or NIL."
  (postmodern:query
   (:select '* :from 'cave-deploy-keys :where (:= 'id id)) :plist))

(defun all-deploy-keys-with-repo ()
  "All deploy keys joined with owner/name, for the authorized_keys file."
  (postmodern:query
   "SELECT d.id, d.public_key, d.fingerprint, d.read_write, d.repo_id,
           r.name AS repo_name, COALESCE(o.name, u.username) AS owner_name
      FROM cave_deploy_keys d
      JOIN cave_repos r ON r.id = d.repo_id
      LEFT JOIN cave_orgs o ON o.id = r.org_id
      LEFT JOIN cave_users u ON u.id = r.owner_id"
   :plists))

(defun close-pull-request (changeset-id)
  "Mark a pull request as closed (without merging)."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-closed t 'closed-at (:now) 'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun reopen-pull-request (changeset-id)
  "Reopen a previously-closed (un-merged) pull request."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-closed nil 'closed-at :null 'updated-at (:now)
    :where (:and (:= 'id changeset-id) (:= 'is-merged nil)))))

(defun merge-pull-request (changeset-id)
  "Mark a pull request as merged."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-merged t 'merged-at (:now) 'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun set-pull-request-draft (changeset-id draft)
  "Mark a PR draft (work-in-progress) or ready. Clears auto-merge when drafting."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'is-draft (if draft t nil)
         'auto-merge-strategy (if draft :null 'auto-merge-strategy)
         'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun set-pull-request-auto-merge (changeset-id strategy user-id)
  "Arm (STRATEGY non-nil) or disarm (NIL) auto-merge for a PR."
  (postmodern:execute
   (:update 'cave-changesets
    :set 'auto-merge-strategy (or strategy :null)
         'auto-merge-by (if strategy user-id :null)
         'updated-at (:now)
    :where (:= 'id changeset-id))))

(defun pull-requests-armed-for-head (repo-id commit-sha)
  "Open, auto-merge-armed PRs whose head is COMMIT-SHA (for status-driven auto-merge)."
  (postmodern:query
   (:select '* :from 'cave-changesets
    :where (:and (:= 'repo-id repo-id)
                 (:= 'head-commit commit-sha)
                 (:= 'is-merged nil)
                 (:= 'is-closed nil)
                 (:not (:is-null 'auto-merge-strategy))))
   :plists))

;;; ========================== STACKS ==========================

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

    ;; Rule 1a: Not a draft. A draft PR is a work-in-progress and never merges
    ;; (manually or via auto-merge) until marked ready.
    (when (getf pr :is-draft)
      (push (list :description "Pull request is a draft" :pass nil) rules))

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

