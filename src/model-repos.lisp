(in-package #:cave)

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

(defun search-public-repos (&key query language (sort "recent") (limit 30) (offset 0))
  "Public repos for the Explore page. QUERY filters name/description (ILIKE);
LANGUAGE filters on the stored primary_language; SORT is recent|newest|name.
Paginated with LIMIT/OFFSET ($1/$2; filter params follow)."
  (let* ((order (cond ((equal sort "newest") "r.created_at DESC")
                      ((equal sort "name") "lower(r.name) ASC")
                      (t "COALESCE(r.last_pushed_at, r.updated_at) DESC")))
         (qq (and query (plusp (length (string-trim " " query))) (string-trim " " query)))
         (like (and qq (format nil "%~A%" qq)))
         (base "SELECT r.*, COALESCE(o.name, u.username) AS owner_name
                FROM cave_repos r
                LEFT JOIN cave_orgs o ON o.id = r.org_id
                LEFT JOIN cave_users u ON u.id = r.owner_id
                WHERE r.is_private = false"))
    (cond
      ((and like language)
       (postmodern:query
        (format nil "~A AND (r.name ILIKE $3 OR r.description ILIKE $3) AND r.primary_language = $4 ORDER BY ~A LIMIT $1 OFFSET $2" base order)
        limit offset like language :plists))
      (like
       (postmodern:query
        (format nil "~A AND (r.name ILIKE $3 OR r.description ILIKE $3) ORDER BY ~A LIMIT $1 OFFSET $2" base order)
        limit offset like :plists))
      (language
       (postmodern:query
        (format nil "~A AND r.primary_language = $3 ORDER BY ~A LIMIT $1 OFFSET $2" base order)
        limit offset language :plists))
      (t
       (postmodern:query
        (format nil "~A ORDER BY ~A LIMIT $1 OFFSET $2" base order)
        limit offset :plists)))))

(defun count-public-repos (&key query language)
  "Total public repos matching the Explore filters (for pagination)."
  (let* ((qq (and query (plusp (length (string-trim " " query))) (string-trim " " query)))
         (like (and qq (format nil "%~A%" qq)))
         (base "SELECT COUNT(*) FROM cave_repos WHERE is_private = false"))
    (or (cond
          ((and like language)
           (postmodern:query
            (format nil "~A AND (name ILIKE $1 OR description ILIKE $1) AND primary_language = $2" base)
            like language :single))
          (like
           (postmodern:query
            (format nil "~A AND (name ILIKE $1 OR description ILIKE $1)" base) like :single))
          (language
           (postmodern:query
            (format nil "~A AND primary_language = $1" base) language :single))
          (t (postmodern:query base :single)))
        0)))

(defun public-language-facets ()
  "Primary languages across public repos with counts, for Explore filter chips."
  (postmodern:query
   "SELECT primary_language AS language, COUNT(*) AS n
    FROM cave_repos WHERE is_private = false AND primary_language IS NOT NULL
    GROUP BY primary_language ORDER BY n DESC, primary_language" :plists))

(defun set-repo-primary-language (repo-id lang)
  "Store a repo's primary language (NIL clears it)."
  (postmodern:execute
   (:update 'cave-repos :set 'primary-language (or lang :null)
            :where (:= 'id repo-id))))

(defun list-orgs ()
  "All organizations, alphabetical (for the Explore people/orgs directory)."
  (postmodern:query (:order-by (:select '* :from 'cave-orgs) 'name) :plists))

(defun trending-public-repos (&key (days 7) (limit 6))
  "Public repos ranked by page views over the last DAYS — Cave's trending signal."
  (postmodern:query
   (format nil "SELECT r.*, COALESCE(o.name, u.username) AS owner_name,
                       COUNT(pv.id) AS views
                FROM cave_page_views pv
                JOIN cave_repos r ON r.id = pv.repo_id
                LEFT JOIN cave_orgs o ON o.id = r.org_id
                LEFT JOIN cave_users u ON u.id = r.owner_id
                WHERE r.is_private = false
                  AND pv.viewed_at > NOW() - INTERVAL '~D days'
                GROUP BY r.id, o.name, u.username
                ORDER BY views DESC LIMIT $1" days)
   limit :plists))

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
  "List repos owned by a user, most recently changed first."
  (if include-private
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos :where (:= 'owner-id user-id))
        (:desc (:coalesce 'last-pushed-at 'updated-at)))
       :plists)
      (postmodern:query
       (:order-by
        (:select '* :from 'cave-repos
         :where (:and (:= 'owner-id user-id) (:= 'is-private nil)))
        (:desc (:coalesce 'last-pushed-at 'updated-at)))
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

(defun set-repo-visibility (repo-id &key (private t))
  "Set a repo's visibility. PRIVATE t makes it private, NIL makes it public."
  (postmodern:execute
   (:update 'cave-repos
    :set 'is-private private 'updated-at (:now)
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

(defun reap-stale-workflow-jobs (&key (max-minutes 120) (max-attempts 3)
                                      (assigned-grace-minutes 10))
  "Recover wedged workflow jobs. A job is ABANDONED when its runner is gone /
   offline / hasn't heartbeat in 3 min, or it has sat 'assigned' (never started)
   past ASSIGNED-GRACE-MINUTES — typically a runner that died or restarted (e.g.
   a cave deploy drops the gRPC streams). Such a job otherwise blocks its runner
   forever via the one-task-per-runner check.

   Outcomes:
   - REQUEUE (retry) an abandoned job that still has attempts left and hasn't hit
     the hard MAX-MINUTES timeout — a runner restart shouldn't kill a build.
   - FAIL (and finalize the run) a job past MAX-MINUTES, or an abandoned job that
     has exhausted MAX-ATTEMPTS. Failing finalizes required-check merge gating.
   Returns (values requeued-count failed-run-count)."
  (let* ((requeued
           ;; Recoverable: abandoned, retries left, not yet hard-timed-out.
           (postmodern:query
            ;; The updated table j may not be referenced inside the FROM
            ;; clause's join tree (Postgres: \"invalid reference to FROM-clause
            ;; entry for table j\"), so the runner lookup is a correlated
            ;; NOT EXISTS in WHERE — where j IS in scope. NOT EXISTS(online &
            ;; fresh runner) reproduces the old LEFT JOIN test
            ;; (r.id IS NULL OR r.status <> 'online' OR r.last_seen_at stale).
            "UPDATE cave_workflow_jobs j
                SET status='queued', runner_id=NULL, started_at=NULL,
                    finished_at=NULL, attempts=attempts+1
               FROM cave_workflow_runs w
              WHERE w.id = j.workflow_run_id
                AND j.status IN ('running','assigned')
                AND j.attempts < $2
                AND COALESCE(w.started_at, j.created_at) >= now() - make_interval(mins => $1::int)
                AND ( NOT EXISTS (SELECT 1 FROM cave_runners r
                                   WHERE r.id = j.runner_id
                                     AND r.status = 'online'
                                     AND r.last_seen_at >= now() - interval '3 minutes')
                      OR (j.status = 'assigned' AND j.started_at IS NULL
                          AND j.created_at < now() - make_interval(mins => $3::int)) )
            RETURNING j.id"
            max-minutes max-attempts assigned-grace-minutes :column))
         (failed-runs
           ;; Unrecoverable: hard timeout, or abandoned with no attempts left.
           (postmodern:query
            ;; Same target-table-in-FROM restriction as the requeue query above:
            ;; the runner-abandoned test is a correlated NOT EXISTS on j.
            "UPDATE cave_workflow_jobs j
                SET status='failure', finished_at=now()
               FROM cave_workflow_runs w
              WHERE w.id = j.workflow_run_id
                AND j.status IN ('running','assigned')
                AND ( COALESCE(w.started_at, j.created_at) < now() - make_interval(mins => $1::int)
                      OR ( j.attempts >= $2
                           AND NOT EXISTS (SELECT 1 FROM cave_runners r
                                            WHERE r.id = j.runner_id
                                              AND r.status = 'online'
                                              AND r.last_seen_at >= now() - interval '3 minutes') ) )
            RETURNING j.workflow_run_id"
            max-minutes max-attempts :column)))
    (dolist (rid (remove-duplicates failed-runs))
      (update-workflow-run-status rid "failure"))
    (values (length requeued) (length failed-runs))))

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
                                (timeout-seconds 0) continue-on-error privileged
                                cache-paths (env "") (matrix "") (output-defs "")
                                base-name)
  "Create a workflow job. NEEDS is a list of job name strings.
   RUNS-ON is a list of label strings the runner must have.
   TIMEOUT-SECONDS is the max job duration (0 means use default).
   CONTINUE-ON-ERROR when true prevents dependent jobs from being skipped on failure.
   PRIVILEGED when true runs the job container with --privileged (nested containers).
   CACHE-PATHS is a list of in-container directories to persist across runs."
  (let ((needs-str (if needs (format nil "~{~A~^,~}" needs) ""))
        (runs-on-str (if runs-on (format nil "~{~A~^,~}" runs-on) ""))
        (cache-str (if cache-paths (format nil "~{~A~^~%~}" cache-paths) "")))
    (postmodern:query
     (:insert-into 'cave-workflow-jobs
      :set 'workflow-run-id workflow-run-id
           'name name
           'base-name (or base-name name)
           'image image
           'needs needs-str
           'runs-on runs-on-str
           'timeout-seconds (or timeout-seconds 0)
           'continue-on-error (if continue-on-error t nil)
           'privileged (if privileged t nil)
           'cache-paths cache-str
           'env (or env "")
           'matrix (or matrix "")
           'output-defs (or output-defs "")
           'status (if needs "blocked" "queued")
      :returning '*)
     :plist)))

(defun set-job-outputs (job-id outputs-json)
  "Store a job's resolved job-level outputs (a JSON object string) so dependent
   jobs can read them via the needs.<job>.outputs context."
  (when (and outputs-json (stringp outputs-json) (plusp (length outputs-json)))
    (postmodern:execute
     (:update 'cave-workflow-jobs
      :set 'outputs outputs-json
      :where (:= 'id job-id)))))

(defun runner-owns-run-p (runner-id run-id)
  "True when RUNNER-ID was assigned a job of workflow run RUN-ID. Authorization
   gate for artifact registration — a runner may only attach artifacts to a run
   it actually worked on, never to an arbitrary run it names over gRPC."
  (and runner-id (integerp run-id) (plusp run-id)
       (postmodern:query
        (:limit (:select 'id :from 'cave-workflow-jobs
                 :where (:and (:= 'workflow-run-id run-id)
                              (:= 'runner-id runner-id)))
                1)
        :single)))

(defun create-artifact (&key workflow-run-id job-id name object-path (size-bytes 0))
  "Record an artifact for a run (replacing any existing one of the same name).
   Returns the row plist."
  (postmodern:execute
   (:delete-from 'cave-artifacts
    :where (:and (:= 'workflow-run-id workflow-run-id) (:= 'name name))))
  (postmodern:query
   (:insert-into 'cave-artifacts
    :set 'workflow-run-id workflow-run-id
         'job-id (or job-id :null)
         'name name
         'object-path object-path
         'size-bytes (or size-bytes 0)
    :returning '*)
   :plist))

(defun list-run-artifacts (workflow-run-id)
  "All artifacts for a run, ordered by name."
  (postmodern:query
   (:order-by (:select '* :from 'cave-artifacts :where (:= 'workflow-run-id workflow-run-id))
              'name)
   :plists))

(defun find-artifact (id)
  "An artifact row by id, or NIL."
  (postmodern:query (:select '* :from 'cave-artifacts :where (:= 'id id)) :plist))

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
                       AND dep.base_name = ANY(string_to_array(wj.needs, ',')) ~
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

(defun create-workflow-step (&key job-id step-order name (command "") (timeout-seconds 0)
                                  continue-on-error (env "") (id-name "") (if-cond "")
                                  (uses "") (with-inputs ""))
  "Create a workflow step. TIMEOUT-SECONDS is the max step duration (0 means no limit).
   CONTINUE-ON-ERROR when true allows the job to proceed even if this step fails.
   ENV is the step-level `env:` map as newline-joined KEY=VALUE.
   ID-NAME is the user `id:` (for steps.<id>.outputs); IF-COND the `if:` expression.
   USES is the `uses:` action ref (owner/repo@ref) for action steps; WITH-INPUTS
   the action's `with:` map as newline-joined KEY=VALUE."
  (postmodern:query
   (:insert-into 'cave-workflow-steps
    :set 'job-id job-id
         'step-order step-order
         'name (or name :null)
         'command (or command "")
         'timeout-seconds (or timeout-seconds 0)
         'continue-on-error (if continue-on-error t nil)
         'env (or env "")
         'id-name (or id-name "")
         'if-cond (or if-cond "")
         'uses (or uses "")
         'with-inputs (or with-inputs "")
    :returning '*)
   :plist))

(defun list-workflow-steps (job-id)
  "List all steps for a job, ordered."
  (postmodern:query
   (:order-by (:select '* :from 'cave-workflow-steps
                :where (:= 'job-id job-id))
              'step-order)
   :plists))

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

(defun %normalize-check-state (raw)
  "Map a raw commit-status/workflow status string to one of
success|failure|pending|running."
  (cond
    ((member raw '("success") :test #'equal) "success")
    ((member raw '("failure" "failed" "error" "cancelled") :test #'equal) "failure")
    ((member raw '("running") :test #'equal) "running")
    ((member raw '("skipped") :test #'equal) "success")
    (t "pending")))            ; queued, blocked, assigned, pending

(defun pull-request-checks (repo-id head-sha owner repo-name)
  "Return (values CHECKS ROLLUP) for HEAD-SHA, combining external commit statuses
with cave workflow jobs. CHECKS is a list of plists (:name :state :description
:url); state is success|failure|pending|running. ROLLUP is a plist
(:total :success :failure :pending :overall) where pending counts in-progress."
  (let ((checks nil))
    (dolist (s (list-commit-statuses repo-id head-sha))
      (push (list :name (getf s :context)
                  :state (%normalize-check-state (getf s :state))
                  :description (let ((d (getf s :description)))
                                 (if (and d (not (eq d :null))) d (getf s :state)))
                  :url (let ((u (getf s :target-url)))
                         (unless (or (null u) (eq u :null)) u)))
            checks))
    (dolist (run (workflow-runs-for-commit repo-id head-sha))
      (dolist (job (list-workflow-jobs (getf run :id)))
        (push (list :name (format nil "~A / ~A" (getf run :workflow-name) (getf job :name))
                    :state (%normalize-check-state (getf job :status))
                    :description (getf job :status)
                    :url (format nil "/~A/~A/runs/w/~A" owner repo-name (getf run :id)))
              checks)))
    (setf checks (nreverse checks))
    (let* ((total (length checks))
           (succ (count "success" checks :key (lambda (c) (getf c :state)) :test #'equal))
           (fail (count "failure" checks :key (lambda (c) (getf c :state)) :test #'equal))
           (pend (- total succ fail))
           (overall (cond ((zerop total) "none")
                          ((plusp fail) "failure")
                          ((plusp pend) "pending")
                          (t "success"))))
      (values checks (list :total total :success succ :failure fail
                           :pending pend :overall overall)))))

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

