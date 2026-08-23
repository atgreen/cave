(in-package #:cave)

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

(defun repos-with-signatures ()
  "Plists (:id :name :owner) for every repo that has recorded commit signatures.
OWNER is the org name, or the username for a personal repo. Used by `reverify`."
  (postmodern:query
   "SELECT DISTINCT r.id, r.name, COALESCE(o.name, u.username) AS owner
      FROM cave_commit_signatures s
      JOIN cave_repos r ON r.id = s.repo_id
      LEFT JOIN cave_orgs o ON o.id = r.org_id
      LEFT JOIN cave_users u ON u.id = r.owner_id"
   :plists))

(defun repo-recorded-shas (repo-id)
  "Commit SHAs that already have a signature row for REPO-ID."
  (postmodern:query
   (:select 'commit-sha :from 'cave-commit-signatures
    :where (:= 'repo-id repo-id))
   :column))

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

(defun repos-needing-dashboard-refresh (marker)
  "Repo ids that either have open alerts or already have a dashboard issue."
  (mapcar (lambda (r) (getf r :repo-id))
          (postmodern:query
           "SELECT DISTINCT repo_id FROM cave_dep_alerts WHERE state = 'open'
            UNION
            SELECT repo_id FROM cave_issues WHERE body LIKE $1"
           (format nil "%~A%" marker) :plists)))
