# Design: Native Dependency Updates & Security Alerts

Status: proposed. Owner: atgreen. Last updated: 2026-06-18.

Cave's answer to Dependabot — not a bot bolted on, but a supply-chain layer of
the forge. The dependency graph, advisory matches, fix PRs, and org policy live
in cave's own data model, so they compose with CI history, code search, reviews,
releases, issues, and notifications in ways an external bot (Renovate) cannot.

## Guiding principle

Borrow the *extraction* moat, own the *platform* value. Renovate/Dependabot's
real cost is the 90+ ecosystem manager/datasource/versioning code. We do **not**
reimplement it. Instead, runner jobs run `syft` (SBOM) and `osv-scanner` as
*producers*; cave is the *system of record* and builds graph/alert/policy/PR
features on top. The only versioning code we own is an incremental native
semver path that unlocks "re-match the stored graph the instant a CVE drops"
without re-scanning every repo.

## Locked decisions

1. **Scope: default branch only** for v1. Matches Dependabot; avoids per-branch
   row explosion and branch-delete cleanup. The schema carries `ref` anyway so
   multi-branch is a later widening, not a migration.
2. **Suppression expiry is enforced in the matcher.** Each match pass reopens
   alerts whose `risk_accepted` suppression has lapsed. No separate sweeper.
3. **Fixability is computed lazily** on first fix-request and cached on the
   alert. The matcher only knows versions; classifying a fix is ecosystem-
   mechanical and belongs to the `deps-fix` runner job.

## Architecture

```
                ┌─ sync-advisories (in-process, timer) ─┐
   OSV exports ─┤  upsert cave_advisories + _affected   ├─► MATCHER ─► cave_dep_alerts
                └────────────────────────────────────────┘    ▲             │
                                                               │             ├─► digest notify (notify.lisp)
                                                               │             ├─► dependency-dashboard issue
   git push ─► post-receive ─► runner: syft ─► CycloneDX ─► /internal/repos/:id/deps ─┘
   (default ref only)                          SBOM         (ingest-sbom: upsert + gen-sweep, then rematch-repo)

   fix requested ─► runner: deps-fix ─► worktree mutate + repo's own CI ─► create-pull-request ─► fix_pr_id
                                        (green-gated; auto-merge per policy + CI history)
```

Two independent producers feed the DB; the matcher joins them. The graph is
useful with zero advisories (org-wide queries); advisories are useful across all
repos. Matching runs **server-side over stored advisory ranges**, so a new CVE
re-matches the existing graph with no repo re-scan — the native superpower.

## Data model

`cave_repo_deps` is recomputable per (repo, ref). `cave_dep_alerts` is a derived,
disposable materialization of current matches. Durable user intent lives in
`cave_dep_suppressions`, keyed by the stable coordinate (not a row id) so it
survives version churn and full graph rebuilds. Governance lives in
`cave_org_dep_policy` and *caps* per-repo `.cave/deps.yml`.

### Migration 46 — the dependency graph

```sql
(46 . "-- Resolved dependency graph, populated from SBOMs on push (default ref).
CREATE TABLE cave_repo_deps (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  ref VARCHAR(256) NOT NULL,            -- branch the SBOM was taken from
  manifest_path VARCHAR(1024) NOT NULL, -- 'package.json', 'go.mod', ...
  ecosystem VARCHAR(64) NOT NULL,       -- OSV ecosystem: 'npm','Go','PyPI','crates.io'
  package_name VARCHAR(512) NOT NULL,
  version VARCHAR(256) NOT NULL,        -- resolved/locked version
  purl VARCHAR(1024) NOT NULL,          -- pkg:npm/lodash@4.17.20  (canonical key)
  is_direct BOOLEAN NOT NULL DEFAULT TRUE,
  scope VARCHAR(32),                    -- 'runtime','dev', NULL
  generation BIGINT NOT NULL,           -- atomic-replace marker per scan
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, ref, manifest_path, purl)
);
CREATE INDEX idx_repo_deps_repo ON cave_repo_deps (repo_id, ref);
CREATE INDEX idx_repo_deps_pkg ON cave_repo_deps (ecosystem, package_name);")
```

### Migration 47 — the advisory database

```sql
(47 . "-- OSV advisories, mirrored by `cave deps sync-advisories`.
CREATE TABLE cave_advisories (
  id BIGSERIAL PRIMARY KEY,
  osv_id VARCHAR(64) NOT NULL UNIQUE,   -- 'GHSA-...','CVE-...','PYSEC-...'
  summary TEXT,
  details TEXT,
  aliases TEXT[] NOT NULL DEFAULT '{}', -- cross-IDs (CVE<->GHSA<->PYSEC)
  severity VARCHAR(16),                 -- 'low'/'moderate'/'high'/'critical'
  cvss_score NUMERIC(3,1),              -- 0.0-10.0, NULL if unknown
  refs JSONB NOT NULL DEFAULT '[]',
  published_at TIMESTAMPTZ,
  modified_at TIMESTAMPTZ,              -- drives incremental sync
  withdrawn_at TIMESTAMPTZ              -- non-NULL => ignore in matching
);
CREATE INDEX idx_advisories_modified ON cave_advisories (modified_at);
CREATE INDEX idx_advisories_aliases ON cave_advisories USING GIN (aliases);

-- OSV affected[].ranges[] flattened to (introduced, fixed) pairs for SQL matching.
CREATE TABLE cave_advisory_affected (
  id BIGSERIAL PRIMARY KEY,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  ecosystem VARCHAR(64) NOT NULL,
  package_name VARCHAR(512) NOT NULL,
  range_type VARCHAR(16) NOT NULL,      -- 'SEMVER','ECOSYSTEM','GIT'
  introduced VARCHAR(256),              -- '0' = from beginning
  fixed VARCHAR(256),                   -- NULL if no fix yet
  last_affected VARCHAR(256)            -- alt. upper bound
);
CREATE INDEX idx_advisory_affected_pkg
  ON cave_advisory_affected (ecosystem, package_name);")
```

### Migration 48 — the matches (derived, disposable)

```sql
(48 . "-- A vulnerable dependency occurrence; recomputed by the matcher.
CREATE TABLE cave_dep_alerts (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  dep_id BIGINT NOT NULL REFERENCES cave_repo_deps(id) ON DELETE CASCADE,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  state VARCHAR(16) NOT NULL DEFAULT 'open'
    CHECK (state IN ('open','dismissed','fixed','auto_fixed')),
  fix_version VARCHAR(256),             -- nearest fixed version, for the PR
  fix_kind VARCHAR(20),                 -- lazily cached: manifest/lockfile/override/transitive_parent/none
  fix_pr_id BIGINT REFERENCES cave_changesets(id) ON DELETE SET NULL,
  reachable BOOLEAN,                    -- zoekt annotation; NULL = unknown
  detected_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(dep_id, advisory_id)
);
CREATE INDEX idx_dep_alerts_repo ON cave_dep_alerts (repo_id, state);")
```

> Verify the PRs table is `cave_changesets` (model.lisp calls them changesets)
> before wiring `fix_pr_id`.

### Migration 49 — durable intent + org governance

```sql
(49 . "-- Durable suppressions; survive graph rebuilds and version churn.
CREATE TABLE cave_dep_suppressions (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  ecosystem VARCHAR(64) NOT NULL,
  package_name VARCHAR(512) NOT NULL,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  reason VARCHAR(32) NOT NULL,          -- 'not_used','no_fix','risk_accepted'
  note TEXT,
  created_by BIGINT REFERENCES cave_users(id),
  expires_at TIMESTAMPTZ,               -- risk acceptances lapse; matcher reopens
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, ecosystem, package_name, advisory_id)
);
CREATE INDEX idx_dep_suppressions_repo ON cave_dep_suppressions (repo_id);

-- Org policy caps per-repo .cave/deps.yml. Repo config can only narrow this.
CREATE TABLE cave_org_dep_policy (
  org_id BIGINT PRIMARY KEY REFERENCES cave_orgs(id) ON DELETE CASCADE,
  allowed_ecosystems TEXT[],            -- NULL = all
  license_allow TEXT[],
  license_deny TEXT[],
  automerge_ceiling VARCHAR(8) NOT NULL DEFAULT 'none'
    CHECK (automerge_ceiling IN ('none','patch','minor','major')),
  security_always_on BOOLEAN NOT NULL DEFAULT TRUE,
  freeze_windows JSONB NOT NULL DEFAULT '[]',
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
)")
```

## Ingestion & matching surface

### Producer A — advisory sync (global, in-process)

New clingon subcommand beside the existing `sync-mirrors` / `sync-themes` in
`main.lisp`, run on a timer:

```lisp
;; cave deps sync-advisories
(defun sync-osv-advisories (&key (ecosystems *tracked-ecosystems*) since)
  "Mirror OSV into cave_advisories + cave_advisory_affected. Incremental on modified_at.
   GETs https://osv-vulnerabilities.storage.googleapis.com/<eco>/all.zip per ecosystem.
   On each record: resolve aliases to a canonical row (GIN lookup on aliases),
   merge-or-insert, then flatten affected[].ranges[] -> (introduced . fixed) rows.
   Enqueue rematch-advisory for changed records as a batched background sweep."
  ...)
```

`since` = `MAX(modified_at)`. Alias resolution before insert: `SELECT ... WHERE
osv_id = ANY(aliases) OR osv_id = $new OR $new = ANY(aliases)`; merge into the
existing canonical record (union aliases, richest summary, max cvss) so one bug
is one advisory, not three.

### Producer B — dep extraction (per repo, on push)

Hook the existing `schedule-automations ... "post_receive"` call site
(`main.lisp:847`) to also enqueue a built-in `deps-scan` task **for the default
branch only**. The runner emits a CycloneDX SBOM (`syft`) and POSTs it back,
authenticated like the post-receive forward already is:

```lisp
;; server.lisp — internal, runner-token auth (mirror the post-receive endpoint)
("/internal/repos/:id/deps" :method :post) ...   ; -> (ingest-sbom repo-id ref sbom-json)

;; model.lisp
(defun ingest-sbom (repo-id ref sbom)
  "Upsert cave_repo_deps from a CycloneDX SBOM, atomically replacing this (repo,ref)."
  (let ((gen (next-generation)))
    (dolist (c (sbom-components sbom)) (upsert-repo-dep repo-id ref c gen))
    (sweep-stale-deps repo-id ref gen)            ; DELETE WHERE generation < gen
    (rematch-repo repo-id ref)))
```

### The matcher

```lisp
(defun rematch-repo (repo-id ref) ...)        ; deps changed: scan this repo's deps
(defun rematch-advisory (advisory-id) ...)    ; CVE dropped: batched sweep over all deps for its pkgs

(defun dep-affected-p (dep affected)
  "Does DEP's version fall in AFFECTED's [introduced, fixed) range?"
  (and (string= (getf dep :ecosystem)    (getf affected :ecosystem))
       (string= (getf dep :package-name) (getf affected :package-name))
       (version>= (getf dep :version) (getf affected :introduced) (getf dep :ecosystem))
       (or (null (getf affected :fixed))
           (version<  (getf dep :version) (getf affected :fixed) (getf dep :ecosystem)))))
```

On match, consult `cave_dep_suppressions`: an active, unexpired suppression
=> alert `state=dismissed`; a lapsed `risk_accepted` => reopen (`state=open`).
New `open` alerts fire a batched notification and update the dependency-dashboard
issue.

`version>=`/`version<` dispatch on ecosystem. **Bootstrap with `osv-scanner`**
in the runner (correct across all ecosystems day one; cave ingests matched
alerts + stores advisories for display). **Build the native semver path** in
parallel (covers npm/crates/Go-modules, the bulk of alerts) so instant
rematch-on-new-CVE lights up without a rescan. Add PEP 440 / Maven as demand
warrants. The schema supports both unchanged.

### Fix pipeline (`deps-fix` runner job, lazy)

Triggered from the dashboard issue or an API call, not by the matcher. Classifies
`fix_kind` (knows the ecosystem), mutates in a worktree/chamber, **runs the
repo's own CI via the workflow runner, opens the PR only if green** via
`create-pull-request`, stamps `fix_pr_id`, caches `fix_kind` on the alert.

| `fix_kind` | situation | mutation |
|---|---|---|
| `manifest` | direct, pinned in manifest | edit manifest, regen lockfile |
| `lockfile` | direct, range already permits fix | lockfile-only bump |
| `override` | transitive, ecosystem supports pins | inject overrides/resolutions/replace |
| `transitive_parent` | transitive, no override | bump the parent |
| `none` | no fixed version | alert only, no PR |

Auto-merge gate: `fix_kind ∈ {lockfile,manifest}` ∧ bump ≤ org `automerge_ceiling`
∧ CI green ∧ historical pass-rate for that package's past bumps ≥ threshold
(computed from `cave_workflow_runs` — cave owns this; Renovate buys it from Mend).

## UI & notifications (reuse, don't reinvent)

- **Dependency-dashboard issue**, one pinned per repo (Renovate's best idea),
  via the existing issue model: pending updates, alerts grouped by severity,
  checkboxes to trigger/retry a fix. Free markdown (cl-commonmark), permissions,
  notifications.
- **Digest notifications** in `notify.lisp`: a 40-dep bump round is one email.
- **Repo Dependencies tab** + org **Insights/Security** dashboard (Spinneret in
  `views.lisp`); inline "N behind / M CVE" badges in the manifest file view.
- **Prometheus**: dependency freshness + open-alert counts in `metrics.lisp`.
- **Reachability** (`reachable`): when an advisory carries affected symbols,
  query zoekt for usage; `false` sorts the alert down and dampens auto-PR
  urgency but never auto-dismisses (index miss is heuristic, not proof).

## Scale notes

- `(ecosystem, package_name)` indexes on both `cave_repo_deps` and
  `cave_advisory_affected` keep both match directions index-bound.
- A popular-package CVE fans out to many repos: `rematch-advisory` is a batched
  background sweep, not inline in `sync-advisories`; log/meter alerts opened.
- Advisory sync incremental on `MAX(modified_at)`; graph rebuilds per (repo,ref)
  so a push never re-matches the whole instance.

## Build order (MVP first)

1. Migrations 46–49 + `model.lisp` query layer.
2. `cave deps sync-advisories` (in-process, timer) — advisory DB populated.
3. `deps-scan` runner task + `/internal/repos/:id/deps` ingest — graph populated.
4. Matcher (osv-scanner bootstrap) + alerts + dependency-dashboard issue.
5. Native semver path → instant rematch-on-new-CVE.
6. `deps-fix` pipeline + CI-gated auto-merge.
7. Org policy + per-repo `.cave/deps.yml` (org caps repo).
8. Dashboards, badges, reachability, metrics.
