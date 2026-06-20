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

### Producer A — advisory sync (in-process) — BUILT

`cave-server sync-advisories` (clingon subcommand beside `sync-mirrors` /
`sync-themes` in `main.lisp`), run on a timer. Implemented in `src/osv.lisp`.

**Decision (deviation from the original plan):** query the **OSV REST API for the
packages the dependency graph actually contains**, rather than mirroring OSV's
per-ecosystem `all.zip` exports. Rationale: no zip library is available (only
`salza2` compression, and the container has no `unzip`); a forge only cares
about advisories for packages it hosts; and the API path is lighter and more
private. `dexador` + `jzon` (both already deps) carry it.

```lisp
(defun sync-osv-advisories (&key ecosystems (verbose t))
  "Query OSV for every package in the dependency graph, upsert matching
   advisories, and re-match affected repos.
   1. distinct-graph-packages -> (ecosystem . name) list
   2. POST /v1/querybatch (<=1000/req) -> distinct vulnerability ids
   3. GET /v1/vulns/<id> per id -> upsert-advisory + replace-advisory-affected
   4. rematch-advisory per upserted advisory")
```

Alias union happens in `upsert-advisory` (SQL `array_agg(DISTINCT …)` on
conflict). Affected ranges are flattened from `affected[].ranges[].events[]`
(introduced/fixed/last_affected), with a bare `versions[]` fallback; GIT ranges
are skipped. Severity is a best-effort qualitative label from
`database_specific.severity`.

Not yet done: incremental-by-`modified` (currently re-fetches and idempotently
upserts every matching advisory), `querybatch` page-token pagination, and a
numeric CVSS score. Verified end to end against the live OSV API (npm/lodash
returns real CVEs that match a seeded graph).

### Producer B — runner-based extraction — BUILT

Extraction runs **on a runner**, not the server — in-process syft was dropped (a
forge can't scan every repo on the box, and cave's repos are bare so there's no
working tree to scan anyway). Implemented in `src/sbom.lisp` + a hook in
`workflow.lisp`.

`enqueue-deps-scan` schedules a normal **workflow run** (marker
`workflow_name = "deps-scan"`): image `:deps-scan-image` (default
`ghcr.io/atgreen/cave-scan:main` — alpine + bash + syft, `Containerfile.scan`),
one step:

```
syft -q dir:/workspace -o cyclonedx-json=/tmp/sbom.json >/dev/null 2>&1 && cat /tmp/sbom.json
```

The runner already clones the repo into the container and streams step output to
`cave_workflow_steps.log`. The step writes syft to a file and `cat`s it, so the
log is clean CycloneDX JSON (the runner captures stdout+stderr combined).

**SBOM return path = the step log (Option B, chosen for security):** no token is
piped into the runner container. When the run finalizes (`check-workflow-job-
completion` → `update-workflow-run-status`), `maybe-ingest-scan-run` reads the
scan step's log, extracts the JSON (`%extract-json-object`, tolerant of stray
output), and calls `ingest-sbom-json` → `ingest-repo-deps` + dashboard refresh.

Triggers (all just *enqueue* a run; guarded by `:deps-scan-enabled`):
- **Push** — post-receive enqueues a scan on default-branch pushes.
- **CLI** — `cave-server deps-scan --repo o/n` enqueues; `--sbom FILE` instead
  ingests a pre-built SBOM directly (no runner).
- (cron/API can call `enqueue-deps-scan` too.)

The `/-/internal/repos/.../deps` HTTP endpoint + `sbom->deps` parser remain (used
by `--sbom` and available for other producers). `parse-purl` handles npm scopes,
Go module paths, Maven group:artifact.

**Operational requirement:** a runner must be running that can pull
`:deps-scan-image` and run podman steps. With no runner, scans queue harmlessly.

Not yet done: per-component direct/transitive detection (defaults to direct),
per-manifest grouping beyond syft's location hints, and `cave-scan` image build
proven end-to-end on a live runner.

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

### Fix pipeline (`deps-fix`) — BUILT (spine)

Implemented in `src/deps-fix.lisp` + `git-commit-file-on-branch` (`git.lisp`).
`open-dependency-fix-pr (alert-id)`: classify `fix_kind` (cached on the alert),
and for the unambiguous manifest case make a real branch+commit in the bare repo
(temp-worktree pattern) and open a PR via `create-pull-request`, stamping
`fix_pr_id`. Reachable via `cave-server deps-fix --alert <id>`.

**Decision:** a correct fix usually needs package-manager/lockfile tooling (for a
ranged dep the *resolved* version isn't even in the manifest), which is the moat
we borrow, not hand-roll. So the bump is `safe-bump-manifest` — it edits only
when the resolved version occurs **exactly once** in the manifest (covers exact
pins: go.mod, `requirements.txt ==`, pinned package.json). Anything ambiguous,
ranged, lockfile-only, or transitive is classified and returned `:manual`
**without touching files** — never a corrupting edit.

| `fix_kind` | situation | status today |
|---|---|---|
| `manifest` | direct, resolved version pinned in manifest | **opens PR** |
| `manifest` | direct, but ranged/absent in manifest | `:manual` (needs lockfile tooling) |
| `transitive_parent` | transitive dep | `:manual` |
| `none` | no fixed version | `:no-fix` |

**Speculative-build-gated fix PRs** — BUILT (`start-speculative-fix`,
`advance-speculative-fix-for-run`, `process-pending-fixes` in `deps-fix.lisp`;
`cave_dep_fix_attempts`). Dependabot-style: create the fix branch + commit, run
the repo's *pull_request* CI on that commit speculatively (push/release
workflows don't fire), and open the PR only once that build is green; a red build
records `build_failed` and opens nothing. Repos with no PR CI fall back to
opening the PR directly (`no_ci`). Auto-triggered after `sync-advisories` for
open, manifest-fixable security alerts, gated per-org by
`auto_fix_security` (default on). `safe-bump-manifest` prefers the qualified
`package@version` edit so shared tags (e.g. GitHub Actions `@v4`) disambiguate
and the v-prefix is preserved.

**CI-gated auto-merge** — BUILT (`process-dependency-automerge`,
`cave-server deps-automerge`; see deps-policy below): merges an eligible fix PR
(bump ≤ effective `automerge_ceiling`) once its head commit is green
(`combined-commit-status` = `:success`). Off by default (ceiling `none`).

Not yet: lockfile regeneration / override injection (needs a tool or runner);
historical pass-rate weighting from `cave_workflow_runs`; triggering a fix from a
dashboard checkbox (needs a web route).

## UI & notifications (reuse, don't reinvent)

- **Dependency-dashboard issue** — BUILT (`src/deps-dashboard.lisp`). One issue
  per repo, authored by a lazily-created `cave-bot` user, located idempotently by
  a hidden `<!-- cave-dependency-dashboard -->` marker in the body. Open alerts
  grouped by severity (critical→low), each with the OSV link and nearest fix
  version. Created on first alert, refreshed after every scan (`scan-repo-deps`)
  and advisory sync (`refresh-dependency-dashboards`); kept and shown as "no open
  alerts" once clean. TODO: per-update checkboxes to trigger a fix (needs the fix
  pipeline). Free markdown (cl-commonmark), permissions, notifications.
- **Repo Security tab** — BUILT (`view-dependencies`, `views.lisp`; route
  `GET /:owner/:repo-name/deps`). Open alerts severity-sorted (OSV links + fix
  versions) above the dependency graph. TODO: org Insights dashboard, inline
  manifest badges.
- **REST API** — BUILT (`server.lisp`): `GET …/deps`, `GET …/alerts`,
  `POST …/alerts/:id/dismiss` (auth + member-gated), `GET /api/v1/deps/usage`
  (org-wide, visibility-filtered).
- **CLI** — BUILT (`cli/cave/deps.go`): `cave deps list|alerts|dismiss|who-uses`.
- **Digest notifications** in `notify.lisp`: a 40-dep round as one email. TODO —
  needs careful new-vs-existing diffing to avoid spam; deferred.
- **Prometheus**: dependency freshness + open-alert counts in `metrics.lisp`. TODO.
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

1. Migrations 46–49 + `model.lisp` query layer. — DONE
2. `cave-server sync-advisories` (in-process, timer) — advisory DB populated. — DONE (`src/osv.lisp`)
3. `deps-scan` + `/-/internal/repos/.../deps` ingest — graph populated. — DONE (`src/sbom.lisp`)
4. Matcher (osv-scanner bootstrap) + alerts + dependency-dashboard issue. — DONE (`src/deps-dashboard.lisp`)
5. Native semver path → instant rematch-on-new-CVE. — DONE (`compare-versions`)
6. `deps-fix` pipeline + CI-gated auto-merge. — DONE: spine (`src/deps-fix.lisp`)
   + gated auto-merge (`process-dependency-automerge`, `cave-server deps-automerge`).
   Lockfile/override regeneration still TODO.
7. Org policy + per-repo `.cave/deps.yml` (org caps repo). — DONE
   (`src/deps-policy.lisp`; `cave_org_dep_policy`). `ignore` globs parsed but not
   yet enforced in matching — TODO.
8. REST API + `cave deps` client + repo Security tab. — DONE.
9. Remaining: lockfile/override fixes, org Insights dashboard + manifest badges,
   metrics, reachability (zoekt), digest notifications, `ignore`-glob enforcement,
   incremental advisory sync.

## Scheduling

The producers are CLI subcommands; run them periodically (cron or a systemd
timer — no timer unit ships yet, matching how `sync-mirrors`/`sync-themes` are
hook-driven today):
- `cave-server sync-advisories` — refresh advisories + rematch (e.g. daily).
- `cave-server deps-automerge` — merge eligible green fix PRs (e.g. every 30 min;
   a no-op until an org/repo raises its automerge ceiling).

`deps-scan` runs automatically on default-branch push; `cave-server deps-scan
--repo o/n` backfills.
