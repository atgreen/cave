# PRD: Cave — A Self-Hosted Code Forge

## Problem Statement

Small self-hosting teams choose between bloated forges and bare Git. The pain
points are drawn from public discussion (Duggan's "If I Could Make My Own GitHub,"
Forgejo/Gitea issue trackers, Hacker News threads on self-hosted dev tooling) and
direct experience running small-team infrastructure:

1. **Feedback arrives after the push.** Server-side checks run post-push. The
   developer has already context-switched. Git pre-receive hooks exist but no
   forge exposes them as a configurable, sandboxed feature.
2. **Review is binary.** Approve or reject. No way to say "land this, but flag
   the auth change for follow-up." Teams work around it with comments that
   get lost.
3. **Stacked changes require workarounds.** Every forge treats a branch as an
   atomic review unit. Dependent branches must be manually rebased and merged
   in order.
4. **Deployment is always separate.** Teams bolt on Drone, Woodpecker, or shell
   scripts. For container-native teams already using podman, the forge should
   handle build-and-deploy.
5. **Self-hosting is heavy.** GitLab needs 4GB+ RAM. Gitea is lighter but
   requires layering on CI, deploy, and notifications separately.

**Why these four pillars belong together:** The developer workflow is
push → validate → review → merge → deploy. Splitting it across 3–4 tools
creates integration gaps, duplicated auth, and operational burden that small
teams cannot afford. Cave's thesis is that a forge scoped to this pipeline —
and nothing else — is more valuable than a forge that does everything poorly.

## Target User (Launch Persona)

**Primary:** 3–10 developers on a Linux team currently running Gitea/Forgejo +
external deploy scripts (Woodpecker, Drone, or bash). They already use podman
for container workloads. They have one shared production host.

**Not launch target:** Solo hobbyists, teams needing SAML/SCIM/compliance,
teams needing GitHub Actions-style workflow graphs, teams deploying to
Kubernetes/Nomad/cloud PaaS.

## Goals

1. Single-binary forge: repositories, code review, issues, podman deploy
2. Pre-receive validation with sandboxed checks and fast feedback
3. Graduated review model with configurable merge policies
4. First-class stacked changesets
5. Comfortable on a 2-core / 2GB VPS

## Non-Goals

These are things a reasonable stakeholder will ask for in the first 3 months.
We say no.

- **Branch protection beyond merge policies.** No path-based rules, code owners,
  or required-review-from-specific-teams at launch.
- **Merge queues.** Only per-repo deploy queue (one at a time).
- **Hosted runners or remote executors.** Checks run on the Cave host only.
- **Container registry.** Images are local to the Cave host's podman storage.
- **Environment promotion.** One environment per repo. No staging/prod model.
- **Chat integrations.** No Slack/Matrix/IRC bots at launch.
- **Audit/compliance features.** No audit log, no RBAC beyond the defined roles.
- **Arbitrary CI/CD pipelines.** Cave runs pre-receive checks, deploys
  containers, and executes limited pre/post-deploy hooks. It does not
  support arbitrary workflow graphs, DAGs, or conditional steps.
- **Self-registration.** At launch, only instance admins create user
  accounts. Self-signup is P2.
- **Federation.** No federation-driven abstractions in MVP design.
- **GitHub/GitLab API compatibility.** Clean API; no compatibility shim.
- **Health-checked deploys.** Deploy is best-effort process replacement. No
  readiness probes, no zero-downtime rotation.
- **Git LFS.** Out of scope at launch. Large binary handling is unoptimized.
- **Tag-triggered operations.** Tags are browsable but do not trigger checks
  or deploys.

---

## Core Domain Model

### Repository
A bare Git repository hosted by Cave, owned by an org or user. Visibility is
public or private (configured by repo admin). Private repos and all their
metadata (issues, changesets, deploy status, build logs) are invisible to
non-members. Public repos: code, issues, changesets, and deploy status are
readable by anonymous users. Build logs for public repos are visible but
secrets are never printed (env var values are masked as `***` in logs).

### Changeset
The primary review unit. **A changeset is branch-backed.** One branch = at
most one changeset. A changeset is created when a developer pushes a branch
that is not the default branch and is not a tag, **and** the branch has a
review target. Not every branch becomes a changeset — developers can push
branches that are not associated with any changeset (e.g., personal backup
branches, experiment branches). A branch becomes a changeset when:
- It matches the stack naming convention (`stack/<name>/NN-slug`), OR
- The developer sets a target via push-option (`-o target=main`), OR
- The developer creates a changeset via the web UI targeting a branch.

Branches pushed without any of these signals are ordinary Git branches
with no changeset, no checks, and no review workflow.

**Target selection:** For stack branches, target is set automatically per
the stack rules below. For non-stack branches, target is specified by the
developer (push-option or UI). Default target if unspecified: none (no
changeset created). The target can be changed via UI or API before merge.

A changeset tracks:
- Source branch name (canonical identity — survives force-push)
- Target branch
- Current head commit (updated on force-push)
- Review state
- Version counter (incremented on each force-push)
- Associated stack (if any)

**State model:** Changeset state is **derived**, not persisted. There is no
stored "approved" or "blocked" field. The changeset is in one of these
display states, computed from its current data:

| Display state | Condition |
|--------------|-----------|
| **Open** | Source branch exists, target exists, not merged |
| **Mergeable** | Open + passes all merge eligibility rules |
| **Merged** | Landing completed (terminal) |
| **Closed** | Source branch deleted or manually closed (terminal) |
| **Orphaned** | Target branch deleted (must retarget or close) |

**Lifecycle:**

```
  push (new branch)
       │
       ▼
     open ──force-push──→ open (version++)
       │                     │
       │                     │
       ├──merge──→ merged (terminal; source branch auto-deleted by default)
       │
       ├──close (via UI/API or source branch deleted)──→ closed
       │
       └──target branch deleted──→ orphaned (cannot merge; must retarget or close)
```

- Branch name reuse after merge/close creates a new changeset (new `#N`).
- A raw Git branch rename (delete + create) closes the old changeset and
  creates a new one.
- Force-push while a merge is in progress: the merge operation holds a lock
  on the changeset. Force-push during lock is queued and applied after the
  merge completes or fails. If the merge succeeds, the force-push is rejected
  (branch was deleted). If the merge fails, the force-push proceeds normally.

**Identity:** `org/repo#N` (monotonic integer per repo). Issues and changesets
share the same number sequence — `#N` is globally unique within a repo. In
URLs, changesets use `/changesets/N` and issues use `/issues/N`. In markdown
and comments, `#N` links to whichever entity owns that number (rendered with
a type indicator).

### Stack
An ordered sequence of changesets where each depends on the previous.

**Creation:** A developer pushes multiple branches named `stack/<name>/NN-slug`
(e.g., `stack/auth-refactor/01-extract-middleware`, `stack/auth-refactor/02-add-jwt`).
Cave detects the naming convention, orders by `NN`, and links them into a stack.
Target assignment: the first branch targets the repo's default branch; each
subsequent branch targets the previous branch in the stack. Stack membership
is derived from naming at push time and persisted. Manual retargeting of a
stack changeset via UI/API removes it from the stack (with a warning).

**Stack item display states (derived, like changeset states):**

| Display state | Condition |
|--------------|-----------|
| **Open** | Changeset exists, reviews insufficient for merge eligibility |
| **Mergeable** | Changeset passes all merge eligibility rules |
| **Needs-rebase** | An ancestor in the stack was force-pushed; this changeset's base is stale |
| **Merge-conflict** | Rebase conflict detected during land attempt (land aborted) |
| **Merged** | Successfully landed (terminal) |
| **Closed** | Source branch deleted or removed from stack (terminal) |

```
  push (new stack branch)
       │
       ▼
     open ──reviews accumulate──→ mergeable
       │
       │ ancestor force-pushed
       ▼
  needs-rebase ──push (rebase)──→ open
       │ rebase conflict during land
       ▼
  merge-conflict (land aborted, manual resolution required)

  mergeable ──land──→ merged
  merged (terminal)
  closed (source branch deleted, or removed from stack)
```

**Landing algorithm (step-by-step):**

Stack landing **flattens the stack onto the base branch** (the first changeset's
target, e.g., `main`). The intermediate target branches between stack items
exist for review isolation only — they are not the merge destination.

1. Verify all changesets in the stack pass merge eligibility (derived state
   = mergeable for each).
2. Lock the stack (no concurrent pushes or landings).
3. Let `base` = current HEAD of the stack's base branch (e.g., `main`).
4. For each changeset in order (01, 02, ...):
   a. Rebase the changeset's commits onto `base`.
   b. If rebase conflict: abort entire landing. Report the conflicting
      changeset and files. Release lock. No changesets are merged.
   c. Create a merge commit on the base branch.
   d. Update `base` = new base branch HEAD.
5. Delete all stack source branches (if auto-delete enabled).
6. Release lock.
7. If the base branch is the deploy branch, enqueue a deploy.

**Stack splitting:** Deleting a branch in the middle of a stack closes that
changeset. Descendants are retargeted: the changeset immediately after the
deleted one is retargeted to the deleted changeset's target. All descendants
are marked `needs-rebase`.

### Review
A review is an assessment of a changeset version by an authenticated user.

| State | Meaning | Merge effect |
|-------|---------|-------------|
| **Approve** | Ship it | Counts toward required approvals |
| **Approve-with-concerns** | Ship it, but concerns tracked | Counts toward approvals (configurable). Creates one or more **Concern** entities. |
| **Request changes** | Do not ship | Blocks merge. Resolved by new push (review becomes stale) or reviewer withdrawing. |
| **Comment** | Observation only | No merge effect |

### Concern
A concern is a first-class entity attached to a review. Fields: text, author
(the reviewer), status (open / resolved), resolved-by, resolved-at.

- Created when a reviewer submits an "approve-with-concerns" review.
- Resolved by the original reviewer via a "resolve" action. If the reviewer's
  account is deactivated or loses repo access, any repo admin can resolve.
- A new push (version increment) does NOT auto-resolve concerns.
- Concerns are displayed on the changeset page.
- The merge policy `require_zero_unresolved_concerns` (default false) can
  block merge on unresolved concerns.

### Review staleness

On force-push (version increment), all reviews on prior versions are marked
**stale**. Staleness affects each review state:

| Review state | When stale | Effect |
|-------------|-----------|--------|
| Approve | Does not count toward approvals (unless `allow_stale_approvals`) |
| Approve-with-concerns | Does not count; concerns remain open regardless |
| Request changes | Does not block merge (treated as withdrawn) |
| Comment | Display only (no change) |

### Merge eligibility algorithm

A changeset is mergeable when ALL of the following are true (evaluated in order):

1. Changeset display state is `Open` (not `Closed`, `Orphaned`, or `Merged`).
2. Target branch exists and is reachable.
3. No concurrent merge/land lock is held on this changeset.
4. Number of non-stale approvals (including approve-with-concerns if
   `concerns_count_as_approval` is true) >= `required_approvals`.
5. If `allow_self_approval` is false: self-approvals are excluded from the count.
6. If `block_on_request_changes` is true: no non-stale request-changes reviews exist.
7. If `require_zero_unresolved_concerns` is true: no open concerns exist.
8. If `required_checks_pass` is true: all configured checks passed on the
   current version.
9. If the changeset is part of a stack: **whole-stack landing only**. All
   changesets in the stack must independently satisfy rules 1–8. Individual
   or prefix-merge of stack members is not supported — the stack lands as
   a unit or not at all.

The UI displays each rule with pass/fail status and the specific blocker.

### Issue
Lightweight tracker scoped to a repository. Issue numbers share the repo-wide
`#N` sequence with changesets.

Fields: title, body (markdown), status (open/closed), labels (free-form strings),
assignee(s), author, timestamps.

**`Closes #N` parsing:** Scanned in ALL commits that are newly reachable on the
target branch after a changeset lands (i.e., the rebased commits plus the merge
commit). Only the literal patterns `Closes #N`, `Fixes #N`, `Resolves #N`
(case-insensitive) are recognized. If `#N` refers to an issue, it is closed.
If it refers to a changeset or doesn't exist, no-op. In stacked landings, each
changeset's newly-reachable commits are scanned after its merge commit is created.

### Merge Strategy
At launch: **rebase-and-merge** only. Source branch is rebased onto target,
then a merge commit is created. Commit messages are not edited at merge time.
Not configurable at launch.

---

## Branch Push Policy

| Ref type | Who can push | Checks run? | Notes |
|----------|-------------|-------------|-------|
| Default branch (e.g., `main`) | **No direct pushes.** Changes must flow through changesets. Repo admins can override with `allow_direct_push_to_default` (default false). | N/A (changesets enforce checks) | Protects the deploy trigger. |
| Changeset branches (with target set via push-option or UI) | Writers and above | Yes (pre-receive) | Creates/updates changeset |
| Stack branches (`stack/<name>/NN-slug`) | Writers and above | Yes (pre-receive) | Creates/updates stack changeset |
| Ordinary branches (no changeset) | Writers and above | No | No review workflow. Useful for experiments, backups. |
| Tags | Repo admins only | No | Create and delete only; no tag update (force) |
| Branch delete | Changeset author (the user who first pushed the branch) or repo admin | No | Closes associated changeset if one exists |

**Multi-ref push:** If a push updates multiple refs and any ref fails
(check failure or permission denial), the entire push is rejected.

---

## User Roles, Membership, and Permissions

### Membership model
- An **org** has members. Each member has an org-level role (admin or member).
- A **repo** has members. Each member has a repo-level role (writer, reviewer,
  or admin). Org admins are implicit repo admins on all org repos.
- Org members are NOT automatically repo members. Repo access must be granted
  explicitly (per-repo or via future team abstraction).
- Authenticated users who are not repo members can only access public repos.

### Permission matrix

| Role | Repos | Changesets | Issues | Deploy | Admin |
|------|-------|------------|--------|--------|-------|
| **Anonymous** | Read public | Read public | Read public | View public deploy status | — |
| **Authenticated (non-member)** | Read + clone public | Comment on public | Comment on public | — | — |
| **Writer** | Push branches, clone | Create + update own changesets | Create, assign, close | — | — |
| **Reviewer** | Writer + approve/reject any changeset | All writer + review | All writer | — | — |
| **Repo admin** | Manage settings, merge rules, deploy config, members | All + merge | Manage labels | Trigger deploy, rollback, edit secrets | — |
| **Org admin** | Create/delete repos, manage org members | All | All | All | Manage org settings |
| **Instance admin** | All | All | All | All | Server config, user management |

### Membership management stories (P0)

- As an org admin, I want to invite users to my org and assign org roles.
  - Acceptance: Invite by username. Invited user must accept. Org admin can
    set role to admin or member. Org admin can remove members.

- As a repo admin, I want to add org members to my repo and assign repo roles.
  - Acceptance: Add by username (must be org member). Set role to writer,
    reviewer, or admin. Repo admin can remove members and change roles.

- As a user, I want to generate and revoke API tokens.
  - Acceptance: Create token via web UI (named, no expiry at launch). Token
    displayed once at creation. Revoke via UI. Multiple tokens per user.

### Visibility rules

| Entity | Public repo | Private repo |
|--------|------------|-------------|
| Repo name / existence | Visible to all | Visible to members only |
| Code / branches / tags | Readable by all | Members only |
| Changesets | Readable by all | Members only |
| Issues (titles, body, comments) | Readable by all | Members only |
| Deploy status | Visible to all | Members only |
| Build logs | Visible to all (secrets masked) | Members only (secrets masked) |
| User profiles (username, key fingerprints) | Public | Public |
| Org name / existence | Visible to all | Visible to all |
| Org member list | Visible to org members | Visible to org members |
| Private repo names | — | Not enumerable to non-members |

---

## Auth and SSH

**Launch architecture: Cave-managed SSH.** Cave runs its own SSH server
(configurable port, default 2222) for Git transport. System sshd is not
used for Git operations.

**SSH URL format:** `ssh://cave@host:2222/org/repo.git` (standard SSH URL
syntax). SCP-like syntax (`cave@host:org/repo.git`) is NOT supported at
launch due to the non-standard port.

- **Web login:** Username + password (bcrypt-hashed).
- **API auth:** Bearer token in `Authorization` header. Same token works for
  HTTP clone of private repos (via query param `?token=` or header).
- **SSH auth:** Public key. Users register keys via web UI.
- **Key lifecycle:** Add via UI, revoke via UI, multiple keys per user,
  no expiry at launch. Duplicate public keys across users are rejected.
- **Repo authorization:** On SSH push/clone, Cave resolves the SSH key to a
  user, then checks membership and role. Unauthorized access to private repos
  returns a generic "repository not found" (no information leak).
- **HTTP clone:** Public repos are cloneable via HTTP without auth. Private
  repos require API token. HTTP push is not supported (SSH only).

---

## User Stories

### P0 (Must have for launch)

**Git hosting**

- As a developer, I want to push code to Cave over SSH.
  - Acceptance: `git clone ssh://cave@host:2222/org/repo.git` and `git push`
    work for authorized users. Push to non-existent repo returns a generic
    error. Push with unregistered key is rejected.

**Pre-receive validation**

- As a developer, I want the server to run checks on my push and reject it
  with output if checks fail.
  - **Execution model:** For each updated ref that creates or updates a
    changeset (identified by push-option target or stack naming convention),
    Cave creates a temporary worktree at the synthetic merge of the pushed
    head onto the current target branch HEAD. Checks run in that worktree.
    Non-changeset pushes (ordinary branches, tag pushes, branch deletes)
    do not trigger checks.
  - **Config:** Server-side per-repo config (set by repo admin via UI) takes
    precedence. If no server-side checks are configured AND the repo admin
    has enabled `allow_repo_checks`, then `.cave/checks.toml` in the pushed
    commit is used. Server-side config replaces (not merges with) repo config.
    Malformed `.cave/checks.toml` is treated as no checks configured (with
    a warning in push output).
  - **Sandbox:** Writable `/tmp` (tmpfs, 64MB). Read-only worktree. No network.
    No access to other repos or Cave internals. Configurable timeout (default
    60s), memory limit (default 512MB). Limits enforced via cgroups v2 if
    available; ulimit fallback with a warning in admin UI that enforcement is
    degraded. CWD is the worktree root.
  - **Inherited env:** `PATH`, `HOME` (set to temp dir), `LANG`. All other
    env vars stripped.
  - **Multi-ref push:** Entire push rejected if any ref fails.
  - **Output:** Buffered (max 64KB), returned to developer. Truncated with
    notice if exceeded. Timeout: "check timed out after Ns."

**Code review**

- As a reviewer, I want graduated approval states.
  - Acceptance: Four states in web UI. Reviews record reviewer, state,
    changeset version, body text. Stale reviews visually distinguished.
    Approve-with-concerns creates Concern entities displayed on changeset page.

- As a team lead, I want configurable merge rules per repo.
  - Acceptance: Rules via repo admin UI:
    - `required_approvals` (integer, default 1)
    - `allow_stale_approvals` (boolean, default false)
    - `allow_self_approval` (boolean, default false)
    - `concerns_count_as_approval` (boolean, default true)
    - `require_zero_unresolved_concerns` (boolean, default false)
    - `block_on_request_changes` (boolean, default true)
    - `required_checks_pass` (boolean, default true)
    - `allow_direct_push_to_default` (boolean, default false)
  - UI shows merge eligibility with per-rule pass/fail and specific blocker.

**Stacked changesets**

- As a developer, I want to submit and land stacked changesets.
  - Acceptance: Branches matching `stack/<name>/NN-slug` are linked into a
    stack. UI shows stack as ordered list with per-changeset review status
    and state. Landing follows the step-by-step algorithm defined in the
    domain model. Failure identifies the blocking changeset and conflicting
    files. No partial landing.

**Issues**

- As a developer, I want to file and browse issues.
  - Acceptance: CRUD via web UI. Filtering by status, label, assignee.
    `Closes #N` / `Fixes #N` / `Resolves #N` close issues on changeset
    landing. Issue numbers are repo-scoped (shared sequence with changesets).
  - **REST API (P0 scope):**
    - `GET /api/v1/repos/:org/:repo/issues` — list with filter params
    - `POST /api/v1/repos/:org/:repo/issues` — create
    - `GET /api/v1/repos/:org/:repo/issues/:id` — read
    - `PATCH /api/v1/repos/:org/:repo/issues/:id` — update
    - Auth: API token required for write; public repos readable without auth.

**Podman deployment**

- As an operator, I want to deploy a repo's container to the local podman
  runtime.
  - **Scope at launch:** Single container per repo. Best-effort process
    replacement (not zero-downtime). No readiness probes. No restart policy
    managed by Cave (operator uses systemd/quadlet for process supervision).
  - **Config split — two sources of truth:**

    **Repo file** (`.cave/deploy.toml`, versioned, code-reviewed):
    ```toml
    [deploy]
    containerfile = "Containerfile"   # path relative to repo root
    volumes = ["data:/app/data"]      # named volumes (name:mountpoint)
    ```

    **Admin UI / DB** (set by repo admin, not in repo):
    - `deploy_branch` (default "main") — branch that triggers auto-deploy
    - `container_port` (integer) — port inside the container
    - `host_port` (integer) — port on host; conflicts validated at save
    - `build_timeout` (seconds, default 600)
    - `deploy_enabled` (boolean, default false)

    Rationale: repo file controls what developers can change through code
    review (build structure, volume mounts). Admin UI controls operational
    settings that affect the host (ports, timeouts, activation). This
    prevents developers from changing host port mappings via a commit.

  - **Secrets:** Stored in DB (AES-256-GCM, key from `cave.conf`). Editable
    by repo admin via UI only. Write-only: secrets cannot be read back after
    creation (only overwritten or deleted). Values masked as `***` in build
    logs and deploy logs. Written to temp env-file, deleted after `podman run`.
    Multiline values supported. Empty string is a valid value; unset deletes.
  - **Build:** `podman build -t cave/<org>/<repo>:<git-short-sha>` from the
    merged commit on the target branch. Build log streamed to UI. Timeout
    per config.
  - **Deploy sequence:**
    1. Build image. On failure: stop, current deployment untouched, status
       "build failed."
    2. `podman stop <name>` (timeout 30s). If container doesn't exist, skip.
    3. `podman rm <name>`. If doesn't exist, skip.
    4. `podman run -d --name cave-<org>-<repo> -p <host_port>:<container_port>
       --env-file <tmpfile> -v <volumes> <image>`.
    5. If run fails: attempt to restart previous image
       (`cave/<org>/<repo>:<prev-sha>`). If previous image not available,
       status "deploy failed, no rollback available." Service is down.
    6. Clean up temp env-file.
    7. Record deploy result (status, image tag, timestamp, commit SHA,
       previous image tag) in DB.
  - **Rollback:** Re-deploys the image tag from the last successful deploy
    record. Uses current env vars (not historical). Does not re-run hooks.
    If previous image was pruned: "rollback failed, image not found."
  - **Triggers:** Merge to deploy branch (auto) or manual "Deploy" button.
    Both use the same deploy sequence. Deploy source: HEAD of deploy branch
    at time of trigger.
  - **Queue:** Each deploy trigger creates a job with a specific commit SHA.
    Jobs are FIFO. No coalescing — each SHA is deployed in order. Max queue
    depth 3; excess rejected with "deploy queue full." One job runs at a time.
  - **Env var change:** Does not trigger rebuild or restart. Operator clicks
    "Restart" to stop/start with current image and new env vars.
  - **Port conflicts:** Validated when saving deploy config. If another repo
    claims the same host port, save is rejected.

**Org and repo management**

- As an org admin, I want to create repos, invite members, and manage roles.
  - Acceptance: Per the membership management stories above.

- As an operator, I want to manage users at the instance level.
  - Acceptance: Instance admin can create users, deactivate users, reset
    passwords, and view all orgs. **No self-registration at launch** — all
    accounts are created by instance admins. Deactivated users cannot log in,
    push, or use API tokens. Existing sessions are invalidated.

**Setup and operations**

- As an operator, I want minimal setup.
  - Acceptance: `cave init --admin-user <name> --admin-password <pass>
    --admin-ssh-key <path> --config cave.conf` bootstraps non-interactively.
    Separate command from `cave serve`. `cave init` is idempotent for schema
    creation (safe to re-run). Admin credential update requires explicit
    `cave admin reset-password` (not part of init).
  - `cave serve --config cave.conf` starts the forge.
  - Host dependencies (documented, not bundled): Git 2.30+, podman 4.0+,
    PostgreSQL 14+. Cave does not manage their lifecycle.

**Backup and restore**

- As an operator, I want to back up and restore my forge.
  - Acceptance: `cave backup --output <path>` puts the server in read-only
    mode, takes a PostgreSQL dump and tarballs all bare Git repos, then
    resumes normal operation. `cave restore --input <path>` requires the
    server to be stopped. Secrets in the dump are encrypted; restore fails
    with a clear error if `cave.conf` has a different encryption key.
    Deploy state (running containers) is NOT backed up.
  - **Upgrade path:** `cave migrate --config cave.conf` runs pending DB
    migrations. Server refuses to start if schema version doesn't match
    binary version (clear error message with instruction to run migrate).

### P1 (Should have)

- **Web code browser:** File tree, commit log, diff view. Binary files show
  size only. Submodules show as links. Symlinks show target path.
- **CLI review:** Requires changeset/review API (defined in P0 as REST
  endpoints alongside issues). `cave review <changeset-id> --approve`.
- **Deploy hooks:** Pre-deploy (runs after build, before stop) and post-deploy
  (runs after successful run). Shell commands in `.cave/deploy.toml`.
  CWD = repo worktree. Timeout 60s. Pre-deploy failure aborts (current
  deployment untouched). Post-deploy failure logged, no rollback.
- **Email notifications:** SMTP config in `cave.conf`. Per-repo preferences
  (all / mentions / none). Events: review requested, review submitted,
  changeset merged, issue assigned. No dedup across force-push cycles.
- **Branch lifecycle config:** Auto-delete source branch on merge (default
  true, configurable per-repo).
- **Changeset/review REST API:** Same pattern as issues API. Needed for CLI
  review and third-party integrations.
- **Repo deletion and archival:** Repo admin can archive (read-only, no new
  pushes/changesets). Org admin can delete (removes repo, issues, changesets,
  deploy config; stops running container).

### P2 (Nice to have)

- Email patch intake (narrowly scoped: `git format-patch` only, max 1MB,
  public repos only, attributed to "email:<address>").
- Mirror repos to/from GitHub, GitLab, Codeberg.
- Syntax-highlighted code/diff viewing.
- OIDC/OAuth login.
- Import repos + issues from Gitea/Forgejo (migration path).

---

## Technical Constraints

- SBCL on Linux (x86_64, aarch64)
- Installable via ocicl
- Single dumped-image binary
- Hunchentoot for HTTP
- Podman (rootless) for container operations
- Cave-managed SSH server for Git transport (port 2222 default)
- PostgreSQL 14+ (no SQLite)
- Config in `cave.conf` (s-expression format)
- Git operations via `git` CLI (shell out)

---

## Open Questions

1. **Stack naming convention vs explicit API:** `stack/<name>/NN-slug` is
   discoverable but fragile. Should we also support `cave stack create` CLI
   or push-options?
2. **Cgroup v2 requirement:** Hard-require for launch, or accept degraded
   ulimit-only mode with admin UI warning?
3. **Database migrations:** CL migration library or hand-rolled numbered
   SQL files?

---

## Success Metrics

### Benchmark fixtures
- 50 repos, 10k commits each, 5k files per repo
- 200 open changesets, 500 open issues across all repos
- 10 concurrent users, mixed read/write workload
- Deploy: single-stage Containerfile, ~200MB image

### Operational
- **Time to first push:** <10 min from binary download on a VPS with PG, Git,
  and podman pre-installed. Measured: wall clock from `wget cave` to successful
  `git push`.
- **Page load:** p95 server response time (HTTP response complete, not browser
  render) <500ms under benchmark load.
- **Deploy latency:** <90s from merge to `podman run` exit under benchmark
  image size.
- **Memory:** <512MB RSS under benchmark load.
- **Push rejection latency:** p95 <10s for checks with 60s timeout configured
  (measures Cave overhead, not check runtime).

### Workflow (after 30 days of team use)
- **Push rejection utility:** Median time from push rejection to successful
  re-push <5 minutes.
- **Review expressiveness:** >20% of reviews use a state other than plain
  approve.
- **Stack usage:** >15% of changesets are part of a stack.
- **Deploy integration:** >80% of repos with a Containerfile use Cave deploy.
- **Merge turnaround:** Median time from changeset creation to merge for
  stacked vs non-stacked (stacked should not be slower).
- **Deploy recovery:** Median time from failed deploy to successful re-deploy
  <15 minutes.

### Instrumentation
Cave logs these events to a `cave_events` table for metric computation:
- `push.accepted`, `push.rejected` (with check name, duration)
- `changeset.created`, `changeset.merged`, `changeset.closed`
- `review.submitted` (with state)
- `concern.created`, `concern.resolved`
- `stack.created`, `stack.landed`, `stack.failed`
- `deploy.started`, `deploy.succeeded`, `deploy.failed`, `deploy.rolledback`
- `issue.created`, `issue.closed`

Each event: timestamp, user ID, repo ID, and relevant entity IDs. Events
retained indefinitely at launch (no rotation policy). Queryable by instance
admin via `cave metrics` CLI command.

### Adoption (6-month horizon)
- 3 teams outside core developers using Cave as primary forge.
