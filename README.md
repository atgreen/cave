# Cave

A self-hosted code forge written in Common Lisp. Push code, review changes, merge with confidence.

Cave is built for small teams who want to own their infrastructure without the bloat of enterprise forges. It's fast, opinionated, and runs on a single server.

## Features

The surface is split by maturity. **Implemented** features are in everyday use;
**Experimental** ones work but have known gaps or sharp edges; **Planned** ones
are not built yet.

### Implemented

- **Git hosting** — SSH push/clone with per-repo access control, plus anonymous read-only clone over HTTPS; pushing a new branch prints an "open a pull request" link back in the terminal
- **Deploy keys** — per-repo SSH keys for CI/automation without a user account; read-only by default, opt-in write. Managed in repo Settings; `authorized_keys` is regenerated automatically so access takes effect immediately
- **Protected branches** — per-repo rules (exact name, `prefix/*` glob, or `*`) that block direct pushes (forcing changes through pull requests; repo admins may override) and/or require signed commits. Enforced at push time in the pre-receive hook, before any check runs
- **Pull requests** — graduated review model (approve, approve with concerns, request changes); enforced merge-eligibility rules: required approvals, blocking change-requests, unresolved concerns, and required status checks (failing/pending checks block the merge); squash merge. Draft PRs, close/reopen (UI/API/CLI), and auto-merge (arms the PR to merge automatically once review + checks pass). Review rounds are recorded per re-push, with interdiff (range-diff) links to see what changed between rounds. CODEOWNERS auto-requests and notifies owners of touched paths
- **Live checks panel** — pull requests show a GitHub-style checks panel combining external commit statuses and cave workflow jobs: a rollup summary (N failing / M in progress / K successful) plus per-check rows with green/red icons and spinners for in-progress checks, polled live while anything runs (the merge box refreshes once they settle)
- **Issues** — create, list, comment, close/reopen; labels with label filtering, assignees, and milestones (with open/closed issue counts)
- **Notifications** — in-app notification feed with an unread-count bell, plus per-repo watching (members are subscribed automatically; anyone can Watch); new issues, comments, PR reviews, and merges notify members + watchers. Email notifications too
- **Releases** — tag-backed releases with markdown body and per-asset uploads (≤ 100 MB each), download counts; release notes auto-generated from commits since the previous tag when the body is left blank
- **Commit status API** — external CI reports pass/fail per context on a commit; feeds merge eligibility and the checks panel
- **CI secrets** — per-repo encrypted secrets (AES-256-CBC at rest) injected into workflow jobs as environment variables and masked in job logs; managed in repo Settings
- **Supply-chain security** — CycloneDX SBOM ingest on push, dependency graph, OSV advisory-feed matching, severity-ranked dependency alerts with durable suppressions, and Dependabot-style auto-fix pull requests (speculative build before opening)
- **Commit signature verification** — SSH- and GPG-signed commits validated against keys registered in Settings (SSH keys / GPG keys), verified on push with a "Verified" badge in the commit list; a `cave-server reverify` command backfills verification for existing history after a key is added
- **Git subprocess sandboxing** — `git` push/clone, repo metadata reads, and pre-receive checks run inside a Landlock filesystem sandbox (via [landrun](https://github.com/Zouuup/landrun)) scoped to the target repo: a compromised git or check process can touch only its own repo plus read-only system paths — not other repos or host secrets (kernel-enforced cross-repo isolation). Gated by `:sandbox-landlock` (on by default); degrades to a no-op where the kernel lacks Landlock
- **Code browser** — file tree with per-file last commit and relative age, repo language breakdown, file-type icons, Monaco editor, syntax-highlighted diffs, branch/tag switcher (overview, code, tree, and file views), SSH/HTTPS clone widget, click-a-line-number permalinks (`#L42`) with a per-line action menu
- **README & markdown rendering** — server-side Markdown (CommonMark + GFM tables, via cl-commonmark) with fenced code blocks, rendered HTML cached by blob sha; math (KaTeX) and Mermaid diagrams rendered client-side, loaded lazily only when the syntax is present
- **Code search** — full-text code search powered by [Zoekt](https://github.com/sourcegraph/zoekt), repo-scoped and global
- **Repo mirroring** — push to and pull from GitHub/GitLab/Codeberg, with scheduled sync
- **Webhooks** — HTTP callbacks on push, PR, and issue events
- **Pulse tab** — per-repo activity chart, total views, unique visitors, top contributors, referring sites (member-only)
- **Public landing** — anonymous visitors get a landing page driven by the `cave-landing` system repo (edit content with a git push), with a list of public repos and recent activity, no login required
- **Explore** — discover public repos with search, sort, and trending; browse people; facet by language with colored language dots (`/-/explore`)
- **User themes** — built-in (Terminal Warmth, Solarized Dark, Nord, Dracula, Light) plus custom themes via `cave-themes` repos
- **Built-in identity (Usher)** — cave embeds its own OpenID Provider (Usher) in-process, so no external IdP is required: OIDC login, self-registration gated by admin approval, email verification, password reset, self-service password change, and 2FA (TOTP) with backup codes. SSH and GPG key management live in Settings. cave still speaks plain OIDC, so it can also federate to an external provider (e.g. Keycloak)
- **Email** — SMTP via mailpit (dev) or any external relay (Resend / Postmark / SES / Fastmail …) configured per deploy
- **Observability** — Prometheus metrics, Grafana dashboards, SBCL runtime stats
- **Backup/restore** — one-command backup and restore of Postgres + repos + config
- **Declarative deployment** — `cavectl` reconciles containers from a single `cave.yaml`; `cavectl doctor` runs end-to-end health checks
- **Quadlet deployment** — systemd user services for production, with rollback

### Experimental

- **Stacked changesets** — dependent PRs are tracked and displayed as a stack, but there is no atomic "land stack" yet; members still merge one PR at a time (see *Planned*)
- **Automation runners + workflows** — `.cave/workflows/*.yml` jobs are scheduled across self-hosted gRPC runners and report status back, with a partial **GitHub-Actions-compatible** syntax: `push`/`pull_request`/`tag` triggers, workflow/job/step `env:`, multi-line `run: |` blocks, the standard `GITHUB_*` env (each with a `CAVE_*` twin) plus `RUNNER_*`/`CI`, and the `$GITHUB_OUTPUT`/`$GITHUB_ENV`/`$GITHUB_PATH`/`$GITHUB_STEP_SUMMARY` file-command protocol, `${{ }}` expressions (in `run:`/`env:`/`if:`, across the `github`/`steps`/`matrix`/`needs`/… contexts), `strategy.matrix`, job `outputs:`/`needs.*`, and `uses:` actions (GitHub model: empty workspace + cave-local, Lisp-native built-ins like `actions/checkout`). Admin policy gates repo-supplied jobs: `privileged` is denied by default and images can be pinned to an allowlist (`:workflows-allow-privileged`, `:workflows-image-allowlist`); job dependencies without an explicit image resolve to a [Nixery](https://nixery.dev) image. Encrypted per-repo secrets are injected as env vars and masked in logs (as is anything a step emits via `::add-mask::`). A reaper requeues jobs orphaned by a dead/restarted runner (bounded retries) so the queue self-heals. Still missing for a fully untrusted multi-tenant setup: per-repo policy overrides and stronger runner-side isolation — so prefer trusted repos
- **Multi-chamber storage** — Praefect-style routing across git storage nodes (read/write split, health checks, async replication). Opt-in; single-chamber is the default and the well-exercised path

### Planned

- **Atomic stack landing** — ordered validation plus all-or-nothing merge of a stack
- **`Closes #N` auto-close** — close issues automatically from commit/PR messages (labels, assignees, milestones, and filtering are now *Implemented*)
- **Deployment auth migration** — the cave binary embeds Usher, but the `cavectl` tooling still provisions an external Keycloak (`auth.mode: keycloak`); wiring `cavectl` to deploy the embedded provider directly is pending
- **Fully sandboxed checks** — pre-receive checks already run under a Landlock filesystem sandbox (cross-repo isolation, scoped to the extracted worktree) with a scrubbed environment, network isolation (`unshare -n` plus Landlock TCP-deny, best-effort where the container can't `unshare`), and a wall-clock timeout (`:checks-*`). Still missing for full isolation: cgroup memory/CPU limits and dropping to a per-repo unprivileged UID
- **Repo deployment / CD** — build images, queue deploys, roll back, manage secrets
- **Unit/integration tests** — for migrations, the REST API, and merge-policy rules (today only the end-to-end Playwright suite exists)

## Quick start

```bash
# Build cave-server + cave (CLI) + cavectl
make build

# Spin up the full stack locally (postgres, keycloak with cave theme + realm,
# mailpit, zoekt, cave, runner) — picks free ports, generates secrets
./cavectl init

# Visit cave at http://localhost:9080
```

`cavectl init` writes a `cave.yaml` with every secret already filled in. Edit it
to change images, ports, or auth mode, then `cavectl apply --yes` to reconcile.

## Production deployment

Two paths. Pick one.

### Path A — `cavectl` (recommended for new installs)

On the target host (Fedora/Rocky/Alma/Debian/Ubuntu, with `podman` or `docker`):

```bash
# 1. Pull cavectl from a release artifact, or build from source:
go build -o cavectl ./cli/cavectl

# 2. Generate cave.yaml. init also brings the stack up after it prints
#    the plan; answer "no" at the prompt if you want to edit cave.yaml
#    before anything is created.
./cavectl init --name cave

# 3. Edit cave.yaml: set base_url to https://cave.example.com,
#    auth.mode to "keycloak", auth.keycloak.public_url to
#    https://auth.cave.example.com, smtp.mode to "external" with your
#    relay credentials, and ports.ssh to 22 if cave's SSH should be on
#    the public port (then move system sshd off 22 first).
#    Then reconcile the edits:
./cavectl apply --yes

# 4. Health-check
./cavectl doctor
```

Front it with Caddy:

```caddyfile
cave.example.com         { reverse_proxy 127.0.0.1:9080 }
auth.cave.example.com    { reverse_proxy 127.0.0.1:9180 }
runner.cave.example.com  { reverse_proxy h2c://127.0.0.1:9443 }
```

The `runner.` block is what lets remote automation runners reach
cave's gRPC service through TLS — point runners at
`grpcs://runner.cave.example.com`. `h2c://` tells Caddy to speak
plaintext HTTP/2 to the upstream (gRPC).

### Path B — Make + systemd quadlets

```bash
make build && make release   # build images, tag :prod
make prod-install            # drop quadlet units in ~/.config/containers/systemd/
make prod-start
```

Default ports: cave 9080, keycloak 9180, mailpit 9025, SSH 9222.

### Rollback / backup

```bash
make prod-rollback                                # swap back to cave:prod-previous
make prod-backup                                  # → ~/cave-backups/cave-YYYY-MM-DD.tar.gz
make prod-restore F=path/to/archive.tar.gz
```

## Container images

Published from `main` and from tagged releases via the `Publish container images`
GitHub Actions workflow:

| Image                              | Source                       |
|------------------------------------|------------------------------|
| `ghcr.io/atgreen/cave`             | `Containerfile.local`        |
| `ghcr.io/atgreen/cave-runner`      | `Containerfile.runner`       |
| `ghcr.io/atgreen/cave-zoekt`       | `Containerfile.zoekt`        |
| `ghcr.io/atgreen/cave-keycloak`    | `Containerfile.keycloak`     |

Tags: `:sha-<short>` (immutable), `:main` (rolling), and on `v*` tags
`:<version>`, `:prod`, `:latest`.

## Architecture

Cave is a single Common Lisp (SBCL) binary serving HTTP via Hunchentoot, with a
gRPC service for runners (`runner-service.lisp`) and a gRPC service for git
storage (`chamber.lisp`). All HTML is generated by Spinneret. CSS is a single
file. No JavaScript framework.

```
src/
├── package.lisp        — Package definition
├── config.lisp         — S-expression config parser
├── db.lisp             — PostgreSQL via Postmodern, numbered migrations
├── auth.lisp           — Embedded Usher OpenID Provider, sessions, API tokens,
│                         GPG/SSH key mgmt, notifications, sudo mode
├── model.lisp          — Domain queries: users, orgs, repos, issues, PRs,
│                         reviews, releases, signatures, page views, …
├── git.lisp            — Git CLI integration (branch, log, tree, diff, merge,
│                         tag, signature verification, trailers)
├── views.lisp          — All HTML views via Spinneret
├── notify.lisp         — Email notifications, webhooks
├── search-zoekt.lisp   — Zoekt code search: indexing, API client, visibility
├── metrics.lisp        — Prometheus metrics endpoint
├── runner-service.lisp — gRPC service for automation runners
├── workflow.lisp       — Workflow orchestration: parse, schedule, deps
├── yaml.lisp           — Minimal YAML parser for workflows
├── chamber.lisp        — Git storage gRPC service
├── chamber-client.lisp — Chamber client with chamber-or graceful fallback
├── chamber-router.lisp — Multi-chamber routing (Praefect-style)
├── ssh.lisp            — SSH transport, authorized_keys generation
├── server.lisp         — HTTP routes and request handling
└── main.lisp           — CLI subcommands (serve, init, migrate, runner, etc.)

cli/cavectl/            — Go deployment tool source
internal/cavectl/       — Go libraries: config, plan, apply, runtime, doctor, …
keycloak/themes/cave/   — Custom Keycloak login theme
keycloak/cave-realm.json — Realm import (placeholders substituted at runtime)
deploy/quadlet/         — systemd-user quadlet units
```

### Key dependencies

| Library | Purpose |
|---------|---------|
| Hunchentoot + easy-routes | HTTP server and routing |
| Postmodern | PostgreSQL client |
| Spinneret | S-expression HTML generation |
| ag-grpc | gRPC server + client (runners, chamber) |
| Usher | Embedded OpenID Provider (in-process auth) |
| cl-commonmark | Markdown rendering (CommonMark + GFM tables) |
| Dexador | HTTP client (OIDC, webhooks) |
| Ironclad + jzon + flexi-streams | crypto, JSON, byte streams |

## Configuration

Two configs live side-by-side:

1. **`cave.conf`** — runtime config consumed by the `cave` binary (rendered by
   `cavectl` or `entrypoint.sh` from environment variables). S-expression plist:

   ```lisp
   (:http-port 8080
    :grpc-port 9443
    :ssh-port 22
    :data-dir "/var/lib/cave"
    :db-host "cave-pg"
    :db-name "cave"
    :db-user "cave"
    :db-password "…"
    :base-url "https://cave.example.com"
    ;; OIDC issuer. For the embedded Usher provider, set this to your base URL —
    ;; cave self-serves OIDC, no external IdP needed. For an external provider
    ;; (e.g. Keycloak) point it at the realm and set :oidc-issuer-internal to the
    ;; in-cluster URL.
    :oidc-issuer "https://cave.example.com"
    :oidc-client-id "cave"
    :oidc-client-secret "…"
    :zoekt-enabled t
    :zoekt-web-url "http://cave-zoekt-web:6070"
    :chamber-enabled t
    :chamber-port 9444
    ;; Runner policy for repo-supplied .cave/workflows (Cave's own
    ;; dep-scan/fix jobs bypass these):
    :workflows-allow-privileged nil          ; deny `privileged: true` jobs
    :workflows-image-allowlist ("ghcr.io/" "docker.io/") ; nil = allow any
    ;; Pre-receive check sandbox:
    :checks-allow-network nil                 ; nil = isolate network if possible
    :checks-require-network-isolation nil     ; t = reject push if isolation unavailable
    :checks-memory-mb nil                     ; integer = per-process address-space cap
    :checks-timeout-seconds 120               ; fallback wall-clock per check
    ;; Landlock filesystem sandbox for git + check subprocesses:
    :sandbox-landlock t)                      ; t = confine (cross-repo isolation); no-op without Landlock
   ```

   `:workflows-allow-privileged` defaults to `nil` — a repo workflow that
   requests `privileged: true` is rejected (the run fails with the reason) until
   an admin flips it. `:workflows-image-allowlist` is `nil` (any image) unless
   set to a list of allowed name prefixes.

   Pre-receive checks run each configured command against an extracted worktree
   of the pushed tree, with a scrubbed environment and a per-check wall-clock
   timeout. `:checks-allow-network` nil tries to isolate the network with
   `unshare -n` (best-effort — the default container can't, so it warns and runs
   without unless `:checks-require-network-isolation` is `t`, which fails the
   push closed). Independently, `:sandbox-landlock` (on by default) wraps git
   push/clone, metadata reads, and checks in a Landlock filesystem sandbox scoped
   to the target repo — kernel-enforced cross-repo isolation, a no-op where
   Landlock is unavailable. In containers these map to
   `CAVE_WORKFLOWS_ALLOW_PRIVILEGED`, `CAVE_WORKFLOWS_IMAGE_ALLOWLIST`,
   `CAVE_CHECKS_ALLOW_NETWORK`, `CAVE_CHECKS_REQUIRE_NETWORK_ISOLATION`,
   `CAVE_CHECKS_MEMORY_MB`, `CAVE_CHECKS_TIMEOUT_SECONDS`, and
   `CAVE_SANDBOX_LANDLOCK`.

2. **`cave.yaml`** — declarative deployment manifest consumed by `cavectl`:

   ```yaml
   apiVersion: v1
   cave:        { image: ghcr.io/atgreen/cave:main, base_url: https://cave.example.com,
                  secret_key: <32-byte hex> }
   ports:       { http: 9080, ssh: 22, ssh_bind: 0.0.0.0,
                  grpc: 9443, grpc_bind: 0.0.0.0,
                  keycloak: 9180, mailpit: 9025 }
   database:    { mode: local, image: docker.io/postgres:16-alpine, password: <random> }
   auth:
     mode: keycloak
     keycloak:  { image: ghcr.io/atgreen/cave-keycloak:main,
                  admin_user: admin, admin_password: <strong>,
                  public_url: https://auth.cave.example.com,
                  client_secret: <random 32-char> }
   runner:      { enabled: true, image: ghcr.io/atgreen/cave-runner:main, count: 1 }
   zoekt:       { enabled: true, image: ghcr.io/atgreen/cave-zoekt:main }
   smtp:
     mode: external
     host: smtp.resend.com
     port: 587
     starttls: true
     user: resend
     password: re_…
     from: cave@example.com
     from_display_name: Cave
   chamber:     { nodes: [] }
   runtime:     { engine: auto, network: cave-net, prefix: cave }
   ```

## CLI

### `cave` — user CLI

Talks to a cave-server over HTTPS using a personal API token from
Settings → API Tokens.

```
cave login [--base-url URL] [--token TOKEN]   Save server URL and token
cave logout                                   Remove saved config
cave status                                   Show current auth config

cave repo create <name> [--description "…"] [--private]
                        [--mode empty|import|mirror]
                        [--url URL] [--auth-token TOKEN]
                        [--mirror-interval MINUTES] [--json]

cave issue list   [--status open|closed] [--json]
cave issue get    [--json] <number>
cave issue create --title TITLE [--body TEXT] [--json]
cave issue close  <number>
cave issue reopen <number>

cave pr list     [--state open|merged|closed] [--json]
cave pr view     [--web] [--json] <number>
cave pr create   --source BRANCH --target BRANCH [--json]
cave pr checks   [--json] <number>     # exit 1 unless all checks pass — CI-gateable
cave pr review   <number> --approve|--request-changes|--comment [--body TEXT|-]
cave pr close    <number>
cave pr reopen   <number>
```

Set `CAVE_REPO=owner/repo` (or pass `--repo`) so issue and PR commands can find
the target without you typing it every time. `cave pr checks <n>` exits non-zero
unless every check is green, so it composes in scripts (`cave pr checks 7 && …`).

### `cave-server` — server binary

```
cave-server serve          Start the web server
cave-server init           Initialize the database
cave-server migrate        Run pending migrations
cave-server runner         Start an automation runner agent
cave-server run-checks     Run pre-receive checks + enforce protected branches
                           (called by git hook)
cave-server reverify       Backfill commit signature verification over history
cave-server backfill-languages
                           Recompute and store each repo's primary language
cave-server sync-mirrors   Sync repo mirrors (push mirrors after each push)
cave-server sync-themes    Sync user themes from a cave-themes repo
cave-server update-keys    Regenerate SSH authorized_keys
cave-server git-shell      SSH git transport handler (called by sshd)
cave-server git-proxy      Proxy git protocol through Chamber gRPC (called by
                           cave-shell.sh when chamber is enabled)
cave-server post-receive   Handle post-receive events (legacy; HTTP endpoint is the
                           live path)
```

### `cavectl` — deployment tool

```
cavectl init                Generate cave.yaml and bring up the full stack
cavectl apply               Reconcile containers to match cave.yaml
cavectl status              Show running containers + assigned ports
cavectl logs <service>      Tail container logs
cavectl doctor              Run sanity checks: runtime, ports, DNS, containers,
                            schema, OIDC, SMTP realm config
cavectl backup              Tar up postgres + repos + cave.yaml
cavectl restore <archive>   Restore from a cavectl backup archive
cavectl instances           List all cavectl-managed instances on this host
cavectl destroy             Tear down (keep volumes with --keep-data)
cavectl version             Print cavectl version
```

## REST API

The API is described by an **OpenAPI 3.1** document served at
`/api/v1/openapi.json`, with an interactive reference at `/api/v1/docs`. Point
any OpenAPI client generator (openapi-generator, Speakeasy, Fern, Kiota, …) at
the spec to produce a typed client library in your language. Timestamps are
RFC 3339 UTC strings; errors are `{ "error": "message" }`.

Authenticate with `Authorization: Bearer <api-token>` (issued from Settings →
API Tokens); read endpoints on public repos work unauthenticated.

```
POST   /api/v1/user/repos                                Create a personal repo
GET    /api/v1/repos/:owner/:repo/issues
POST   /api/v1/repos/:owner/:repo/issues
GET    /api/v1/repos/:owner/:repo/issues/:number
PATCH  /api/v1/repos/:owner/:repo/issues/:number         Close / reopen
GET    /api/v1/repos/:owner/:repo/pulls
POST   /api/v1/repos/:owner/:repo/pulls
GET    /api/v1/repos/:owner/:repo/pulls/:number
PATCH  /api/v1/repos/:owner/:repo/pulls/:number             Close / reopen ({"state": …})
GET    /api/v1/repos/:owner/:repo/pulls/:number/reviews
POST   /api/v1/repos/:owner/:repo/pulls/:number/reviews
GET    /api/v1/repos/:owner/:repo/statuses/:sha
POST   /api/v1/repos/:owner/:repo/statuses/:sha
```

## Automation Runners

```bash
# Generate a registration token (Admin → Runners, or org/repo settings)
cave-server runner --url grpc://cave-host:9443 --token <token> --name my-runner

# Or use the runner container image (Podman-in-Podman)
make runner-image
podman run --privileged ghcr.io/atgreen/cave-runner:main \
  --url grpc://cave-host:9443 --token <token>
```

Automations are configured per-repo in Settings → Automations, with triggers:
`pre_receive`, `post_receive`, `changeset_opened`, `changeset_merged`, `manual`.

Workflow files at `.cave/workflows/*.yml` are picked up on push and scheduled
across runners. Repo-supplied jobs are subject to admin policy: `privileged`
jobs are denied unless `:workflows-allow-privileged` is set, and job images can
be restricted with `:workflows-image-allowlist`. A job that violates policy
fails its run with the reason instead of being dispatched.

```yaml
# .cave/workflows/ci.yml
on: [push]
jobs:
  test:
    # No image? One is built on the fly from these nixpkgs names via Nixery.
    dependencies: [nodejs, git]
    # Directories persisted across runs in a repo-scoped volume — a big
    # speed-up for dependency installs and incremental builds.
    cache:
      - ~/.npm
      - node_modules
    env:
      NODE_ENV: test          # workflow/job/step env: layering
    steps:
      - name: install
        run: npm ci
      - name: test            # multi-line run: | blocks are supported
        run: |
          echo "building $GITHUB_REPOSITORY @ $GITHUB_SHA"
          npm test
          echo "passed=true" >> "$GITHUB_OUTPUT"
```

**Reusable caches** — a job's `cache:` directive lists in-container directories
to persist across runs (npm, cargo, `~/.go/pkg`, `~/.cache/common-lisp`, …).
Each is backed by a **repo-scoped** persistent volume (different repos never
share a cache), mounted at the declared path. The runner additionally keeps a
built-in Common Lisp FASL cache at `~/.cache/common-lisp` for every job, so SBCL
builds stay incremental without any configuration.

**GitHub-Actions-style workflows** — cave aims to run the common subset of GitHub
Actions syntax so existing `run:`-based workflows port with little change:

- **Triggers** — `on: [push]`, `on: [pull_request]`, and `on: [tag]` (a push to a
  `refs/tags/*` ref; lets a release workflow run only on tags). `manual` too.
- **`env:` layering** — at workflow, job, and step level (step overrides job
  overrides workflow), injected into each step.
- **Standard env** — every step gets the usual `GITHUB_*` variables
  (`GITHUB_REPOSITORY`, `GITHUB_SHA`, `GITHUB_REF`/`REF_NAME`/`REF_TYPE`,
  `GITHUB_RUN_ID`, `GITHUB_WORKFLOW`, `GITHUB_ACTOR`, `GITHUB_EVENT_NAME`,
  `GITHUB_WORKSPACE`, …), the `RUNNER_OS`/`RUNNER_ARCH`/`RUNNER_TEMP` set, and
  `CI=true`. **For every `GITHUB_*` variable there is a `CAVE_*` twin** with the
  same value (`CAVE_REPOSITORY`, `CAVE_SHA`, …) for cave-native scripts.
- **File commands** — `$GITHUB_OUTPUT`, `$GITHUB_ENV`, `$GITHUB_PATH`, and
  `$GITHUB_STEP_SUMMARY` (each with a `CAVE_*` twin). Writing to `$GITHUB_ENV` /
  `$GITHUB_PATH` carries into later steps; `$GITHUB_STEP_SUMMARY` is streamed into
  the job log; `::add-mask::` redacts a value from the logs.
- **Multi-line `run: |`** literal and `>` folded block scalars are parsed
  (shell `#` comments inside a block are preserved).
- **`${{ }}` expressions** in `run:`, `env:`, and `if:` are evaluated — the full
  GitHub Actions expression language (operators, `contains`/`startsWith`/`format`/
  `join`/`toJSON`/`fromJSON`/status functions/…) against the `github`, `env`,
  `secrets`, `runner`, `steps`, `matrix`, `needs`, and `job` contexts. **A `cave`
  context mirrors `github`**, so `${{ cave.sha }}` works like `${{ github.sha }}`.
- **Step `if:` conditions** gate execution: the default is `success()` (a step is
  skipped once a prior step failed); `always()`/`failure()`/`cancelled()` and any
  expression (e.g. `if: steps.build.outcome == 'success'`) are honored.
- **`steps.<id>.outputs`** — a step's `$GITHUB_OUTPUT` is captured (keyed by its
  `id:`) with `outcome`/`conclusion`, and is available to later steps' `${{ }}`
  and `if:`.
- **`strategy.matrix`** expands a job into one job per combination (with
  `include`/`exclude`), each exposing its combo as `${{ matrix.* }}`.
- **Job `outputs:` + `needs.<job>.outputs`/`needs.<job>.result`** — a job resolves
  its declared `outputs:` from its steps and publishes them to dependent jobs.
  A `needs:` on a matrix job **fans in**: the dependent waits on every expanded
  instance, its `outputs` merge, and `result` is `success` only if all succeeded.
- **`uses:` actions** — like GitHub, **the workspace starts empty**; a step must
  `uses: actions/checkout@v4` to populate it (cave does not auto-clone). Actions
  are referenced `owner/repo@ref` and resolved **cave-local** (no github.com), in
  two tiers:
  - **Built-in `actions/*`** — cave-authored, compiled into the runner. They run
    as in-runner *orchestrators* that effect their changes **in the job
    container** via `podman exec` (so they share the `run:` steps' filesystem and
    environment, like GitHub driving a container job). `actions/checkout` clones
    the repo into the container honoring `ref`, `path`, `fetch-depth` (`0` = full
    history), and `submodules`, and sets `commit`/`ref` outputs.
  - **Third-party `owner/repo@ref`** — fetched from the chamber and run in a
    dedicated **`cave-actions` container** (a `using: lisp` action), sharing only
    the workspace + the file-command runtime dir. Untrusted code never enters the
    runner process; a crash/runaway dies with the throwaway container. The action
    uses the dependency-free `cave-actions` (`ca`) SDK (`input`/`set-output`/
    `export-var`/`add-path`/`sh`/`api`); `with:` inputs arrive as `INPUT_*`.

Not yet supported: Docker/JS/composite actions; `docker://`, URL, and `./local`
action refs; per-action network policy + a minted job-scoped token (the slot is
wired, currently empty); `hashFiles()` (stubbed).

## Themes

Switch themes in Settings → Theme. Built-in: Terminal Warmth, Solarized Dark,
Nord, Dracula, Light.

Custom themes: fork `cave/cave-themes`, add `.toml` files:

```toml
# mytheme.toml
bg = "#1a1b26"
accent = "#7aa2f7"
font-url = "https://fonts.googleapis.com/css2?family=JetBrains+Mono"
font-mono = "'JetBrains Mono', monospace"
```

Push → themes sync automatically. Invalid values get lint issues filed on your
themes repo.

## Makefile targets

```
make build         Build cave-server + cave + cavectl + zoekt-git-index
make load          Load-test the Lisp tree without producing an image
make lint          Run ocicl lint on the source
make podman-up     Bring up the laptop dev stack via plain podman commands
make podman-down   Tear it down
make runner-image  Build the runner container image
make release       Build prod images and tag :prod
make prod-install  Install systemd quadlet units
make prod-start    Start production
make prod-stop     Stop production
make prod-rollback Roll back to cave:prod-previous
make prod-backup   Back up all data
make prod-restore F=path/to/archive.tar.gz
make prod-status   Show systemd service status
```

## Testing

End-to-end tests are Playwright browser tests in `tests/`, run against a
running Cave stack:

```bash
npm install              # once
npm run test:install     # install the Chromium browser (once)

# Bring a stack up first (cavectl init, or make podman-up), then point the
# tests at it:
CAVE_URL=http://localhost:9080 \
CAVE_ADMIN_USER=admin CAVE_ADMIN_PASSWORD=admin \
  npm test
```

`npm test` runs `playwright test` (15 specs covering smoke pages, registration,
org/repo flows, and the file browser). The tests assume an admin user can log
in via Keycloak.

There is not yet a unit/integration suite for migrations, the REST API, or
merge-policy rules — see **Planned** above.

## License

MIT

## Author

Anthony Green <green@moxielogic.com>
