# Cave

A self-hosted code forge written in Common Lisp. Push code, review changes, merge with confidence.

Cave is built for small teams who want to own their infrastructure without the bloat of enterprise forges. It's fast, opinionated, and runs on a single server.

## Features

The surface is split by maturity. **Implemented** features are in everyday use;
**Experimental** ones work but have known gaps or sharp edges; **Planned** ones
are not built yet.

### Implemented

- **Git hosting** — SSH push/clone with per-repo access control, plus anonymous read-only clone over HTTPS
- **Pull requests** — graduated review model (approve, approve with concerns, request changes); enforced merge-eligibility rules: required approvals, blocking change-requests, unresolved concerns, and required status checks (failing/pending checks block the merge); squash merge
- **Issues** — create, list, comment, close/reopen (labels, assignees, and filtering are *Planned*)
- **Releases** — tag-backed releases with markdown body and per-asset uploads (≤ 100 MB each), download counts
- **Commit status API** — external CI reports pass/fail per context on a commit; feeds merge eligibility
- **Commit signature verification** — SSH-signed commits validated against registered user keys on push; "Verified" badge in the commit list
- **Code browser** — file tree with per-file last commit and relative age, repo language breakdown, file-type icons, Monaco editor, syntax-highlighted diffs, branch/tag switcher (overview, code, tree, and file views), SSH/HTTPS clone widget, click-a-line-number permalinks (`#L42`) with a per-line action menu
- **README rendering** — server-side Markdown with tables and fenced code blocks; rendered HTML cached by blob sha
- **Code search** — full-text code search powered by [Zoekt](https://github.com/sourcegraph/zoekt), repo-scoped and global
- **Repo mirroring** — push to and pull from GitHub/GitLab/Codeberg, with scheduled sync
- **Webhooks** — HTTP callbacks on push, PR, and issue events
- **Pulse tab** — per-repo activity chart, total views, unique visitors, top contributors, referring sites (member-only)
- **Public landing** — anonymous visitors get a list of public repos and recent activity, no login required
- **User themes** — built-in (Terminal Warmth, Solarized Dark, Nord, Dracula, Light) plus custom themes via `cave-themes` repos
- **Keycloak SSO** — OIDC authentication, self-registration gated by admin approval, email verification, 2FA (TOTP)
- **Email** — SMTP via mailpit (dev) or any external relay (Resend / Postmark / SES / Fastmail …) configured per deploy
- **Observability** — Prometheus metrics, Grafana dashboards, SBCL runtime stats
- **Backup/restore** — one-command backup and restore of Postgres + repos + config
- **Declarative deployment** — `cavectl` reconciles containers from a single `cave.yaml`; `cavectl doctor` runs end-to-end health checks
- **Quadlet deployment** — systemd user services for production, with rollback

### Experimental

- **Stacked changesets** — dependent PRs are tracked and displayed as a stack, but there is no atomic "land stack" yet; members still merge one PR at a time (see *Planned*)
- **Automation runners + workflows** — `.cave/workflows/*.yml` jobs are scheduled across self-hosted gRPC runners and report status back. Admin policy gates repo-supplied jobs: `privileged` is denied by default and images can be pinned to an allowlist (`:workflows-allow-privileged`, `:workflows-image-allowlist`). Still missing for a fully untrusted multi-tenant setup: per-repo policy overrides, secret masking in logs, and stronger runner-side isolation — so prefer trusted repos
- **Multi-chamber storage** — Praefect-style routing across git storage nodes (read/write split, health checks, async replication). Opt-in; single-chamber is the default and the well-exercised path

### Planned

- **Atomic stack landing** — ordered validation plus all-or-nothing merge of a stack
- **Richer issues** — labels, assignees, filtering, and `Closes #N` auto-close (the schema exists; the UI/API don't)
- **Sandboxed checks/runners** — pre-receive checks currently run as `bash -c` in the bare repo with no isolation; planned: a synthetic merge worktree, timeout/resource limits, a no-network option, and a runner image allowlist
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
├── auth.lisp           — OIDC auth, sessions, API tokens, sudo mode
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
| 3bmd | Markdown rendering |
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
    :oidc-issuer "https://auth.cave.example.com/realms/cave"
    :oidc-issuer-internal "http://cave-keycloak:8080/realms/cave"
    :oidc-client-id "cave"
    :oidc-client-secret "…"
    :zoekt-enabled t
    :zoekt-web-url "http://cave-zoekt-web:6070"
    :chamber-enabled t
    :chamber-port 9444
    ;; Runner policy for repo-supplied .cave/workflows (Cave's own
    ;; dep-scan/fix jobs bypass these):
    :workflows-allow-privileged nil          ; deny `privileged: true` jobs
    :workflows-image-allowlist ("ghcr.io/" "docker.io/")) ; nil = allow any
   ```

   `:workflows-allow-privileged` defaults to `nil` — a repo workflow that
   requests `privileged: true` is rejected (the run fails with the reason) until
   an admin flips it. `:workflows-image-allowlist` is `nil` (any image) unless
   set to a list of allowed name prefixes. In containers these map to
   `CAVE_WORKFLOWS_ALLOW_PRIVILEGED` (`t`/`nil`) and
   `CAVE_WORKFLOWS_IMAGE_ALLOWLIST` (space-separated prefixes).

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
```

Set `CAVE_REPO=owner/repo` (or pass `--repo`) so issue commands can find
the target without you typing it every time.

### `cave-server` — server binary

```
cave-server serve          Start the web server
cave-server init           Initialize the database
cave-server migrate        Run pending migrations
cave-server runner         Start an automation runner agent
cave-server run-checks     Run pre-receive checks (called by git hook)
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

Authenticate with `Authorization: Bearer <api-token>` (issued from Settings →
API Tokens):

```
POST   /api/v1/user/repos                                Create a personal repo
GET    /api/v1/repos/:owner/:repo/issues
POST   /api/v1/repos/:owner/:repo/issues
GET    /api/v1/repos/:owner/:repo/issues/:number
PATCH  /api/v1/repos/:owner/:repo/issues/:number         Close / reopen
GET    /api/v1/repos/:owner/:repo/pulls
POST   /api/v1/repos/:owner/:repo/pulls
GET    /api/v1/repos/:owner/:repo/pulls/:number
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
