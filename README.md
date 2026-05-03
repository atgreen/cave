# Cave

A self-hosted code forge written in Common Lisp. Push code, review changes, merge with confidence.

Cave is built for small teams who want to own their infrastructure without the bloat of enterprise forges. It's fast, opinionated, and runs on a single server.

## Features

- **Git hosting** — SSH push/clone with per-repo access control
- **Pull requests** — graduated review model (approve, approve with concerns, request changes), merge eligibility rules, squash merge
- **Stacked changesets** — first-class support for dependent PRs
- **Issues** — with comments, close/reopen, labels
- **Code browser** — file tree, Monaco editor, syntax-highlighted diffs via diff2html
- **README rendering** — server-side Markdown with tables and fenced code blocks
- **Keycloak SSO** — OIDC authentication, self-registration, email verification, 2FA (TOTP)
- **Observability** — Prometheus metrics, Grafana dashboards, SBCL runtime stats
- **Webhooks** — HTTP callbacks on push, PR, and issue events
- **Commit status API** — external CI reports pass/fail on PRs
- **Automation runners** — self-hosted gRPC runners execute checks and automations
- **Repo mirroring** — push to and pull from GitHub/GitLab/Codeberg
- **User themes** — built-in themes (Terminal Warmth, Nord, Solarized, Dracula, Light) plus custom themes via `cave-themes` repos
- **Backup/restore** — one-command backup and restore of all data
- **Quadlet deployment** — systemd user services for production, with rollback

## Quick Start

```bash
# Build
make

# Start locally (Cave + Postgres + Keycloak + Mailpit)
make podman-up

# Open Cave
open http://localhost:8080

# Keycloak admin
open http://localhost:8180  # admin/admin

# Mailpit (email catcher)
open http://localhost:8025
```

## Production Deployment

```bash
# Build and tag a release
make build
make release

# Install systemd quadlet units
make prod-install

# Start production
make prod-start

# Ports: Cave 9080, Keycloak 9180, Mailpit 9025, SSH 9222
```

### Deploy workflow

```bash
make build && make release && systemctl --user restart cave
```

### Rollback

```bash
make prod-rollback
```

### Backup

```bash
make prod-backup                    # → ~/cave-backups/cave-YYYY-MM-DD.tar.gz
make prod-restore F=path/to/archive.tar.gz
```

## Architecture

Cave is a single Common Lisp (SBCL) binary serving HTTP via Hunchentoot, with a gRPC runner service via ag-grpc. All HTML is generated with Spinneret (s-expression HTML). CSS is a single file. No JavaScript frameworks.

```
src/
├── package.lisp        — Package definition
├── config.lisp         — S-expression config parser
├── db.lisp             — PostgreSQL via Postmodern, numbered migrations
├── auth.lisp           — OIDC auth, sessions, API tokens, sudo mode
├── model.lisp          — Domain queries: users, orgs, repos, issues, PRs, reviews
├── git.lisp            — Git CLI integration (branch, log, tree, diff, merge)
├── views.lisp          — All HTML views via Spinneret
├── notify.lisp         — Email notifications, webhooks, automation scheduling
├── metrics.lisp        — Prometheus metrics endpoint
├── runner-service.lisp — gRPC service for automation runners
├── ssh.lisp            — SSH transport, authorized_keys generation
├── server.lisp         — HTTP routes and request handling
└── main.lisp           — CLI subcommands (serve, init, migrate, runner, etc.)
```

### Key dependencies

| Library | Purpose |
|---------|---------|
| Hunchentoot + easy-routes | HTTP server and routing |
| Postmodern | PostgreSQL client |
| Spinneret | S-expression HTML generation |
| ag-grpc | gRPC server for runner protocol |
| 3bmd | Markdown rendering |
| Dexador | HTTP client (OIDC, webhooks) |
| Ironclad | Cryptography (tokens, HMAC) |
| cl-bcrypt | Password hashing (legacy) |

## Configuration

Cave reads `cave.conf`, an s-expression plist:

```lisp
(:http-port 8080
 :grpc-port 9443
 :ssh-port 22
 :data-dir "/var/lib/cave"
 :db-host "localhost"
 :db-name "cave"
 :db-user "cave"
 :db-password "cave"
 :base-url "http://localhost:8080"
 :oidc-issuer "http://localhost:8180/realms/cave"
 :oidc-client-id "cave"
 :oidc-client-secret "cave-dev-secret")
```

## CLI

```
cave serve     — Start the web server
cave init      — Initialize the database
cave migrate   — Run pending migrations
cave runner    — Start an automation runner agent
cave run-checks — Run pre-receive checks (called by git hook)
cave sync-mirrors — Sync repo mirrors
cave sync-themes  — Sync user themes from cave-themes repo
cave update-keys  — Regenerate SSH authorized_keys
cave git-shell    — SSH git transport handler
cave post-receive — Handle post-receive events
```

## REST API

Authenticate with `Authorization: Bearer <api-token>`.

```
GET    /api/v1/repos/:owner/:repo/issues
POST   /api/v1/repos/:owner/:repo/issues
GET    /api/v1/repos/:owner/:repo/issues/:number
GET    /api/v1/repos/:owner/:repo/pulls
POST   /api/v1/repos/:owner/:repo/pulls
GET    /api/v1/repos/:owner/:repo/pulls/:number
GET    /api/v1/repos/:owner/:repo/pulls/:number/reviews
POST   /api/v1/repos/:owner/:repo/pulls/:number/reviews
GET    /api/v1/repos/:owner/:repo/statuses/:sha
POST   /api/v1/repos/:owner/:repo/statuses/:sha
```

## Automation Runners

Cave includes a gRPC-based runner system for executing automations on repo events.

```bash
# Generate a registration token (Admin page)
# Start a runner
cave runner --url grpc://cave-host:9443 --token <token> --name my-runner

# Or use the runner container image (includes Podman for nested containers)
make runner-image
podman run --privileged cave-runner:latest \
  --url grpc://cave-host:9443 --token <token>
```

Automations are configured per-repo in Settings → Automations with triggers:
`pre_receive`, `post_receive`, `changeset_opened`, `changeset_merged`, `manual`

## Themes

Switch themes in Settings → Theme. Built-in: Terminal Warmth, Solarized Dark, Nord, Dracula, Light.

Custom themes: fork `cave/cave-themes`, add `.toml` files:

```toml
# mytheme.toml
bg = "#1a1b26"
accent = "#7aa2f7"
font-url = "https://fonts.googleapis.com/css2?family=JetBrains+Mono"
font-mono = "'JetBrains Mono', monospace"
```

Push → themes sync automatically. Invalid values get lint issues filed on your repo.

## Makefile Targets

```
make              — Show all targets
make build        — Build cave binary + Go CLI
make test         — Run Playwright tests
make podman-up    — Start dev environment
make podman-down  — Stop dev environment
make runner-image — Build runner container image
make release      — Build prod container image
make prod-install — Install quadlet systemd units
make prod-start   — Start production
make prod-stop    — Stop production
make prod-backup  — Backup all data
make prod-restore — Restore from backup
make prod-rollback — Roll back to previous release
make prod-status  — Show service status
```

## License

MIT

## Author

Anthony Green
