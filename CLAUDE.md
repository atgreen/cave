# Cave

A self-hosted code forge written in Common Lisp (SBCL).

## Build

```bash
make          # builds cave + cav + cavectl + zoekt-git-index using /usr/bin/sbcl
make load     # load-test the Lisp tree without producing an image
make lint     # run ocicl lint on source
```

## Run locally

The fast path uses cavectl (declarative; generates a cave.yaml with the right
secrets, brings up the full stack):

```bash
./cavectl init   # spins up postgres, keycloak (with cave theme + realm),
                 # mailpit, zoekt, cave, runner — pulling from ghcr.io
```

The plumbing path uses raw podman:

```bash
podman network create cave-net
podman run -d --name cave-pg --network cave-net \
  -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
  postgres:16-alpine
podman build -t cave -f Containerfile.local .
podman run -d --name cave --network cave-net \
  -p 8080:8080 -p 2222:22 \
  -e CAVE_DB_HOST=cave-pg \
  -e CAVE_OIDC_ISSUER=http://cave-keycloak:8080/realms/cave \
  -e CAVE_OIDC_CLIENT_ID=cave -e CAVE_OIDC_CLIENT_SECRET=... \
  -v cave-data:/var/lib/cave cave:latest
```

Cave uses OIDC via Keycloak for all user auth — there's no local-admin
username/password flag.

## Architecture

- **src/package.lisp** — Package definition and exports
- **src/config.lisp** — S-expression config parser (cave.conf)
- **src/db.lisp** — PostgreSQL via postmodern, numbered migrations
- **src/auth.lisp** — OIDC auth, sessions, API tokens, sudo mode
- **src/model.lisp** — Domain queries: users, orgs, repos, issues, PRs, reviews,
  releases, signatures, page views, SSH keys, etc.
- **src/git.lisp** — Git CLI integration (branch listing, log, file tree, diff,
  merge, tags, signature verification, commit trailers)
- **src/views.lisp** — All HTML via Spinneret (s-expression HTML, no template files)
- **src/notify.lisp** — Email notifications and webhooks
- **src/search-zoekt.lisp** — Zoekt code search: indexing, API client, visibility filter
- **src/metrics.lisp** — Prometheus metrics endpoint
- **src/runner-service.lisp** — gRPC service for automation runners
- **src/workflow.lisp** — Workflow file parsing, scheduling, dependency resolution
- **src/yaml.lisp** — Minimal YAML parser for workflow files
- **src/chamber.lisp** — Git storage gRPC service
- **src/chamber-client.lisp** — Chamber client with `chamber-or` graceful fallback to direct git
- **src/chamber-router.lisp** — Multi-chamber routing (Praefect-style)
- **src/ssh.lisp** — SSH transport: git-shell auth, authorized_keys generation
- **src/server.lisp** — Hunchentoot routes and request handling
- **src/main.lisp** — CLI subcommands via clingon: init, serve, migrate,
  git-shell, update-keys, post-receive, runner, sync-mirrors, sync-themes
- **cave-shell.sh** — Bash wrapper called by sshd, parses `cave git-shell` output
  (user-id + disk-path), exports `CAVE_PUSH_USER_ID`, then execs git
- **cli/cavectl/** — Go source for the declarative deploy tool
- **internal/cavectl/** — Go libraries: config, plan, apply, runtime, doctor,
  backup, instance discovery
- **keycloak/themes/cave/** — Custom Keycloak login theme (parent: keycloak.v2)
- **keycloak/cave-realm.json** — Realm import with `__PLACEHOLDER__` tokens
  substituted at container start by `keycloak/entrypoint.sh`
- **deploy/quadlet/** — systemd-user quadlet units for production

## Key conventions

- Use `/usr/bin/sbcl` (system SBCL), not linuxbrew, so binaries work in containers.
- HTML is generated with Spinneret, not template files. All views are functions
  in `views.lisp`.
- Logging via llog (structured). Known bug: `set-root-level` doesn't suppress
  output (atgreen/cl-llog#2).
- Database queries use postmodern's s-sql. Each migration is a
  `(version . sql-string)` pair in `*migrations*`. The splitter is naive on
  semicolons — keep `;` out of inline SQL comments.
- Routes use easy-routes with `/:owner/:repo-name` for unified user/org repo
  access. Trailing slashes on GET URIs are 301-redirected to the slash-trimmed
  form by the acceptor.
- Repos can be owned by an org (org_id) or a user (owner_id), never both.
- Config is an s-expression plist in cave.conf. Secrets (cave.conf, data/,
  `/*.yaml` for cavectl instance configs) are gitignored.
- The `when-let` macro is defined in `auth.lisp`.
- SSH transport: sshd calls `cave-shell.sh` which calls `cave git-shell` for
  auth. `cave git-shell` prints two lines on stdout: the user-id, then the
  repo disk path. `cave-shell.sh` exports `CAVE_PUSH_USER_ID` so the
  post-receive hook can forward the actor to the internal HTTP endpoint
  (which logs the rich git.push event and verifies signatures).
- Chamber RPC: every chamber-* read function wraps its body in `chamber-or`,
  which falls back to direct git on `chamber-rpc-error`. Never let a chamber
  RPC failure bubble — it'll take down the SBCL image.
- Container images are published from `main` and tagged releases to
  `ghcr.io/atgreen/{cave,cave-runner,cave-zoekt,cave-keycloak}` by the
  containers.yaml GHA workflow. `cavectl` pulls from `:main` by default.
- Static files served from `static/`.
