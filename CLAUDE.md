# Cave

A self-hosted code forge written in Common Lisp (SBCL).

## Build

```bash
make          # builds the cave binary using /usr/bin/sbcl
make load     # load-test without building image
make lint     # run ocicl lint on source
```

## Run locally

```bash
podman run -d --name cave-pg -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave -p 5432:5432 postgres:16-alpine
./cave init --admin-user admin --admin-password admin --config cave.conf
./cave serve --config cave.conf
```

## Run in container

```bash
podman network create cave-net
podman run -d --name cave-pg --network cave-net -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave postgres:16-alpine
podman build -t cave -f Containerfile.local .
podman run -d --name cave --network cave-net -p 8080:8080 -p 2222:22 -e CAVE_DB_HOST=cave-pg -e CAVE_ADMIN_USER=admin -e CAVE_ADMIN_PASSWORD=admin -v cave-data:/var/lib/cave cave:latest
```

## Architecture

- **src/package.lisp** — Package definition and exports
- **src/config.lisp** — S-expression config parser (cave.conf)
- **src/db.lisp** — PostgreSQL via postmodern, numbered migrations
- **src/auth.lisp** — bcrypt passwords, sessions, API tokens
- **src/model.lisp** — All domain queries: users, orgs, repos, issues, changesets, reviews, concerns, stacks, events, SSH keys
- **src/git.lisp** — Shell out to git CLI for branch listing, log, file tree, diff
- **src/views.lisp** — All HTML via Spinneret (s-expression HTML, no template files)
- **src/ssh.lisp** — SSH transport: git-shell auth, authorized_keys generation
- **src/server.lisp** — Hunchentoot routes and request handling
- **src/main.lisp** — CLI subcommands via clingon: init, serve, migrate, git-shell, update-keys
- **cave-shell.sh** — Bash wrapper called by sshd, execs git after auth

## Key conventions

- Use `/usr/bin/sbcl` (system SBCL), not linuxbrew, so binaries work in containers.
- HTML is generated with Spinneret, not template files. All views are functions in views.lisp.
- Logging via llog (structured). Known bug: `set-root-level` doesn't suppress output (atgreen/cl-llog#2).
- Database queries use postmodern's s-sql. Each migration is a (version . sql-string) pair in `*migrations*`.
- Routes use easy-routes with `/:owner/:repo-name` for unified user/org repo access.
- Repos can be owned by an org (org_id) or a user (owner_id), never both.
- Config is an s-expression plist in cave.conf. Secrets (cave.conf, data/) are gitignored.
- The `when-let` macro is defined in auth.lisp.
- SSH transport: sshd calls cave-shell.sh which calls `cave git-shell` for auth, then execs git.
- Static files served from `static/` directory.
