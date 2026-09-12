# Cave

A self-hosted code forge written in Common Lisp (SBCL).

## Build

```bash
make          # builds cave-server + cave + cavectl + zoekt-git-index using /usr/bin/sbcl
make load     # load-test the Lisp tree without producing an image
make lint     # run ocicl lint on source
```

## Run locally

The fast path uses cavectl (declarative; generates a cave.yaml with the right
secrets, brings up the full stack):

```bash
./cavectl init   # spins up postgres, mailpit, zoekt, cave, runner — pulling
                 # from ghcr.io. Auth is Cave's embedded Usher OIDC provider
                 # (auth.mode: local); no external IdP.
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
  -e CAVE_OIDC_ISSUER=http://localhost:8080 \
  -e CAVE_OIDC_ISSUER_INTERNAL=http://localhost:8080 \
  -e CAVE_OIDC_CLIENT_ID=cave -e CAVE_OIDC_CLIENT_SECRET=cave-dev-secret \
  -e CAVE_BASE_URL=http://localhost:8080 \
  -v cave-data:/var/lib/cave cave:latest
# Bootstrap the first admin (embedded Usher has no users on a fresh instance):
podman exec cave cave-server usher-add-user --config /etc/cave.conf \
  --username admin --password admin --email admin@example.com --admin
```

Cave hosts its own OpenID Provider (Usher) in-process, so it authenticates all
users itself — the `oidc-issuer` is Cave's own base URL and every OIDC endpoint
(`/authorize`, `/token`, `/userinfo`) is served at the root. There's no
local-admin username/password flag; bootstrap the first admin with
`cave-server usher-add-user --admin` (see `make podman-up`). Cave can still
federate to an external OIDC provider by setting a different `oidc-issuer`.

## Architecture

- **src/package.lisp** — Package definition and exports
- **src/config.lisp** — S-expression config parser (cave.conf)
- **src/db.lisp** — PostgreSQL via postmodern, numbered migrations
- **src/auth.lisp** — embedded Usher OIDC provider (`init-usher`) + relying-party
  client, sessions, API tokens, sudo mode
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
- **cave-shell.sh** — Bash wrapper called by sshd, parses `cave-server git-shell` output
  (user-id + disk-path), exports `CAVE_PUSH_USER_ID`, then execs git
- **cli/cavectl/** — Go source for the declarative deploy tool
- **internal/cavectl/** — Go libraries: config, plan, apply, runtime, doctor,
  backup, instance discovery
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
- SSH transport: sshd calls `cave-shell.sh` which calls `cave-server git-shell` for
  auth. `cave-server git-shell` prints two lines on stdout: the user-id, then the
  repo disk path. `cave-shell.sh` exports `CAVE_PUSH_USER_ID` so the
  post-receive hook can forward the actor to the internal HTTP endpoint
  (which logs the rich git.push event and verifies signatures).
- Chamber RPC: every chamber-* read function wraps its body in `chamber-or`,
  which falls back to direct git on `chamber-rpc-error`. Never let a chamber
  RPC failure bubble — it'll take down the SBCL image.
- Container images are published from `main` and tagged releases to
  `ghcr.io/atgreen/{cave,cave-runner,cave-zoekt}` by the
  containers.yaml GHA workflow. `cavectl` pulls from `:main` by default.
- Static files served from `static/`.

## Skill routing

When the user's request matches an available skill, ALWAYS invoke it using the Skill
tool as your FIRST action. Do NOT answer directly, do NOT use other tools first.
The skill has specialized workflows that produce better results than ad-hoc answers.

Key routing rules:
- Product ideas, "is this worth building", brainstorming → invoke office-hours
- Bugs, errors, "why is this broken", 500 errors → invoke investigate
- Ship, deploy, push, create PR → invoke ship
- QA, test the site, find bugs → invoke qa
- Code review, check my diff → invoke review
- Update docs after shipping → invoke document-release
- Weekly retro → invoke retro
- Design system, brand → invoke design-consultation
- Visual audit, design polish → invoke design-review
- Architecture review → invoke plan-eng-review
- Save progress, checkpoint, resume → invoke checkpoint
- Code quality, health check → invoke health


<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:6cd5cc61 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->
