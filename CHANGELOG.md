# Changelog

## v0.4.0 — unreleased

### Releases (new tab)
- Releases tied to git tags, with a Markdown body and pre-release / draft flags.
- Per-asset uploads (≤ 100 MB), stored on disk under `<data>/releases/<repo>/<release>/`, with download counts and a "Latest" badge in the list.
- Creating a release auto-creates the git tag at the default branch's HEAD if it doesn't already exist.

### Pulse (new tab)
- Per-repo 14-day activity chart, color-coded by event type (push, clone, fork, issue, merge).
- Total views and unique visitors charts; the IP hash is salted with `:secret-key` so the raw IP is never stored.
- Top contributors and top referring sites side-by-side.
- HTTP clones (`git-upload-pack`) now log a `git.clone` event so they're counted alongside SSH clones.
- Pulse is repo-member-only — it exposes referrers and per-contributor activity.

### Commit provenance (new)
- New `cave_commit_signatures` table (migration 44).
- On every push, walk the new commit range and verify SSH signatures against an `allowed_signers` file built from every SSH key in cave's DB; cache the result.
- "Verified" / "Unverified" badge on the commit list and on the single commit page.
- `Co-Authored-By:` / `Signed-off-by:` and other trailers rendered as chips on the commit page.
- GPG signatures are detected but recorded as unverified for now (no GPG key store yet).

### Public reading
- Anonymous visitors at `/` now get a list of public repos and recent public activity instead of a login redirect.
- Public repo browsing already worked; the dashboard was the missing piece.

### Clone widget
- Every repo overview and Code tab gets an SSH/HTTPS clone-URL widget with a one-click Copy button.

### Repo visibility
- A repo's visibility can now be changed after creation from the settings Danger zone (admin-only). Previously `is_private` was fixed at creation time.
- Going private→public re-indexes the repo in Zoekt so it becomes searchable immediately; search results are already filtered by live visibility at query time.

### cavectl
- New `cavectl doctor` runs end-to-end health checks: container runtime, SELinux, `ip_unprivileged_port_start` vs configured ports, DNS, every expected container, cave's SSH listener, keycloak database, schema version, realm SMTP placeholder substitution.
- New `cave-keycloak` container image bakes the cave theme and the realm import on top of `keycloak:26.0`; runs `kc.sh build --db=postgres` in a builder stage so `start --optimized` works at runtime.
- Realm import takes runtime values for the OIDC client secret, the cave base URL, and the entire SMTP block — every deploy gets a unique OIDC client secret.
- `KeycloakConfig` gains `public_url` (sets `KC_HOSTNAME`, `KC_PROXY_HEADERS=xforwarded`) and `client_secret`. New port `ports.keycloak` published to the host.
- Ensure-keycloak-DB step now creates the `keycloak` database before launching keycloak (postgres only created `cave`).
- New `ports.ssh_bind` config (default `127.0.0.1`) — set to `0.0.0.0` on a public VPS so cave's SSH port is reachable from outside.
- New `SMTPConfig` with `mode: mailpit` (default; spawns a mailpit container) or `mode: external` (Resend / Postmark / SES / any relay). Substituted into the realm SMTP block at start.
- New `ports.mailpit` (web UI) when mailpit mode is on.
- `CreateNetwork` is now idempotent.
- Default images point at `ghcr.io/atgreen/*` so a fresh VPS install pulls instead of needing to build.
- Cave-runner support in `cavectl init` and `cavectl apply`.
- `cavectl backup` and `cavectl init --from-backup`.

### Container images and CI
- New `Publish container images` workflow publishes `cave`, `cave-runner`, `cave-zoekt`, and `cave-keycloak` to `ghcr.io/atgreen/*` from `main` and tagged releases.
- Tags: `sha-<short>` (immutable), `main` (rolling), and on `v*` tags `<version>`, `prod`, `latest`.

### Resilience
- Chamber RPC failures (including the ag-grpc framing bug we hit on cave-themes) used to walk all the way up to SBCL's top-level and exit the image. Now wrapped in `chamber-or` — every read falls back to direct git on `chamber-rpc-error`; the channel is reset so the next call doesn't reuse a poisoned stream.
- `/raw/` binary blobs are streamed directly to the response instead of being round-tripped through Chamber gRPC.
- Git CLI output (diffs, logs, blobs) is now decoded with a replacement character instead of a bare `:utf-8` slurp. A diff carrying a non-UTF-8 byte (latin-1 source, binary hunk) used to raise `STREAM-DECODING-ERROR` and 500 the page — most visibly on the PR view when the Chamber RPC path was degraded and `chamber-or` fell back to direct git. Valid UTF-8 still decodes exactly; only the bad octet becomes `U+FFFD`.
- The stale-workflow-job reaper's `UPDATE … FROM` referenced the update target table `j` inside a `LEFT JOIN` in the `FROM` clause, which Postgres rejects (`invalid reference to FROM-clause entry for table "j"`). The reaper failed on every 120s tick, so abandoned jobs (dead/restarted runners) were never requeued or failed. The runner-abandoned test is now a correlated `NOT EXISTS` subquery in `WHERE`, where `j` is in scope.
- Chamber gRPC calls on a shared channel are now serialized per channel. ag-grpc's client connection has no reader thread and no read-lock, so two Hunchentoot workers calling on the same channel interleaved their frame reads and desynced the HTTP/2 stream (`-N is not of type (MOD …)`, `NIL is not of type BUFFER`), after which the reset closed the socket under the others (broken pipe / closed stream). A per-channel call lock keeps concurrent reads from overlapping; calls to different chamber nodes still run in parallel.
- Push mirrors no longer prune refs the remote's own CI maintains. `git-push-mirror` used `git push --mirror`, which deletes any ref the destination has that cave doesn't — so a `gh-pages` branch built by GitHub Actions on the mirror (and never present on cave) was wiped on every push, taking the published site down and disabling GitHub Pages. It now force-updates only the refs cave owns (`+refs/heads/*`, `+refs/tags/*`) and leaves everything else on the remote intact. Trade-off: a branch deleted on cave is no longer pruned from the mirror.
- Merging a PR from the web UI/API now syncs push mirrors. The merge advances the target ref out of band of the git post-receive hook (which is what runs the mirror sync on a normal push), so mirrors silently fell behind after every web merge. `perform-pr-merge` now kicks a background mirror sync, covering the web, API, and auto-merge paths.

### UX polish
- Trailing-slash URIs (`/atgreen/`) now 301-redirect to the slash-trimmed form with a path-only `Location` header so we don't take an extra `http→https` hop behind Caddy.
- Activity feed renders as prose ("green pushed 3 commits to main: Fix realm theme") instead of raw `git.push` event types. Relative timestamps everywhere.
- Repo overview shows last-pushed-at relative to now; `last_pushed_at` column added to `cave_repos` and bumped by the post-receive HTTP endpoint.
- Push events now record ref, commit count, and tip-commit subject as metadata.
- README rendered HTML is cached by blob sha to avoid re-rendering on every visit.

### Keycloak
- Fixed realm theme so the login page renders under Keycloak 26 (the upstream `keycloak` parent theme was removed in 26; we inherit from `keycloak.v2` and rewrote the cave overrides for PatternFly v5).
- Login theme inheritance, theme bind-mount SELinux labels (`:ro,z`), and the OIDC redirect URI list now include the deploy's `base_url`.

### Internals
- New `chamber-or` macro replaces the `if (chamber-enabled-p) … direct-git` pattern in every read function.
- New `git-create-tag` / `git-tag-exists-p` helpers for the releases flow.
- Two-layer caching for the `/raw/` endpoint.
- Monaco syntax highlighting for Common Lisp.
- Live log streaming via SSE for workflow runs.

---

## v0.3.0

### Authentication & security
- Sudo mode: step-up re-authentication via Keycloak for dangerous actions.
- 2FA via TOTP, custom Keycloak browser flow.
- Forgot-password flow enabled on the Keycloak login page.
- Invalid OIDC state redirects to login instead of showing an error.
- Persist SSH host keys on the data volume across container restarts.

### Automations
- Full automation system: triggers (`pre_receive`, `post_receive`, `changeset_opened`, `changeset_merged`, `manual`), per-repo UI, runner CLI, and a runner container image.
- gRPC runner service.

### Workflows
- YAML workflow files at `.cave/workflows/*.yml` parsed and scheduled on push.
- Per-job and per-step timeouts.
- `runs-on` label matching for jobs.
- `continue-on-error` for both jobs and steps.
- Live log streaming via Server-Sent Events.

### Chamber (git storage)
- New gRPC service that owns git operations for cave (`chamber.lisp`).
- All web routes rewired to go through Chamber.
- SSH push lock-bracket via Chamber for write coordination.
- Multi-chamber router (Praefect-style) routes reads across replicas and writes to the primary.
- Streaming `ReceivePack`/`UploadPack` RPCs.
- Per-repo write semaphores for cross-thread coordination.

### Mirroring & forks
- Push and pull repo mirroring (GitHub / GitLab / Codeberg).
- Repo forking from any repo you can read.
- Repo import from URL at creation time.

### Search
- Zoekt code search integration, built from source.
- GitHub-style result cards with context lines, click-through to the blob with Monaco line position and the find widget pre-filled.

### Themes
- Built-in themes (Terminal Warmth, Solarized Dark, Nord, Dracula, Light).
- Custom themes via the `cave-themes` repo with TOML files; auto-lints invalid values and files issues on the themes repo.

### UX
- Squash merge.
- Activity feed.
- Org member management UI.
- Webhooks + commit status API for external CI.

---

## v0.2.0

(Foundational release. Repos, issues, PRs with graduated review, stacked changesets, basic Keycloak SSO, Postgres-backed storage. See `git log v0.1.0..v0.2.0` for the full set.)

---

## v0.1.0

*Poof*. Created.
