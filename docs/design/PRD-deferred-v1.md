# PRD Deferred Items (from Codex Review Iterations 1–5)

Items cut from the initial PRD during review to keep launch scope tight.
Enough context is included to pick each up later without re-deriving the
requirements.

## Deferred Product Features

### Self-registration
- Instance-admin-only account creation at launch.
- Self-registration (with optional invite-only gating) is P2.
- Affects onboarding UX and admin workload for growing teams.

### Path-based / code-owner review rules
- Launch has flat per-repo merge rules. No path-based required reviewers.
- Would require a code-owners file and per-path matching engine.
- Relevant when teams grow beyond 5–6 developers.

### Merge strategy options
- Launch is rebase-and-merge only. No squash, no fast-forward.
- Squash is the most commonly requested alternative.

### Prefix-merge for stacks
- Launch requires whole-stack landing. No individual or prefix merge of
  stack members.
- Prefix-merge would allow landing the first N items of a stack while
  the rest continue in review. Adds complexity to the landing algorithm
  and retargeting logic.

### SQLite evaluation mode
- Cut to simplify the data layer. PostgreSQL is the only supported DB.
- SQLite could lower the barrier for single-user evaluation installs.
- Dual-backend maintenance cost was the concern.

### Audit log
- Explicitly a non-goal at launch.
- The `cave_events` instrumentation table captures most events but is
  designed for metrics, not compliance. A proper audit log would need
  immutability, tamper-evidence, and retention policies.

### OIDC / OAuth login
- P2. Launch uses local accounts only.
- Should be designed to coexist with local accounts (not replace).
- Teams migrating from Gitea/Forgejo may already have OIDC configured.

### Container registry
- Launch uses local podman image storage only.
- A registry would enable multi-host deploy and external CI integration.

### Environment promotion (staging/prod)
- Launch supports one environment per repo.
- Promotion model would require per-environment deploy config, secrets,
  and branch-to-environment mapping.

### Health-checked / zero-downtime deploys
- Launch is best-effort process replacement.
- Would require readiness probes, rolling restart, and potentially
  multiple container instances.

### Detailed deploy failure recovery
- Several deploy failure edge cases were identified in review:
  - `podman run` fails after successful `podman stop/rm` → service down
  - Current behavior: attempt to restart previous image; if unavailable,
    service is down with "no rollback available" status.
  - More sophisticated: blue-green with old container kept running until
    new one is healthy.

### Built-in container deployment (PaaS)
- All container build-and-deploy functionality is deferred indefinitely.
- Cave focuses on code review and forge capabilities.
- Deploy pipeline should be handled by external tools (Drone, Woodpecker,
  GitHub Actions, shell scripts, etc.)
- DB schema retains deploy-related columns (cave_deploys, cave_deploy_secrets,
  deploy_enabled, etc.) but they are unused.
- Related deferred items below (container registry, environment promotion,
  health-checked deploys, deploy failure recovery) are all part of this scope.

## Deferred Specification Work

### Comprehensive API contract
- Only issues REST API is specified in P0.
- Changeset/review/deploy/stack APIs are P1 (needed for CLI review).
- Full OpenAPI spec deferred until P1 implementation.

### Sandbox hardening
- Launch sandbox uses cgroups v2 (if available) or ulimit fallback.
- Bubblewrap or seccomp-based sandboxing deferred.
- Nested container builds inside checks are explicitly unsupported.

### Branch rename detection
- A raw Git branch rename (delete + create) closes the old changeset
  and creates a new one. Reviews are lost on the old changeset.
- Smarter rename detection (e.g., by commit history) deferred.

### Email notification deduplication
- No dedup across force-push cycles at launch.
- Could suppress redundant "review requested" notifications when only
  the commit hash changed.

### Event retention policy
- `cave_events` retained indefinitely at launch.
- Rotation / archival policy deferred until storage pressure is observed.
