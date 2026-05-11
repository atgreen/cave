# Changelog

## Unreleased

### Added
- **Code search** — Full-text code search powered by Zoekt (sourcegraph/zoekt), with global and repo-scoped modes, context lines, and match highlighting
- **Import from URL** — Clone external repos (GitHub, GitLab, etc.) into Cave with full history
- **Mirror at creation** — Set up pull mirrors during repo creation for ongoing sync
- **Workflow: runs-on labels** — Jobs can target specific runners by label
- **Workflow: per-step timeouts** — Configurable timeout at the job and step level
- **Workflow: continue-on-error** — Steps and jobs can be marked to not block dependents on failure
- **Social login** — GitHub, GitLab, and Google identity providers via Keycloak (env var gated)
- **Monaco theme matching** — Editor theme follows Cave's active light/dark theme

### Fixed
- Password reset no longer redirects to authenticator setup
- Leaked file descriptor in log polling
- Incremental log chunks (max 64KB) instead of full log each cycle
