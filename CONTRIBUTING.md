# Contributing to Cave

Thanks for your interest in contributing to Cave.

## Getting started

```bash
# Build
make

# Start the dev environment (Cave + Postgres + Keycloak + Mailpit)
make podman-up

# Open Cave at http://localhost:8080
# Keycloak admin at http://localhost:8180 (admin/admin)
```

See the [README](README.md) for full setup instructions.

## Development workflow

1. Create a branch for your change.
2. Make your changes. Run `make load` to verify compilation.
3. Run `make test` if you've changed behavior covered by Playwright tests.
4. Push and open a pull request on the Cave instance.

## Code conventions

- **Language**: Common Lisp (SBCL). Use `/usr/bin/sbcl`, not linuxbrew, so binaries work in containers.
- **HTML**: Generated with Spinneret (s-expression HTML). All views are functions in `src/views.lisp`. No template files.
- **Database**: PostgreSQL via Postmodern's s-sql. Migrations are `(version . sql-string)` pairs in `src/db.lisp`.
- **Routes**: Hunchentoot + easy-routes. Repos use `/:owner/:repo-name` for unified user/org access.
- **Logging**: `llog` (structured logging).
- **HTTP client**: Dexador (`dex:get`, `dex:post`).
- **CSS**: Single file at `static/css/cave.css`. No CSS frameworks or preprocessors.
- **JavaScript**: Minimal. Monaco editor loaded from CDN for file viewing. No JS build step.

## What to work on

Check the issue tracker on the Cave instance for open issues. If you're unsure whether a change would be welcome, open an issue first to discuss.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
