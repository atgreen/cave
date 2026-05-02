# Cave development workflow

Build, test, and iterate on Cave. After making changes:

1. **Compile check**: `make load` to verify everything compiles clean.
2. **Lint**: `make lint` and fix substantive issues (ignore special-name-style for template vars).
3. **Paren check**: Run `python3 -c` with a depth counter on any edited .lisp file — sed/edit surgery can break nesting.
4. **Build**: `make` to produce the binary.
5. **Container test** (if touching SSH, entrypoint, or Containerfile):
   ```
   podman stop cave; podman rm cave
   podman build --no-cache -q -t cave -f Containerfile.local .
   podman run -d --name cave --network cave-net -p 8080:8080 -p 2222:22 -e CAVE_DB_HOST=cave-pg -e CAVE_ADMIN_USER=admin -e CAVE_ADMIN_PASSWORD=admin -v cave-data:/var/lib/cave cave:latest
   podman logs cave
   ```
6. **Test HTTP**: `curl -s http://localhost:8080/login | head -5`
7. **Test SSH push**: Generate key in settings, then `git push ssh://cave@localhost:2222/owner/repo.git`

## Key gotchas

- **Spinneret**: HTML as s-expressions. Use `(:tag.class :attr "val" body)`. No template files.
- **llog**: Writes to stdout by default. `set-root-level` is broken (atgreen/cl-llog#2). In git-shell, cave-shell.sh filters stdout with `grep '/' | tail -1`.
- **postmodern s-sql**: Can't build queries dynamically with `#'sql` — it's a macro. Write separate query forms per case.
- **easy-routes**: Path params are `:name` in URL, auto-bound as variables. No `&path` needed.
- **SBCL image dump**: Use `/usr/bin/sbcl` so binaries have standard linker path for containers.
- **Migrations**: Each is a `(version . sql-string)` pair. Multi-statement SQL must be split on `;` (postmodern limitation).
- **NIL vs :null**: postmodern sends CL NIL as SQL `false`. Use `:null` for SQL NULL.
- **jzon**: Parses `null` as symbol `NULL` (not NIL), arrays as vectors (not lists).
