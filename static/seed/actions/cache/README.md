# actions/cache

Cache files (dependencies, build outputs) between workflow runs to speed up jobs.

cave-native and compatible with GitHub `cache@v4`: the step **restores** a cache
by `key` (falling back to `restore-keys` prefixes), and **saves** at job end (a
post step) unless the exact `key` already hit.

```yaml
steps:
  - uses: actions/checkout@v4
  - uses: actions/cache@v4
    with:
      path: |
        ~/.npm
        node_modules
      key: ${{ runner.os }}-npm-${{ hashFiles('**/package-lock.json') }}
      restore-keys: |
        ${{ runner.os }}-npm-
  - run: npm ci && npm test
```

## Inputs

| Input | Required | Description |
|-------|----------|-------------|
| `path` | yes | Files/dirs to cache, one per line. |
| `key` | yes | Exact cache key (often includes `hashFiles(...)`). |
| `restore-keys` | no | Ordered prefixes to fall back to on a miss. |
| `fail-on-cache-miss` | no | Fail the step if the exact key isn't found. |
| `lookup-only` | no | Check for a hit without restoring or saving. |

## Outputs

| Output | Description |
|--------|-------------|
| `cache-hit` | `true` when an exact match for `key` was found. |

## Notes

The store is **repo-scoped** and shared across job containers on the same runner
host. Keys are immutable: once a key is saved it isn't overwritten. A restore-key
match restores the newest matching entry but still reports `cache-hit: false`, so
the job saves a fresh entry under the exact `key`.
