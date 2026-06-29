# actions/checkout

Check out the workflow repository into the workspace.

cave models GitHub Actions: the workspace starts **empty**, so a job checks out
the repo before doing anything with the source.

```yaml
jobs:
  build:
    image: ghcr.io/atgreen/fedora-cave-builder:latest
    steps:
      - name: Check out the repo
        uses: actions/checkout@v4
      - name: Build
        run: make
```

## Inputs

| Input | Default | Description |
|-------|---------|-------------|
| `ref` | triggering ref | Branch, tag, or SHA to check out. |
| `path` | `.` | Directory under the workspace to check out into. |
| `fetch-depth` | `1` | Commits to fetch; `0` = full history (needed for `git describe --tags`). |
| `submodules` | `false` | `true`, `recursive`, or `false`. |

## Outputs

| Output | Description |
|--------|-------------|
| `ref` | The ref that was checked out. |
| `commit` | The commit SHA that was checked out. |

## Notes

This is a **cave-native** action: it is referenced as `actions/checkout@v4` but
resolved cave-local (no github.com) and implemented in Lisp, running in-process
on the runner. A different `repository:` is not supported yet.
