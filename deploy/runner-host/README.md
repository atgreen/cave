# Runner host housekeeping

A Cave runner host accumulates container images: every update of
`ghcr.io/atgreen/cave-runner:main` leaves the previous image untagged, and
nothing removes it. At roughly 900MB each that fills a disk in a handful of
deploys. When it does, every job fails at the image pull with an error naming
the registry — `Failed to pull image ghcr.io/...` — while the real cause,
`no space left on device`, is buried in podman's output (cave-pav).

Two defences, and they are independent:

- **The runner refuses to start a job it cannot finish.** Before pulling, it
  checks free space on its work directory and fails the job with a message
  naming the host, the path, and the shortfall — which lands in the job's log,
  not just the runner's. The floor defaults to 2048 MB and is set with
  `CAVE_RUNNER_MIN_FREE_MB`.
- **The host reclaims what it no longer needs.** Install the units here.

## Installing the prune timer

Rootless podman, as the user that runs the runners:

```sh
mkdir -p ~/.config/systemd/user
cp cave-runner-prune.service cave-runner-prune.timer ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now cave-runner-prune.timer
systemctl --user list-timers cave-runner-prune.timer   # confirm it is scheduled
```

If the runners run as root, drop `--user` and use `/etc/systemd/system`.

Enable lingering (`loginctl enable-linger <user>`) if the user's units should
run while they are not logged in — otherwise the timer only fires during a
session.

## What it deliberately does not prune

**Volumes.** The per-repo job caches (`cave-cache-<repo>-<hash>`) live there,
and podman cannot distinguish a cache between jobs from an abandoned one — an
unused-volume prune would delete live caches and quietly slow every build. If a
host is short of space and images are already reclaimed, review volumes by hand:

```sh
podman volume ls --format '{{.Name}}\t{{.CreatedAt}}'
podman system df -v
```
