package quadlet

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"moxielogic.com/cave/internal/cavectl/config"
)

// Supported returns true if quadlet/systemd is available (Linux only).
func Supported() bool {
	return runtime.GOOS == "linux"
}

// QuadletDir returns the user quadlet directory.
func QuadletDir() string {
	configDir := os.Getenv("XDG_CONFIG_HOME")
	if configDir == "" {
		home, _ := os.UserHomeDir()
		configDir = filepath.Join(home, ".config")
	}
	return filepath.Join(configDir, "containers", "systemd")
}

// Install generates and writes quadlet unit files, then reloads systemd.
func Install(cfg *config.Config) error {
	dir := QuadletDir()
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("creating quadlet dir: %w", err)
	}

	units := generate(cfg)
	for name, content := range units {
		path := filepath.Join(dir, name)
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			return fmt.Errorf("writing %s: %w", name, err)
		}
	}

	// Reload systemd to pick up new units
	if err := exec.Command("systemctl", "--user", "daemon-reload").Run(); err != nil {
		return fmt.Errorf("systemctl daemon-reload: %w", err)
	}

	// Without linger, the user systemd manager (and every --user quadlet
	// container under it) stops when the last login session ends. The
	// deployment would silently die whenever the deploying user logged out.
	// Best-effort: polkit usually lets a user enable their own linger; if
	// it's denied, warn but don't fail the whole install.
	if out, err := exec.Command("loginctl", "enable-linger").CombinedOutput(); err != nil {
		fmt.Fprintf(os.Stderr, "  Warning: loginctl enable-linger failed (%v): %s\n", err, strings.TrimSpace(string(out)))
		fmt.Fprintln(os.Stderr, "  Deployment will stop when this user logs out. Fix with: sudo loginctl enable-linger $USER")
	}

	return nil
}

// Enable starts the Cave service via systemd.
func Enable(cfg *config.Config) error {
	prefix := cfg.Runtime.Prefix
	services := []string{prefix + "-pg", prefix}
	if cfg.Zoekt.Enabled {
		services = append(services, prefix+"-zoekt-web")
	}
	if cfg.Runner.Enabled {
		for i := 0; i < cfg.Runner.Count; i++ {
			if i == 0 {
				services = append(services, prefix+"-runner")
			} else {
				services = append(services, fmt.Sprintf("%s-runner-%d", prefix, i+1))
			}
		}
	}

	args := append([]string{"--user", "start"}, services...)
	if err := exec.Command("systemctl", args...).Run(); err != nil {
		return fmt.Errorf("systemctl start: %w", err)
	}
	return nil
}

// Uninstall removes quadlet files and reloads systemd.
func Uninstall(cfg *config.Config) error {
	dir := QuadletDir()
	prefix := cfg.Runtime.Prefix

	entries, _ := os.ReadDir(dir)
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), prefix) {
			os.Remove(filepath.Join(dir, e.Name()))
		}
	}

	exec.Command("systemctl", "--user", "daemon-reload").Run()
	return nil
}

func generate(cfg *config.Config) map[string]string {
	prefix := cfg.Runtime.Prefix
	units := make(map[string]string)

	// Network
	units[prefix+".network"] = fmt.Sprintf(`[Network]
NetworkName=%s
`, cfg.Runtime.Network)

	// Volumes
	units[prefix+"-data.volume"] = fmt.Sprintf(`[Volume]
VolumeName=%s
`, cfg.VolumeName("data"))

	if cfg.Database.Mode == "local" {
		units[prefix+"-pgdata.volume"] = fmt.Sprintf(`[Volume]
VolumeName=%s
`, cfg.VolumeName("pgdata"))
	}

	if cfg.Zoekt.Enabled {
		units[prefix+"-zoekt.volume"] = fmt.Sprintf(`[Volume]
VolumeName=%s
`, cfg.VolumeName("zoekt"))
	}

	// Postgres (local mode)
	if cfg.Database.Mode == "local" {
		units[prefix+"-pg.container"] = fmt.Sprintf(`[Unit]
Description=Cave PostgreSQL (%s)

[Container]
ContainerName=%s
Image=%s
Network=%s.network
PodmanArgs=--no-hosts
Volume=%s-pgdata.volume:/var/lib/postgresql/data
Environment=POSTGRES_USER=cave
Environment=POSTGRES_PASSWORD=%s
Environment=POSTGRES_DB=cave
HealthCmd=pg_isready -U cave
HealthInterval=2s
Label=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always

[Install]
WantedBy=default.target
`, prefix, cfg.ContainerName("pg"), cfg.Database.Image,
			prefix, prefix, cfg.Database.Password, prefix)
	}

	// Cave
	var caveAfter, caveRequires string
	if cfg.Database.Mode == "local" {
		caveAfter = fmt.Sprintf("After=%s-pg.service", prefix)
		caveRequires = fmt.Sprintf("Requires=%s-pg.service", prefix)
	}

	var envLines strings.Builder
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_DB_HOST=%s\n", cfg.DBHost()))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_DB_PORT=%s\n", cfg.DBPort()))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_DB_NAME=%s\n", cfg.DBName()))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_DB_USER=%s\n", cfg.DBUser()))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_DB_PASSWORD=%s\n", cfg.DBPassword()))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_BASE_URL=%s\n", cfg.Cave.BaseURL))
	envLines.WriteString(fmt.Sprintf("Environment=CAVE_SECRET_KEY=%s\n", cfg.Cave.SecretKey))
	envLines.WriteString("Environment=CAVE_CHAMBER_ENABLED=t\n")
	if cfg.Runner.Enabled {
		// Runners clone over the container network, where the public base_url
		// hairpins to loopback and fails. Point them at the cave container's
		// internal HTTP endpoint instead.
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_RUNNER_CLONE_BASE_URL=http://%s:8080\n",
			cfg.ContainerName("cave")))
	}

	if cfg.Zoekt.Enabled {
		envLines.WriteString("Environment=CAVE_ZOEKT_ENABLED=t\n")
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_ZOEKT_WEB_URL=http://%s:6070\n", cfg.ContainerName("zoekt-web")))
	}

	if cfg.Auth.Mode == "local" {
		// Embedded Usher: cave hosts its own OIDC provider. Browser reaches it
		// at the public base URL; cave calls its own in-container HTTP port.
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_ISSUER=%s\n", cfg.Cave.BaseURL))
		envLines.WriteString("Environment=CAVE_OIDC_ISSUER_INTERNAL=http://localhost:8080\n")
		envLines.WriteString("Environment=CAVE_OIDC_CLIENT_ID=cave\n")
		if cfg.Auth.OIDC.ClientSecret != "" {
			envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_CLIENT_SECRET=%s\n", cfg.Auth.OIDC.ClientSecret))
		}
	} else if cfg.Auth.Mode == "oidc" {
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_ISSUER=%s\n", cfg.Auth.OIDC.Issuer))
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_ISSUER_INTERNAL=%s\n", cfg.Auth.OIDC.Issuer))
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_CLIENT_ID=%s\n", cfg.Auth.OIDC.ClientID))
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_CLIENT_SECRET=%s\n", cfg.Auth.OIDC.ClientSecret))
	}

	var volumeLines strings.Builder
	volumeLines.WriteString(fmt.Sprintf("Volume=%s-data.volume:/var/lib/cave\n", prefix))
	if cfg.Zoekt.Enabled {
		volumeLines.WriteString(fmt.Sprintf("Volume=%s-zoekt.volume:/data/zoekt-index\n", prefix))
	}

	sshBind := cfg.Ports.SSHBind
	if sshBind == "" {
		sshBind = "127.0.0.1"
	}
	grpcBind := cfg.Ports.GRPCBind
	if grpcBind == "" {
		grpcBind = "127.0.0.1"
	}
	var grpcPublishLine string
	if cfg.Ports.GRPC > 0 {
		grpcPublishLine = fmt.Sprintf("PublishPort=%s:%d:9443\n", grpcBind, cfg.Ports.GRPC)
	}

	units[prefix+".container"] = fmt.Sprintf(`[Unit]
Description=Cave Code Forge (%s)
%s
%s

[Container]
ContainerName=%s
Image=%s
Network=%s.network
PodmanArgs=--no-hosts
PublishPort=127.0.0.1:%d:8080
PublishPort=%s:%d:22
%s%s%sLabel=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always

[Install]
WantedBy=default.target
`, prefix, caveAfter, caveRequires,
		cfg.ContainerName("cave"), cfg.Cave.Image, prefix,
		cfg.Ports.HTTP, sshBind, cfg.Ports.SSH,
		grpcPublishLine, envLines.String(), volumeLines.String(), prefix)

	// Zoekt
	if cfg.Zoekt.Enabled {
		units[prefix+"-zoekt-web.container"] = fmt.Sprintf(`[Unit]
Description=Cave Zoekt (%s)

[Container]
ContainerName=%s
Image=%s
Network=%s.network
PodmanArgs=--no-hosts
Volume=%s-zoekt.volume:/data/index
Volume=%s-data.volume:/var/lib/cave:ro
Exec=-rpc -listen :6070 -index /data/index
Label=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always

[Install]
WantedBy=default.target
`, prefix, cfg.ContainerName("zoekt-web"), cfg.Zoekt.Image,
			prefix, prefix, prefix, prefix)
	}

	// Runners
	if cfg.Runner.Enabled {
		for i := 0; i < cfg.Runner.Count; i++ {
			svc := "runner"
			if i > 0 {
				svc = fmt.Sprintf("runner-%d", i+1)
			}
			containerName := cfg.ContainerName(svc)
			caveName := cfg.ContainerName("cave")
			grpcURL := fmt.Sprintf("grpc://%s:9443", caveName)
			// Registration tokens are single-use and the runner re-registers on
			// every (re)start, so a static token would only survive one start.
			// Mint a fresh token via the cave container (which holds the DB
			// credentials) before each start and hand it to the runner through a
			// bind-mounted file. %t is the systemd user runtime dir.
			tokenDir := fmt.Sprintf("%%t/%s", containerName)

			units[prefix+"-"+svc+".container"] = fmt.Sprintf(`[Unit]
Description=Cave Runner %s (%s)
After=%s.service
Requires=%s.service

[Container]
ContainerName=%s
Image=%s
Network=%s.network
PodmanArgs=--no-hosts
# Rootless podman-in-podman for workflow jobs (least privilege — no --privileged).
# The image (quay.io/podman/stable based) handles the userns/subuid/caps setup;
# the runner container just needs /dev/fuse for fuse-overlayfs and SELinux label
# separation disabled so the nested store can be mounted on enforcing hosts.
AddDevice=/dev/fuse
SecurityLabelDisable=true
Volume=%s:/run/runner:ro,z
Exec=cave-server runner --url %s --name %s --token-file /run/runner/token
Label=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always
ExecStartPre=/usr/bin/install -d -m 755 %s
ExecStartPre=/bin/sh -c '/usr/bin/podman exec %s cave-server runner-token --quiet --config /etc/cave.conf > %s/token'

[Install]
WantedBy=default.target
`, svc, prefix, prefix, prefix,
				containerName, cfg.Runner.Image, prefix,
				tokenDir,
				grpcURL, containerName, prefix,
				tokenDir, caveName, tokenDir)
		}
	}

	return units
}
