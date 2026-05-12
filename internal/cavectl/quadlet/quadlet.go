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

	return nil
}

// Enable starts the Cave service via systemd.
func Enable(cfg *config.Config) error {
	prefix := cfg.Runtime.Prefix
	services := []string{prefix + "-pg", prefix}
	if cfg.KeycloakEnabled() {
		services = append(services, prefix+"-keycloak")
	}
	if cfg.Zoekt.Enabled {
		services = append(services, prefix+"-zoekt-web")
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

	// Keycloak
	if cfg.KeycloakEnabled() {
		units[prefix+"-keycloak.container"] = fmt.Sprintf(`[Unit]
Description=Cave Keycloak (%s)
After=%s-pg.service
Requires=%s-pg.service

[Container]
ContainerName=%s
Image=%s
Network=%s.network
Environment=KC_DB=postgres
Environment=KC_DB_URL=jdbc:postgresql://%s:5432/keycloak
Environment=KC_DB_USERNAME=cave
Environment=KC_DB_PASSWORD=%s
Environment=KEYCLOAK_ADMIN=%s
Environment=KEYCLOAK_ADMIN_PASSWORD=%s
Exec=start-dev
Label=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always

[Install]
WantedBy=default.target
`, prefix, prefix, prefix,
			cfg.ContainerName("keycloak"), cfg.Auth.Keycloak.Image,
			prefix, cfg.ContainerName("pg"), cfg.Database.Password,
			cfg.Auth.Keycloak.AdminUser, cfg.Auth.Keycloak.AdminPassword, prefix)
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

	if cfg.Zoekt.Enabled {
		envLines.WriteString("Environment=CAVE_ZOEKT_ENABLED=t\n")
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_ZOEKT_WEB_URL=http://%s:6070\n", cfg.ContainerName("zoekt-web")))
	}

	if cfg.Auth.Mode == "keycloak" {
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_ISSUER=http://localhost:%d/realms/cave\n", cfg.Ports.HTTP))
		envLines.WriteString(fmt.Sprintf("Environment=CAVE_OIDC_ISSUER_INTERNAL=http://%s:8080/realms/cave\n", cfg.ContainerName("keycloak")))
		envLines.WriteString("Environment=CAVE_OIDC_CLIENT_ID=cave\n")
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

	units[prefix+".container"] = fmt.Sprintf(`[Unit]
Description=Cave Code Forge (%s)
%s
%s

[Container]
ContainerName=%s
Image=%s
Network=%s.network
PublishPort=127.0.0.1:%d:8080
PublishPort=127.0.0.1:%d:22
%s%sLabel=cave.managed-by=cavectl
Label=cave.instance=%s

[Service]
Restart=always

[Install]
WantedBy=default.target
`, prefix, caveAfter, caveRequires,
		cfg.ContainerName("cave"), cfg.Cave.Image, prefix,
		cfg.Ports.HTTP, cfg.Ports.SSH,
		envLines.String(), volumeLines.String(), prefix)

	// Zoekt
	if cfg.Zoekt.Enabled {
		units[prefix+"-zoekt-web.container"] = fmt.Sprintf(`[Unit]
Description=Cave Zoekt (%s)

[Container]
ContainerName=%s
Image=%s
Network=%s.network
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

	return units
}
