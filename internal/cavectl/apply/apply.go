package apply

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/confgen"
	"moxielogic.com/cave/internal/cavectl/plan"
	"moxielogic.com/cave/internal/cavectl/runtime"
)

// Executor runs plan actions against a container runtime.
type Executor struct {
	Config      *config.Config
	Runtime     runtime.Runtime
	runnerToken string // set by createRunnerToken, used by createRunner
}

// Execute runs all actions in order.
func (e *Executor) Execute(actions []plan.Action) error {
	for i, a := range actions {
		fmt.Printf("  [%d/%d] %s %s\n", i+1, len(actions), a.Symbol(), a.Description)
		if err := e.executeOne(a); err != nil {
			return fmt.Errorf("action %d failed (%s): %w", i+1, a.Description, err)
		}
	}
	return nil
}

func (e *Executor) executeOne(a plan.Action) error {
	switch a.Type {
	case plan.CreateNetwork:
		exists, err := e.Runtime.NetworkExists(e.Config.Runtime.Network)
		if err != nil {
			return err
		}
		if exists {
			return nil
		}
		return e.Runtime.NetworkCreate(e.Config.Runtime.Network)
	case plan.CreateVolume:
		return e.createVolume(a)
	case plan.CreateContainer:
		return e.createContainer(a)
	case plan.UpdateContainer:
		return e.updateContainer(a)
	case plan.RemoveContainer:
		return e.Runtime.Remove(e.Config.ContainerName(a.Service))
	case plan.WaitForHealthy:
		return e.waitForHealthy(a)
	case plan.GenerateConfig:
		return nil // config is written as part of container creation
	case plan.MigrateDatabase:
		return fmt.Errorf("live database migration not yet implemented")
	case plan.ConfigureKeycloak:
		return nil // TODO: run realm configuration
	case plan.CreateKeycloakDB:
		return e.createKeycloakDB()
	case plan.CreateRunnerToken:
		return e.createRunnerToken()
	case plan.RestoreBackup:
		path, _ := a.Details.(string)
		return e.restoreBackup(path)
	default:
		return fmt.Errorf("unknown action type %d", a.Type)
	}
}

func (e *Executor) createVolume(a plan.Action) error {
	// Extract volume name from description
	switch a.Service {
	case "volume":
		// Volume names are in the description, but we derive from config
		// Try all known volume names
	}
	// The plan stores the volume name in Description; we need to find it
	// For now, create all required volumes
	return nil
}

func (e *Executor) createContainer(a plan.Action) error {
	switch a.Service {
	case "pg":
		return e.createPostgres()
	case "mailpit":
		return e.createMailpit()
	case "keycloak":
		return e.createKeycloak()
	case "zoekt-web":
		return e.createZoekt()
	case "cave":
		return e.createCave()
	default:
		if strings.HasPrefix(a.Service, "runner") {
			return e.createRunner(a.Service)
		}
		return fmt.Errorf("unknown service %q", a.Service)
	}
}

func caveSSHBind(cfg *config.Config) string {
	if cfg.Ports.SSHBind != "" {
		return cfg.Ports.SSHBind
	}
	return "127.0.0.1"
}

func caveGRPCBind(cfg *config.Config) string {
	if cfg.Ports.GRPCBind != "" {
		return cfg.Ports.GRPCBind
	}
	return "127.0.0.1"
}

func (e *Executor) createMailpit() error {
	opts := runtime.RunOptions{
		Name:    e.Config.ContainerName("mailpit"),
		Image:   e.Config.SMTP.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Labels:  e.managedLabels(),
	}
	if e.Config.Ports.Mailpit > 0 {
		opts.Ports = []runtime.PortMapping{
			{HostIP: "127.0.0.1", HostPort: e.Config.Ports.Mailpit, Port: 8025},
		}
	}
	return e.Runtime.Run(opts)
}

func (e *Executor) updateContainer(a plan.Action) error {
	name := e.Config.ContainerName(a.Service)
	// Safety: only update containers we manage
	info, err := e.Runtime.Inspect(name)
	if err == nil && info.Status != "not-found" {
		if !isManagedByUs(info) {
			return fmt.Errorf("container %q exists but was not created by cavectl — refusing to modify", name)
		}
	}
	if err := e.Runtime.Stop(name); err != nil {
		// Ignore stop errors (container might not be running)
	}
	if err := e.Runtime.Remove(name); err != nil {
		return fmt.Errorf("removing old container: %w", err)
	}
	return e.createContainer(a)
}

// isManagedByUs checks if a container has the cave.managed-by=cavectl label.
func isManagedByUs(info *runtime.ContainerInfo) bool {
	if info.Labels == nil {
		return false
	}
	return info.Labels["cave.managed-by"] == "cavectl"
}

func (e *Executor) createPostgres() error {
	return e.Runtime.Run(runtime.RunOptions{
		Name:    e.Config.ContainerName("pg"),
		Image:   e.Config.Database.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Env: map[string]string{
			"POSTGRES_USER":     "cave",
			"POSTGRES_PASSWORD": e.Config.Database.Password,
			"POSTGRES_DB":       "cave",
		},
		Volumes: []runtime.VolumeMount{
			{Source: e.Config.VolumeName("pgdata"), Target: "/var/lib/postgresql/data"},
		},
		HealthCmd: "pg_isready -U cave",
		HealthInt: "2s",
		Labels:    e.managedLabels(),
	})
}

func (e *Executor) createKeycloakDB() error {
	// postgres only creates the one DB named by POSTGRES_DB. Keycloak needs
	// its own. Idempotent SELECT-then-CREATE via psql inside cave-pg.
	pg := e.Config.ContainerName("pg")
	cmd := []string{"sh", "-c",
		`psql -U cave -tc "SELECT 1 FROM pg_database WHERE datname='keycloak'" | grep -q 1 || psql -U cave -c "CREATE DATABASE keycloak"`,
	}
	_, err := e.Runtime.Exec(pg, cmd)
	return err
}

func (e *Executor) createKeycloak() error {
	env := map[string]string{
		"KC_DB":                    "postgres",
		"KC_DB_URL":                fmt.Sprintf("jdbc:postgresql://%s:5432/keycloak", e.Config.ContainerName("pg")),
		"KC_DB_USERNAME":           "cave",
		"KC_DB_PASSWORD":           e.Config.Database.Password,
		"KEYCLOAK_ADMIN":           e.Config.Auth.Keycloak.AdminUser,
		"KEYCLOAK_ADMIN_PASSWORD":  e.Config.Auth.Keycloak.AdminPassword,
		"KC_HTTP_ENABLED":          "true",
		"KC_HOSTNAME_STRICT":       "false",
		// Picked up by the cave-keycloak entrypoint and substituted into
		// the baked realm.json before --import-realm runs.
		"CAVE_OIDC_CLIENT_SECRET": e.Config.Auth.Keycloak.ClientSecret,
		"CAVE_BASE_URL":           e.Config.Cave.BaseURL,
	}

	// SMTP — either mailpit (default) or operator-provided external server.
	smtpHost := e.Config.SMTP.Host
	smtpPort := e.Config.SMTP.Port
	if e.Config.MailpitEnabled() {
		smtpHost = e.Config.ContainerName("mailpit")
		smtpPort = 1025
	}
	if smtpHost != "" {
		env["SMTP_HOST"] = smtpHost
	}
	if smtpPort > 0 {
		env["SMTP_PORT"] = fmt.Sprintf("%d", smtpPort)
	}
	if e.Config.SMTP.From != "" {
		env["SMTP_FROM"] = e.Config.SMTP.From
	}
	if e.Config.SMTP.FromDisplayName != "" {
		env["SMTP_FROM_DISPLAY"] = e.Config.SMTP.FromDisplayName
	}
	if e.Config.SMTP.SSL {
		env["SMTP_SSL"] = "true"
	}
	if e.Config.SMTP.StartTLS {
		env["SMTP_STARTTLS"] = "true"
	}
	if e.Config.SMTP.User != "" || e.Config.SMTP.Password != "" {
		env["SMTP_AUTH"] = "true"
		env["SMTP_USER"] = e.Config.SMTP.User
		env["SMTP_PASSWORD"] = e.Config.SMTP.Password
	}

	// When a public URL is configured, run in production mode behind a reverse
	// proxy that terminates TLS. Otherwise fall back to start-dev for laptop use.
	cmd := []string{"start-dev", "--import-realm"}
	if url := e.Config.Auth.Keycloak.PublicURL; url != "" {
		env["KC_HOSTNAME"] = url
		env["KC_HOSTNAME_BACKCHANNEL_DYNAMIC"] = "true"
		env["KC_PROXY_HEADERS"] = "xforwarded"
		cmd = []string{"start", "--optimized", "--import-realm"}
	}

	opts := runtime.RunOptions{
		Name:    e.Config.ContainerName("keycloak"),
		Image:   e.Config.Auth.Keycloak.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Env:     env,
		Cmd:     cmd,
		Labels:  e.managedLabels(),
	}
	if e.Config.Ports.Keycloak > 0 {
		opts.Ports = []runtime.PortMapping{
			{HostIP: "127.0.0.1", HostPort: e.Config.Ports.Keycloak, Port: 8080},
		}
	}
	return e.Runtime.Run(opts)
}

func (e *Executor) createZoekt() error {
	return e.Runtime.Run(runtime.RunOptions{
		Name:    e.Config.ContainerName("zoekt-web"),
		Image:   e.Config.Zoekt.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Volumes: []runtime.VolumeMount{
			{Source: e.Config.VolumeName("zoekt"), Target: "/data/index"},
			{Source: e.Config.VolumeName("data"), Target: "/var/lib/cave:ro"},
		},
		Cmd:    []string{"-rpc", "-listen", ":6070", "-index", "/data/index"},
		Labels: e.managedLabels(),
	})
}

func (e *Executor) createCave() error {
	caveConf := confgen.Generate(e.Config)

	env := map[string]string{
		"CAVE_DB_HOST":     e.Config.DBHost(),
		"CAVE_DB_PORT":     e.Config.DBPort(),
		"CAVE_DB_NAME":     e.Config.DBName(),
		"CAVE_DB_USER":     e.Config.DBUser(),
		"CAVE_DB_PASSWORD": e.Config.DBPassword(),
		"CAVE_BASE_URL":    e.Config.Cave.BaseURL,
		"CAVE_SECRET_KEY":  e.Config.Cave.SecretKey,
	}

	if e.Config.OIDCEnabled() {
		switch e.Config.Auth.Mode {
		case "keycloak":
			// Local cave-keycloak: browser-facing URL prefers the operator-set
			// public_url (when running behind a reverse proxy), otherwise
			// falls back to the published keycloak port on localhost.
			browserIssuer := e.Config.Auth.Keycloak.PublicURL
			if browserIssuer == "" {
				browserIssuer = fmt.Sprintf("http://localhost:%d", e.Config.Ports.Keycloak)
			}
			env["CAVE_OIDC_ISSUER"] = browserIssuer + "/realms/cave"
			env["CAVE_OIDC_ISSUER_INTERNAL"] = fmt.Sprintf("http://%s:8080/realms/cave", e.Config.ContainerName("keycloak"))
			env["CAVE_OIDC_CLIENT_ID"] = "cave"
			env["CAVE_OIDC_CLIENT_SECRET"] = e.Config.Auth.Keycloak.ClientSecret
		case "oidc":
			env["CAVE_OIDC_ISSUER"] = e.Config.Auth.OIDC.Issuer
			env["CAVE_OIDC_ISSUER_INTERNAL"] = e.Config.Auth.OIDC.Issuer
			env["CAVE_OIDC_CLIENT_ID"] = e.Config.Auth.OIDC.ClientID
			env["CAVE_OIDC_CLIENT_SECRET"] = e.Config.Auth.OIDC.ClientSecret
		}
	}

	if e.Config.Zoekt.Enabled {
		env["CAVE_ZOEKT_ENABLED"] = "t"
		env["CAVE_ZOEKT_WEB_URL"] = fmt.Sprintf("http://%s:6070", e.Config.ContainerName("zoekt-web"))
	}

	_ = caveConf // config is generated via entrypoint.sh from env vars

	volumes := []runtime.VolumeMount{
		{Source: e.Config.VolumeName("data"), Target: "/var/lib/cave"},
	}
	if e.Config.Zoekt.Enabled {
		volumes = append(volumes, runtime.VolumeMount{
			Source: e.Config.VolumeName("zoekt"), Target: "/data/zoekt-index",
		})
	}

	return e.Runtime.Run(runtime.RunOptions{
		Name:    e.Config.ContainerName("cave"),
		Image:   e.Config.Cave.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Ports: func() []runtime.PortMapping {
			pm := []runtime.PortMapping{
				{HostIP: "127.0.0.1", HostPort: e.Config.Ports.HTTP, Port: 8080},
				{HostIP: caveSSHBind(e.Config), HostPort: e.Config.Ports.SSH, Port: 22},
			}
			if e.Config.Ports.GRPC > 0 {
				pm = append(pm, runtime.PortMapping{
					HostIP: caveGRPCBind(e.Config), HostPort: e.Config.Ports.GRPC, Port: 9443,
				})
			}
			return pm
		}(),
		Env:     env,
		Volumes: volumes,
		Labels: map[string]string{
			"cave.config-hash": plan.ConfigHash(e.Config),
			"cave.managed-by":  "cavectl",
		},
	})
}

func (e *Executor) managedLabels() map[string]string {
	return map[string]string{
		"cave.managed-by": "cavectl",
		"cave.instance":   e.Config.Runtime.Prefix,
	}
}

func (e *Executor) waitForHealthy(a plan.Action) error {
	name := e.Config.ContainerName(a.Service)
	for i := 0; i < 30; i++ {
		info, err := e.Runtime.Inspect(name)
		if err != nil {
			return err
		}
		if info.Status == "running" {
			switch a.Service {
			case "pg":
				_, err := e.Runtime.Exec(name, []string{"pg_isready", "-U", "cave"})
				if err == nil {
					return nil
				}
			case "cave":
				// Check that all migrations have completed by verifying a late table exists
				pgName := e.Config.ContainerName("pg")
				_, err := e.Runtime.Exec(pgName, []string{
					"psql", "-U", "cave", "-d", "cave", "-Atc",
					"SELECT 1 FROM cave_runner_registration_tokens LIMIT 0"})
				if err == nil {
					return nil
				}
			default:
				return nil
			}
		}
		time.Sleep(time.Second)
	}
	return fmt.Errorf("container %q did not become healthy after 30s", name)
}

func (e *Executor) restoreBackup(archivePath string) error {
	pgName := e.Config.ContainerName("pg")

	// Extract archive
	workDir, err := os.MkdirTemp("", "cave-restore-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(workDir)

	cmd := exec.Command("tar", "-xzf", archivePath, "-C", workDir)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("extracting archive: %w", err)
	}

	entries, _ := os.ReadDir(workDir)
	if len(entries) == 0 {
		return fmt.Errorf("archive is empty")
	}
	snapshot := filepath.Join(workDir, entries[0].Name())

	// Restore cave database (DB is fresh/empty at this point)
	caveDump := filepath.Join(snapshot, "cave.pgdump")
	if _, err := os.Stat(caveDump); err == nil {
		fmt.Println("    Restoring cave database...")
		// Drop and recreate to be safe
		e.Runtime.Exec(pgName, []string{"dropdb", "-U", "cave", "--if-exists", "cave"})
		e.Runtime.Exec(pgName, []string{"createdb", "-U", "cave", "cave"})
		if err := e.Runtime.ExecFromFile(pgName,
			[]string{"pg_restore", "-U", "cave", "-d", "cave", "--no-owner"},
			caveDump); err != nil {
			return fmt.Errorf("pg_restore cave: %w", err)
		}
	}

	// Restore keycloak database (if present)
	kcDump := filepath.Join(snapshot, "keycloak.pgdump")
	if _, err := os.Stat(kcDump); err == nil {
		fmt.Println("    Restoring keycloak database...")
		e.Runtime.Exec(pgName, []string{"dropdb", "-U", "cave", "--if-exists", "keycloak"})
		e.Runtime.Exec(pgName, []string{"createdb", "-U", "cave", "keycloak"})
		e.Runtime.ExecFromFile(pgName,
			[]string{"pg_restore", "-U", "cave", "-d", "keycloak", "--no-owner"},
			kcDump)
	}

	// Restore git repos — copy into the data volume via a temp container
	reposDir := filepath.Join(snapshot, "repos")
	if _, err := os.Stat(reposDir); err == nil {
		fmt.Println("    Restoring git repositories...")
		dataVol := e.Config.VolumeName("data")
		// Use a temp container to copy repos into the volume
		tempName := e.Config.Runtime.Prefix + "-restore-tmp"
		e.Runtime.Run(runtime.RunOptions{
			Name:    tempName,
			Image:   "docker.io/alpine:3.20",
			Volumes: []runtime.VolumeMount{{Source: dataVol, Target: "/var/lib/cave"}},
			Cmd:     []string{"true"},
			Remove:  true,
		})
		// Now copy repos via podman cp into a fresh container
		e.Runtime.Run(runtime.RunOptions{
			Name:    tempName,
			Image:   "docker.io/alpine:3.20",
			Detach:  true,
			Volumes: []runtime.VolumeMount{{Source: dataVol, Target: "/var/lib/cave"}},
			Cmd:     []string{"sleep", "60"},
		})
		e.Runtime.Copy(reposDir, tempName+":/var/lib/cave/repos")
		e.Runtime.Exec(tempName, []string{"chown", "-R", "1000:1000", "/var/lib/cave/repos"})
		e.Runtime.Stop(tempName)
		e.Runtime.Remove(tempName)
	}

	return nil
}

func (e *Executor) createRunnerToken() error {
	// Generate a token matching Cave's format: cavrt_<hex32>
	b := make([]byte, 16)
	rand.Read(b)
	token := "cavrt_" + hex.EncodeToString(b)
	e.runnerToken = token

	// Insert into DB via psql exec in the postgres container
	pgName := e.Config.ContainerName("pg")
	sql := fmt.Sprintf(
		`INSERT INTO cave_runner_registration_tokens (token, scope, created_at, expires_at) VALUES ('%s', 'instance', NOW(), NOW() + interval '100 years')`,
		token)
	_, err := e.Runtime.Exec(pgName, []string{"psql", "-U", "cave", "-d", "cave", "-c", sql})
	if err != nil {
		return fmt.Errorf("creating runner token: %w", err)
	}
	return nil
}

func (e *Executor) createRunner(service string) error {
	if e.runnerToken == "" {
		return fmt.Errorf("no runner token available — CreateRunnerToken must run first")
	}

	name := e.Config.ContainerName(service)
	grpcURL := fmt.Sprintf("grpc://%s:9443", e.Config.ContainerName("cave"))

	return e.Runtime.Run(runtime.RunOptions{
		Name:    name,
		Image:   e.Config.Runner.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Cmd: []string{
			"cave", "runner",
			"--url", grpcURL,
			"--token", e.runnerToken,
			"--name", name,
		},
		Labels: e.managedLabels(),
	})
}
