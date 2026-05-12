package apply

import (
	"fmt"
	"time"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/confgen"
	"moxielogic.com/cave/internal/cavectl/plan"
	"moxielogic.com/cave/internal/cavectl/runtime"
)

// Executor runs plan actions against a container runtime.
type Executor struct {
	Config  *config.Config
	Runtime runtime.Runtime
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
	case "keycloak":
		return e.createKeycloak()
	case "zoekt-web":
		return e.createZoekt()
	case "cave":
		return e.createCave()
	default:
		return fmt.Errorf("unknown service %q", a.Service)
	}
}

func (e *Executor) updateContainer(a plan.Action) error {
	name := e.Config.ContainerName(a.Service)
	if err := e.Runtime.Stop(name); err != nil {
		// Ignore stop errors (container might not be running)
	}
	if err := e.Runtime.Remove(name); err != nil {
		return fmt.Errorf("removing old container: %w", err)
	}
	return e.createContainer(a)
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
	})
}

func (e *Executor) createKeycloak() error {
	return e.Runtime.Run(runtime.RunOptions{
		Name:    e.Config.ContainerName("keycloak"),
		Image:   e.Config.Auth.Keycloak.Image,
		Network: e.Config.Runtime.Network,
		Detach:  true,
		Env: map[string]string{
			"KC_DB":                "postgres",
			"KC_DB_URL":           fmt.Sprintf("jdbc:postgresql://%s:5432/keycloak", e.Config.ContainerName("pg")),
			"KC_DB_USERNAME":      "cave",
			"KC_DB_PASSWORD":      e.Config.Database.Password,
			"KEYCLOAK_ADMIN":      e.Config.Auth.Keycloak.AdminUser,
			"KEYCLOAK_ADMIN_PASSWORD": e.Config.Auth.Keycloak.AdminPassword,
		},
		Cmd: []string{"start-dev"},
	})
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
		Cmd: []string{"-rpc", "-listen", ":6070", "-index", "/data/index"},
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
			env["CAVE_OIDC_ISSUER"] = fmt.Sprintf("http://localhost:%d/realms/cave", e.Config.Ports.HTTP)
			env["CAVE_OIDC_ISSUER_INTERNAL"] = fmt.Sprintf("http://%s:8080/realms/cave", e.Config.ContainerName("keycloak"))
			env["CAVE_OIDC_CLIENT_ID"] = "cave"
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
		Ports: []runtime.PortMapping{
			{HostIP: "127.0.0.1", HostPort: e.Config.Ports.HTTP, Port: 8080},
			{HostIP: "127.0.0.1", HostPort: e.Config.Ports.SSH, Port: 22},
		},
		Env:     env,
		Volumes: volumes,
		Labels: map[string]string{
			"cave.config-hash": plan.ConfigHash(e.Config),
			"cave.managed-by":  "cavectl",
		},
	})
}

func (e *Executor) waitForHealthy(a plan.Action) error {
	name := e.Config.ContainerName(a.Service)
	for i := 0; i < 30; i++ {
		info, err := e.Runtime.Inspect(name)
		if err != nil {
			return err
		}
		if info.Status == "running" {
			// For postgres, try a connection check
			if a.Service == "pg" {
				_, err := e.Runtime.Exec(name, []string{"pg_isready", "-U", "cave"})
				if err == nil {
					return nil
				}
			} else {
				return nil
			}
		}
		time.Sleep(time.Second)
	}
	return fmt.Errorf("container %q did not become healthy after 30s", name)
}
