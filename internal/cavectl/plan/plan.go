package plan

import (
	"crypto/sha256"
	"fmt"
	"path/filepath"
	"sort"
	"strings"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/runtime"
	"moxielogic.com/cave/internal/cavectl/state"
)

// ActionType identifies the kind of action to perform.
type ActionType int

const (
	CreateNetwork ActionType = iota
	CreateVolume
	CreateContainer
	UpdateContainer
	RemoveContainer
	WaitForHealthy
	GenerateConfig
	MigrateDatabase
	ConfigureKeycloak
	CreateKeycloakDB
	CreateRunnerToken
	RestoreBackup
)

// Action represents a single step in the apply plan.
type Action struct {
	Type        ActionType
	Service     string
	Description string
	Details     interface{}
}

func (a Action) Symbol() string {
	switch a.Type {
	case CreateNetwork, CreateVolume, CreateContainer, GenerateConfig, ConfigureKeycloak, CreateKeycloakDB, CreateRunnerToken, RestoreBackup:
		return "+"
	case UpdateContainer:
		return "~"
	case RemoveContainer:
		return "-"
	case WaitForHealthy:
		return "…"
	case MigrateDatabase:
		return "⇄"
	default:
		return "?"
	}
}

// Diff compares desired config against current state and returns an ordered action list.
// If backupPath is non-empty, a RestoreBackup action is inserted after postgres is healthy.
func Diff(cfg *config.Config, current *state.DeploymentState, backupPath ...string) []Action {
	backup := ""
	if len(backupPath) > 0 {
		backup = backupPath[0]
	}
	var actions []Action

	// 1. Network
	if !current.IsDeployed() {
		actions = append(actions, Action{
			Type:        CreateNetwork,
			Service:     "network",
			Description: fmt.Sprintf("create network %q", cfg.Runtime.Network),
		})
	}

	// 2. Volumes
	requiredVolumes := map[string]string{
		cfg.VolumeName("data"): "data",
	}
	if cfg.Database.Mode == "local" {
		requiredVolumes[cfg.VolumeName("pgdata")] = "pgdata"
	}
	if cfg.Zoekt.Enabled {
		requiredVolumes[cfg.VolumeName("zoekt")] = "zoekt"
	}

	// Sort for deterministic output
	var volNames []string
	for v := range requiredVolumes {
		volNames = append(volNames, v)
	}
	sort.Strings(volNames)
	for _, v := range volNames {
		if !current.Volumes[v] {
			actions = append(actions, Action{
				Type:        CreateVolume,
				Service:     "volume",
				Description: fmt.Sprintf("create volume %q", v),
			})
		}
	}

	// 3. Postgres (if local)
	if cfg.Database.Mode == "local" {
		pgInfo := current.Containers["pg"]
		if pgInfo == nil || pgInfo.Status == "not-found" {
			actions = append(actions, Action{
				Type:        CreateContainer,
				Service:     "pg",
				Description: fmt.Sprintf("create container %q (%s)", cfg.ContainerName("pg"), cfg.Database.Image),
			})
			actions = append(actions, Action{
				Type:        WaitForHealthy,
				Service:     "pg",
				Description: fmt.Sprintf("wait for %q healthy", cfg.ContainerName("pg")),
			})
		} else if needsUpdate(pgInfo, cfg.Database.Image) {
			actions = append(actions, Action{
				Type:        UpdateContainer,
				Service:     "pg",
				Description: fmt.Sprintf("update container %q (%s)", cfg.ContainerName("pg"), cfg.Database.Image),
			})
			actions = append(actions, Action{
				Type:        WaitForHealthy,
				Service:     "pg",
				Description: fmt.Sprintf("wait for %q healthy", cfg.ContainerName("pg")),
			})
		}
	}

	// 3b. Restore backup (if provided) — after postgres is healthy, before Cave starts
	if backup != "" {
		actions = append(actions, Action{
			Type:        RestoreBackup,
			Service:     "backup",
			Description: fmt.Sprintf("restore from backup %s", filepath.Base(backup)),
			Details:     backup,
		})
	}

	// 4. Keycloak
	kcInfo := current.Containers["keycloak"]
	if cfg.KeycloakEnabled() {
		if kcInfo == nil || kcInfo.Status == "not-found" {
			// keycloak needs its own DB; postgres only created "cave"
			actions = append(actions, Action{
				Type:        CreateKeycloakDB,
				Service:     "keycloak",
				Description: "ensure keycloak database exists",
			})
			actions = append(actions, Action{
				Type:        CreateContainer,
				Service:     "keycloak",
				Description: fmt.Sprintf("create container %q (%s)", cfg.ContainerName("keycloak"), cfg.Auth.Keycloak.Image),
			})
			actions = append(actions, Action{
				Type:        WaitForHealthy,
				Service:     "keycloak",
				Description: fmt.Sprintf("wait for %q healthy", cfg.ContainerName("keycloak")),
			})
			actions = append(actions, Action{
				Type:        ConfigureKeycloak,
				Service:     "keycloak",
				Description: "configure Keycloak realm",
			})
		} else if needsUpdate(kcInfo, cfg.Auth.Keycloak.Image) {
			actions = append(actions, Action{
				Type:        UpdateContainer,
				Service:     "keycloak",
				Description: fmt.Sprintf("update container %q", cfg.ContainerName("keycloak")),
			})
		}
	} else if kcInfo != nil && kcInfo.Status != "not-found" {
		actions = append(actions, Action{
			Type:        RemoveContainer,
			Service:     "keycloak",
			Description: fmt.Sprintf("remove container %q (auth mode changed)", cfg.ContainerName("keycloak")),
		})
	}

	// 5. Zoekt
	zoektInfo := current.Containers["zoekt-web"]
	if cfg.Zoekt.Enabled {
		if zoektInfo == nil || zoektInfo.Status == "not-found" {
			actions = append(actions, Action{
				Type:        CreateContainer,
				Service:     "zoekt-web",
				Description: fmt.Sprintf("create container %q (%s)", cfg.ContainerName("zoekt-web"), cfg.Zoekt.Image),
			})
		} else if needsUpdate(zoektInfo, cfg.Zoekt.Image) {
			actions = append(actions, Action{
				Type:        UpdateContainer,
				Service:     "zoekt-web",
				Description: fmt.Sprintf("update container %q", cfg.ContainerName("zoekt-web")),
			})
		}
	} else if zoektInfo != nil && zoektInfo.Status != "not-found" {
		actions = append(actions, Action{
			Type:        RemoveContainer,
			Service:     "zoekt-web",
			Description: fmt.Sprintf("remove container %q (zoekt disabled)", cfg.ContainerName("zoekt-web")),
		})
	}

	// 6. Cave
	caveInfo := current.Containers["cave"]
	if caveInfo == nil || caveInfo.Status == "not-found" {
		actions = append(actions, Action{
			Type:        GenerateConfig,
			Service:     "cave",
			Description: "generate cave.conf",
		})
		actions = append(actions, Action{
			Type:        CreateContainer,
			Service:     "cave",
			Description: fmt.Sprintf("create container %q (%s)", cfg.ContainerName("cave"), cfg.Cave.Image),
		})
	} else if needsUpdate(caveInfo, cfg.Cave.Image) || configChanged(caveInfo, cfg) {
		actions = append(actions, Action{
			Type:        GenerateConfig,
			Service:     "cave",
			Description: "generate cave.conf",
		})
		actions = append(actions, Action{
			Type:        UpdateContainer,
			Service:     "cave",
			Description: fmt.Sprintf("update container %q (%s)", cfg.ContainerName("cave"), cfg.Cave.Image),
		})
	}

	// 7. Runners
	if cfg.Runner.Enabled {
		needsToken := false
		for i := 0; i < cfg.Runner.Count; i++ {
			svc := "runner"
			if i > 0 {
				svc = fmt.Sprintf("runner-%d", i+1)
			}
			rInfo := current.Containers[svc]
			if rInfo == nil || rInfo.Status == "not-found" {
				if !needsToken {
					needsToken = true
					// Wait for Cave to finish migrations before inserting token
					actions = append(actions, Action{
						Type:        WaitForHealthy,
						Service:     "cave",
						Description: fmt.Sprintf("wait for %q ready (migrations)", cfg.ContainerName("cave")),
					})
					actions = append(actions, Action{
						Type:        CreateRunnerToken,
						Service:     "runner",
						Description: "create runner registration token",
					})
				}
				actions = append(actions, Action{
					Type:        CreateContainer,
					Service:     svc,
					Description: fmt.Sprintf("create container %q (%s)", cfg.ContainerName(svc), cfg.Runner.Image),
				})
			} else if needsUpdate(rInfo, cfg.Runner.Image) {
				actions = append(actions, Action{
					Type:        UpdateContainer,
					Service:     svc,
					Description: fmt.Sprintf("update container %q", cfg.ContainerName(svc)),
				})
			}
		}
	}

	return actions
}

// PrintPlan displays the action list in a terraform-style format.
func PrintPlan(actions []Action) {
	if len(actions) == 0 {
		fmt.Println("No changes needed. Deployment matches desired state.")
		return
	}

	fmt.Println("cavectl will perform the following actions:")
	fmt.Println()
	for _, a := range actions {
		fmt.Printf("  %s %s\n", a.Symbol(), a.Description)
	}
	fmt.Println()
}

func needsUpdate(info *runtime.ContainerInfo, desiredImage string) bool {
	if info.Status == "not-found" {
		return false
	}
	return info.Image != desiredImage
}

func configChanged(info *runtime.ContainerInfo, cfg *config.Config) bool {
	if info.Labels == nil {
		return true // no label = first deploy or pre-cavectl
	}
	currentHash := info.Labels["cave.config-hash"]
	if currentHash == "" {
		return true
	}
	return currentHash != ConfigHash(cfg)
}

// ConfigHash produces a deterministic hash of the config values that affect the cave container.
func ConfigHash(cfg *config.Config) string {
	h := sha256.New()
	fmt.Fprintf(h, "image=%s\n", cfg.Cave.Image)
	fmt.Fprintf(h, "base_url=%s\n", cfg.Cave.BaseURL)
	fmt.Fprintf(h, "http=%d\n", cfg.Ports.HTTP)
	fmt.Fprintf(h, "ssh=%d\n", cfg.Ports.SSH)
	fmt.Fprintf(h, "db_mode=%s\n", cfg.Database.Mode)
	fmt.Fprintf(h, "db_url=%s\n", cfg.Database.URL)
	fmt.Fprintf(h, "auth_mode=%s\n", cfg.Auth.Mode)
	fmt.Fprintf(h, "zoekt=%v\n", cfg.Zoekt.Enabled)
	fmt.Fprintf(h, "chamber_nodes=%d\n", len(cfg.Chamber.Nodes))
	return fmt.Sprintf("%x", h.Sum(nil))[:16]
}

// Confirm asks for user confirmation. Returns true if approved.
func Confirm() bool {
	fmt.Print("Proceed? [y/N] ")
	var answer string
	fmt.Scanln(&answer)
	return strings.ToLower(strings.TrimSpace(answer)) == "y"
}
