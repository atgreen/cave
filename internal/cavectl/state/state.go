package state

import (
	"fmt"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/runtime"
)

// DeploymentState represents the current state of a Cave deployment.
type DeploymentState struct {
	Network    string
	Containers map[string]*runtime.ContainerInfo // keyed by service name
	Volumes    map[string]bool                   // keyed by volume name
}

// Read inspects the container runtime to determine the current deployment state.
func Read(cfg *config.Config, rt runtime.Runtime) (*DeploymentState, error) {
	s := &DeploymentState{
		Network:    cfg.Runtime.Network,
		Containers: make(map[string]*runtime.ContainerInfo),
		Volumes:    make(map[string]bool),
	}

	// Check containers
	services := []string{"cave", "pg"}
	if cfg.KeycloakEnabled() {
		services = append(services, "keycloak")
	}
	if cfg.Zoekt.Enabled {
		services = append(services, "zoekt-web")
	}
	if cfg.Runner.Enabled {
		for i := 0; i < cfg.Runner.Count; i++ {
			if i == 0 {
				services = append(services, "runner")
			} else {
				services = append(services, fmt.Sprintf("runner-%d", i+1))
			}
		}
	}

	for _, svc := range services {
		name := cfg.ContainerName(svc)
		info, err := rt.Inspect(name)
		if err != nil {
			return nil, err
		}
		s.Containers[svc] = info
	}

	// Check volumes
	volumeNames := []string{
		cfg.VolumeName("data"),
	}
	if cfg.Database.Mode == "local" {
		volumeNames = append(volumeNames, cfg.VolumeName("pgdata"))
	}
	if cfg.Zoekt.Enabled {
		volumeNames = append(volumeNames, cfg.VolumeName("zoekt"))
	}

	for _, v := range volumeNames {
		exists, err := rt.VolumeExists(v)
		if err != nil {
			return nil, err
		}
		s.Volumes[v] = exists
	}

	return s, nil
}

// IsDeployed returns true if any containers are running.
func (s *DeploymentState) IsDeployed() bool {
	for _, c := range s.Containers {
		if c.Status == "running" || c.Status == "stopped" {
			return true
		}
	}
	return false
}
