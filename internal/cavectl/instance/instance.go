package instance

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"net"
	"os/exec"
	"sort"
	"strings"
)

// Instance represents a discovered Cave deployment on this host.
type Instance struct {
	Name     string
	Status   string // "running", "stopped", "partial"
	HTTPPort int
	SSHPort  int
	URL      string
	Services []ServiceInfo
}

// ServiceInfo is a running container belonging to an instance.
type ServiceInfo struct {
	Name   string
	Image  string
	Status string
}

// RandomPassword generates a random alphanumeric password of the given length.
func RandomPassword(length int) string {
	b := make([]byte, length)
	rand.Read(b)
	return hex.EncodeToString(b)[:length]
}

// RandomSecretKey generates a 64-char hex secret key.
func RandomSecretKey() string {
	b := make([]byte, 32)
	rand.Read(b)
	return hex.EncodeToString(b)
}

// PortAvailable checks if a TCP port is free on localhost.
func PortAvailable(port int) bool {
	ln, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%d", port))
	if err != nil {
		return false
	}
	ln.Close()
	return true
}

// FindFreePorts finds N consecutive-ish free ports starting from the given base.
// Returns a slice of free ports. Each port is searched independently starting from its base.
func FindFreePorts(bases []int) []int {
	result := make([]int, len(bases))
	used := make(map[int]bool)
	for i, base := range bases {
		port := base
		for !PortAvailable(port) || used[port] {
			port++
			if port > base+100 {
				// Give up, just use the base and let it fail at bind time
				port = base
				break
			}
		}
		result[i] = port
		used[port] = true
	}
	return result
}

// Discover finds all Cave instances on this host by scanning containers
// with the "cave.managed-by=cavectl" label.
func Discover(runtimeBin string) ([]Instance, error) {
	if runtimeBin == "" {
		// Try podman first, then docker
		if path, err := exec.LookPath("podman"); err == nil {
			runtimeBin = path
		} else if path, err := exec.LookPath("docker"); err == nil {
			runtimeBin = path
		} else {
			return nil, fmt.Errorf("neither podman nor docker found")
		}
	}

	// List all containers with our label
	out, err := exec.Command(runtimeBin, "ps", "-a",
		"--filter", "label=cave.managed-by=cavectl",
		"--format", "json").CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("listing containers: %w", err)
	}

	outStr := strings.TrimSpace(string(out))
	if outStr == "" || outStr == "[]" {
		return nil, nil
	}

	// Parse container list — podman outputs one JSON object per line, docker outputs an array
	var containers []containerEntry
	if strings.HasPrefix(outStr, "[") {
		json.Unmarshal([]byte(outStr), &containers)
	} else {
		// Podman: one JSON per line
		for _, line := range strings.Split(outStr, "\n") {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			var c containerEntry
			if json.Unmarshal([]byte(line), &c) == nil {
				containers = append(containers, c)
			}
		}
	}

	// Group by prefix (instance name)
	instances := make(map[string]*Instance)
	for _, c := range containers {
		prefix := extractPrefix(c.Names, c.Labels)
		if prefix == "" {
			continue
		}

		inst, ok := instances[prefix]
		if !ok {
			inst = &Instance{Name: prefix, Status: "stopped"}
			instances[prefix] = inst
		}

		svc := ServiceInfo{
			Name:   c.containerName(),
			Image:  c.Image,
			Status: c.State,
		}
		inst.Services = append(inst.Services, svc)

		if c.State == "running" {
			inst.Status = "running"
		}

		// Extract ports from the main cave container (not -pg, -zoekt-web, etc.)
		if c.containerName() == prefix {
			inst.HTTPPort = extractHostPort(c.Ports, 8080)
			inst.SSHPort = extractHostPort(c.Ports, 22)
			if inst.HTTPPort > 0 {
				inst.URL = fmt.Sprintf("http://localhost:%d", inst.HTTPPort)
			}
		}
	}

	var result []Instance
	for _, inst := range instances {
		// Check if partially running
		running := 0
		for _, s := range inst.Services {
			if s.Status == "running" {
				running++
			}
		}
		if running > 0 && running < len(inst.Services) {
			inst.Status = "partial"
		}
		result = append(result, *inst)
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Name < result[j].Name
	})

	return result, nil
}

// NameInUse checks if an instance name is already taken.
func NameInUse(runtimeBin, name string) bool {
	instances, err := Discover(runtimeBin)
	if err != nil {
		return false
	}
	for _, inst := range instances {
		if inst.Name == name {
			return true
		}
	}
	return false
}

type containerEntry struct {
	Names  interface{}       `json:"Names"`  // string (podman) or []string (docker)
	Image  string            `json:"Image"`
	State  string            `json:"State"`
	Labels map[string]string `json:"Labels"`
	Ports  interface{}       `json:"Ports"` // varies between podman/docker
}

func (c *containerEntry) containerName() string {
	switch v := c.Names.(type) {
	case string:
		return strings.TrimSpace(v)
	case []interface{}:
		if len(v) > 0 {
			if s, ok := v[0].(string); ok {
				return strings.TrimPrefix(s, "/")
			}
		}
	}
	return ""
}

func extractPrefix(names interface{}, labels map[string]string) string {
	// First try the label
	if p, ok := labels["cave.instance"]; ok {
		return p
	}
	// Fall back to stripping known suffixes from container name
	name := ""
	switch v := names.(type) {
	case string:
		name = strings.TrimSpace(v)
	case []interface{}:
		if len(v) > 0 {
			if s, ok := v[0].(string); ok {
				name = strings.TrimPrefix(s, "/")
			}
		}
	}
	for _, suffix := range []string{"-pg", "-zoekt-web", "-mailpit", "-init"} {
		if strings.HasSuffix(name, suffix) {
			return strings.TrimSuffix(name, suffix)
		}
	}
	return name
}

func extractHostPort(ports interface{}, containerPort int) int {
	// Podman format: []{"host_ip":"127.0.0.1","container_port":8080,"host_port":9080,...}
	// Docker format: similar but field names may differ
	switch v := ports.(type) {
	case []interface{}:
		for _, p := range v {
			if m, ok := p.(map[string]interface{}); ok {
				cp := toInt(m["container_port"])
				if cp == 0 {
					cp = toInt(m["PrivatePort"])
				}
				if cp == containerPort {
					hp := toInt(m["host_port"])
					if hp == 0 {
						hp = toInt(m["PublicPort"])
					}
					return hp
				}
			}
		}
	}
	return 0
}

func toInt(v interface{}) int {
	switch n := v.(type) {
	case float64:
		return int(n)
	case int:
		return n
	case int64:
		return int(n)
	}
	return 0
}
