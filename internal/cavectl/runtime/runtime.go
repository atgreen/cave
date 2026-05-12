package runtime

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// Runtime abstracts container operations over podman/docker.
type Runtime interface {
	Inspect(name string) (*ContainerInfo, error)
	Run(opts RunOptions) error
	Stop(name string) error
	Remove(name string) error
	Exec(name string, cmd []string) (string, error)
	ExecToFile(name string, cmd []string, outPath string) error
	ExecFromFile(name string, cmd []string, inPath string) error
	Copy(src, dst string) error // "container:/path" or local path
	Logs(name string, follow bool) error
	NetworkExists(name string) (bool, error)
	NetworkCreate(name string) error
	VolumeExists(name string) (bool, error)
	VolumeCreate(name string) error
	VolumeRemove(name string) error
	Name() string
}

type ContainerInfo struct {
	Name   string
	Image  string
	Status string // "running", "stopped", "not-found"
	Ports  []PortMapping
	Env    map[string]string
	Labels map[string]string
}

type PortMapping struct {
	HostIP   string
	HostPort int
	Port     int
	Protocol string
}

type VolumeMount struct {
	Source string
	Target string
}

type RunOptions struct {
	Name       string
	Image      string
	Network    string
	Ports      []PortMapping
	Env        map[string]string
	Volumes    []VolumeMount
	Cmd        []string
	Detach     bool
	Labels     map[string]string
	HealthCmd  string
	HealthInt  string
	Remove     bool // --rm for one-shot containers
}

// Detect probes for podman first, then docker.
func Detect() (Runtime, error) {
	if path, err := exec.LookPath("podman"); err == nil {
		return &containerRuntime{bin: path, name: "podman"}, nil
	}
	if path, err := exec.LookPath("docker"); err == nil {
		return &containerRuntime{bin: path, name: "docker"}, nil
	}
	return nil, fmt.Errorf("neither podman nor docker found in PATH")
}

// ForEngine returns a runtime for the specified engine.
func ForEngine(engine string) (Runtime, error) {
	switch engine {
	case "podman":
		if path, err := exec.LookPath("podman"); err == nil {
			return &containerRuntime{bin: path, name: "podman"}, nil
		}
		return nil, fmt.Errorf("podman not found in PATH")
	case "docker":
		if path, err := exec.LookPath("docker"); err == nil {
			return &containerRuntime{bin: path, name: "docker"}, nil
		}
		return nil, fmt.Errorf("docker not found in PATH")
	case "auto", "":
		return Detect()
	default:
		return nil, fmt.Errorf("unknown engine %q (use \"podman\", \"docker\", or \"auto\")", engine)
	}
}

type containerRuntime struct {
	bin  string
	name string
}

func (r *containerRuntime) Name() string { return r.name }

func (r *containerRuntime) run(args ...string) (string, error) {
	cmd := exec.Command(r.bin, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("%s %s: %w\n%s", r.name, strings.Join(args, " "), err, string(out))
	}
	return strings.TrimSpace(string(out)), nil
}

func (r *containerRuntime) Inspect(name string) (*ContainerInfo, error) {
	out, err := r.run("container", "inspect", "--format", "json", name)
	if err != nil {
		if strings.Contains(err.Error(), "no such") || strings.Contains(err.Error(), "not found") {
			return &ContainerInfo{Name: name, Status: "not-found"}, nil
		}
		return nil, err
	}

	var raw []json.RawMessage
	if err := json.Unmarshal([]byte(out), &raw); err != nil {
		return nil, fmt.Errorf("parsing inspect output: %w", err)
	}
	if len(raw) == 0 {
		return &ContainerInfo{Name: name, Status: "not-found"}, nil
	}

	var parsed struct {
		Name   string `json:"Name"`
		Config struct {
			Image  string            `json:"Image"`
			Env    []string          `json:"Env"`
			Labels map[string]string `json:"Labels"`
		} `json:"Config"`
		State struct {
			Status  string `json:"Status"`
			Running bool   `json:"Running"`
		} `json:"State"`
		Image string `json:"Image"`
	}
	if err := json.Unmarshal(raw[0], &parsed); err != nil {
		return nil, fmt.Errorf("parsing inspect JSON: %w", err)
	}

	status := "stopped"
	if parsed.State.Running {
		status = "running"
	}

	env := make(map[string]string)
	for _, e := range parsed.Config.Env {
		parts := strings.SplitN(e, "=", 2)
		if len(parts) == 2 {
			env[parts[0]] = parts[1]
		}
	}

	image := parsed.Config.Image
	if image == "" {
		image = parsed.Image
	}

	return &ContainerInfo{
		Name:   strings.TrimPrefix(parsed.Name, "/"),
		Image:  image,
		Status: status,
		Env:    env,
		Labels: parsed.Config.Labels,
	}, nil
}

func (r *containerRuntime) Run(opts RunOptions) error {
	args := []string{"run"}
	if opts.Detach {
		args = append(args, "-d")
	}
	if opts.Remove {
		args = append(args, "--rm")
	}
	if opts.Name != "" {
		args = append(args, "--name", opts.Name)
	}
	if opts.Network != "" {
		args = append(args, "--network", opts.Network)
	}
	for _, p := range opts.Ports {
		hostIP := p.HostIP
		if hostIP == "" {
			hostIP = "127.0.0.1"
		}
		args = append(args, "-p", fmt.Sprintf("%s:%d:%d", hostIP, p.HostPort, p.Port))
	}
	for k, v := range opts.Env {
		args = append(args, "-e", fmt.Sprintf("%s=%s", k, v))
	}
	for _, v := range opts.Volumes {
		args = append(args, "-v", fmt.Sprintf("%s:%s", v.Source, v.Target))
	}
	for k, v := range opts.Labels {
		args = append(args, "--label", fmt.Sprintf("%s=%s", k, v))
	}
	if opts.HealthCmd != "" {
		args = append(args, "--health-cmd", opts.HealthCmd)
	}
	if opts.HealthInt != "" {
		args = append(args, "--health-interval", opts.HealthInt)
	}
	args = append(args, opts.Image)
	args = append(args, opts.Cmd...)

	_, err := r.run(args...)
	return err
}

func (r *containerRuntime) Stop(name string) error {
	_, err := r.run("stop", name)
	return err
}

func (r *containerRuntime) Remove(name string) error {
	_, err := r.run("rm", "-f", name)
	return err
}

func (r *containerRuntime) Exec(name string, cmd []string) (string, error) {
	args := append([]string{"exec", name}, cmd...)
	return r.run(args...)
}

func (r *containerRuntime) ExecToFile(name string, cmd []string, outPath string) error {
	args := append([]string{"exec", name}, cmd...)
	c := exec.Command(r.bin, args...)
	f, err := os.Create(outPath)
	if err != nil {
		return err
	}
	defer f.Close()
	c.Stdout = f
	c.Stderr = os.Stderr
	return c.Run()
}

func (r *containerRuntime) ExecFromFile(name string, cmd []string, inPath string) error {
	args := append([]string{"exec", "-i", name}, cmd...)
	c := exec.Command(r.bin, args...)
	f, err := os.Open(inPath)
	if err != nil {
		return err
	}
	defer f.Close()
	c.Stdin = f
	c.Stderr = os.Stderr
	return c.Run()
}

func (r *containerRuntime) Copy(src, dst string) error {
	_, err := r.run("cp", src, dst)
	return err
}

func (r *containerRuntime) Logs(name string, follow bool) error {
	args := []string{"logs"}
	if follow {
		args = append(args, "-f")
	}
	args = append(args, name)
	cmd := exec.Command(r.bin, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (r *containerRuntime) NetworkExists(name string) (bool, error) {
	_, err := r.run("network", "inspect", name)
	if err != nil {
		if strings.Contains(err.Error(), "not found") || strings.Contains(err.Error(), "no such") {
			return false, nil
		}
		return false, err
	}
	return true, nil
}

func (r *containerRuntime) NetworkCreate(name string) error {
	_, err := r.run("network", "create", name)
	return err
}

func (r *containerRuntime) VolumeExists(name string) (bool, error) {
	_, err := r.run("volume", "inspect", name)
	if err != nil {
		if strings.Contains(err.Error(), "not found") || strings.Contains(err.Error(), "no such") {
			return false, nil
		}
		return false, err
	}
	return true, nil
}

func (r *containerRuntime) VolumeCreate(name string) error {
	_, err := r.run("volume", "create", name)
	return err
}

func (r *containerRuntime) VolumeRemove(name string) error {
	_, err := r.run("volume", "rm", name)
	return err
}
