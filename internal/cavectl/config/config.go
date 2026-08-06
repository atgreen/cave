package config

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"net/url"
	"os"
	"regexp"
	"strings"

	"gopkg.in/yaml.v3"
)

type Config struct {
	APIVersion string          `yaml:"apiVersion"`
	Cave       CaveConfig      `yaml:"cave"`
	Ports      PortsConfig     `yaml:"ports"`
	Database   DatabaseConfig  `yaml:"database"`
	Auth       AuthConfig      `yaml:"auth"`
	Runner     RunnerConfig    `yaml:"runner"`
	Zoekt      ZoektConfig     `yaml:"zoekt"`
	Chamber    ChamberConfig   `yaml:"chamber"`
	SMTP       SMTPConfig      `yaml:"smtp,omitempty"`
	Runtime    RuntimeConfig   `yaml:"runtime"`
}

type CaveConfig struct {
	Image     string `yaml:"image"`
	BaseURL   string `yaml:"base_url"`
	SecretKey string `yaml:"secret_key"`
}

type PortsConfig struct {
	HTTP    int `yaml:"http"`
	SSH     int `yaml:"ssh"`
	Mailpit int `yaml:"mailpit,omitempty"`
	GRPC    int `yaml:"grpc,omitempty"`
	// SSHBind is the host IP that cave's SSH (git-shell) port is published
	// on. Default "127.0.0.1" — fine for laptop. For a public VPS where
	// users push from outside, set to "0.0.0.0" (and open the firewall).
	SSHBind string `yaml:"ssh_bind,omitempty"`
	// GRPCBind is the host IP that cave's gRPC runner service is published
	// on. Default "127.0.0.1". Set to "0.0.0.0" if you want remote runners
	// (e.g. on a laptop) to connect over the internet — note that runner
	// auth is currently plaintext, so keep that to trusted networks.
	GRPCBind string `yaml:"grpc_bind,omitempty"`
}

type SMTPConfig struct {
	// Mode is "mailpit" (spawn a local mailpit container; emails captured,
	// not really sent — fine for dev/testing) or "external" (use the
	// host/port/user/password fields to point at a real SMTP server).
	Mode            string `yaml:"mode"`
	Image           string `yaml:"image,omitempty"` // mailpit image
	Host            string `yaml:"host,omitempty"`
	Port            int    `yaml:"port,omitempty"`
	User            string `yaml:"user,omitempty"`
	Password        string `yaml:"password,omitempty"`
	From            string `yaml:"from,omitempty"`
	FromDisplayName string `yaml:"from_display_name,omitempty"`
	StartTLS        bool   `yaml:"starttls,omitempty"`
	SSL             bool   `yaml:"ssl,omitempty"`
}

type DatabaseConfig struct {
	Mode     string `yaml:"mode"`     // "local" or "external"
	Image    string `yaml:"image"`    // for local mode
	Password string `yaml:"password"` // for local mode
	URL      string `yaml:"url"`      // for external mode
}

type AuthConfig struct {
	Mode string     `yaml:"mode"` // "local" (embedded Usher) or "oidc" (external IdP)
	OIDC OIDCConfig `yaml:"oidc,omitempty"`
}

type OIDCConfig struct {
	Issuer       string `yaml:"issuer"`
	ClientID     string `yaml:"client_id"`
	ClientSecret string `yaml:"client_secret"`
}

type RunnerConfig struct {
	Enabled bool   `yaml:"enabled"`
	Image   string `yaml:"image"`
	Count   int    `yaml:"count"` // number of runner instances
}

type ZoektConfig struct {
	Enabled bool   `yaml:"enabled"`
	Image   string `yaml:"image"`
}

type ChamberConfig struct {
	Nodes []ChamberNode `yaml:"nodes"`
}

type ChamberNode struct {
	Name    string `yaml:"name"`
	Address string `yaml:"address"`
}

type RuntimeConfig struct {
	Engine  string `yaml:"engine"`  // "auto", "podman", "docker"
	Network string `yaml:"network"`
	Prefix  string `yaml:"prefix"`
}

// Default returns a Config with sensible defaults for a laptop deployment.
func Default() *Config {
	return &Config{
		APIVersion: "v1",
		Cave: CaveConfig{
			Image:   "ghcr.io/atgreen/cave:main",
			BaseURL: "http://localhost:9080",
		},
		Ports: PortsConfig{
			HTTP:     9080,
			SSH:      9222,
			Mailpit:  9025,
			GRPC:     9443,
			SSHBind:  "127.0.0.1",
			GRPCBind: "127.0.0.1",
		},
		Database: DatabaseConfig{
			Mode:     "local",
			Image:    "docker.io/postgres:16-alpine",
			Password: "cave",
		},
		Auth: AuthConfig{
			Mode: "local",
		},
		Runner: RunnerConfig{
			Enabled: true,
			Image:   "ghcr.io/atgreen/cave-runner:main",
			Count:   1,
		},
		Zoekt: ZoektConfig{
			Enabled: true,
			Image:   "ghcr.io/atgreen/cave-zoekt:main",
		},
		Chamber: ChamberConfig{
			Nodes: []ChamberNode{},
		},
		SMTP: SMTPConfig{
			Mode:            "mailpit",
			Image:           "docker.io/axllent/mailpit:latest",
			From:            "cave@localhost",
			FromDisplayName: "Cave",
		},
		Runtime: RuntimeConfig{
			Engine:  "auto",
			Network: "cave-net",
			Prefix:  "cave",
		},
	}
}

// Load reads and parses a cave.yaml file.
func Load(path string) (*Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("reading %s: %w", path, err)
	}

	// Expand ${ENV_VAR} references
	expanded := expandEnvVars(string(data))

	cfg := Default()
	if err := yaml.Unmarshal([]byte(expanded), cfg); err != nil {
		return nil, fmt.Errorf("parsing %s: %w", path, err)
	}

	if err := cfg.Validate(); err != nil {
		return nil, fmt.Errorf("validating %s: %w", path, err)
	}

	// Auto-generate secret key if empty
	if cfg.Cave.SecretKey == "" {
		key := make([]byte, 32)
		if _, err := rand.Read(key); err != nil {
			return nil, fmt.Errorf("generating secret key: %w", err)
		}
		cfg.Cave.SecretKey = hex.EncodeToString(key)
	}

	// Local mode runs Cave's embedded Usher OIDC provider. It needs a stable
	// client secret; auto-generate one if the config doesn't carry it yet.
	if cfg.Auth.Mode == "local" && cfg.Auth.OIDC.ClientSecret == "" {
		key := make([]byte, 32)
		if _, err := rand.Read(key); err != nil {
			return nil, fmt.Errorf("generating oidc client secret: %w", err)
		}
		cfg.Auth.OIDC.ClientSecret = hex.EncodeToString(key)
	}

	return cfg, nil
}

// Validate checks the config for errors.
func (c *Config) Validate() error {
	if c.APIVersion != "v1" {
		return fmt.Errorf("unsupported apiVersion: %q (expected \"v1\")", c.APIVersion)
	}
	if c.Cave.Image == "" {
		return fmt.Errorf("cave.image is required")
	}
	if c.Ports.HTTP <= 0 {
		return fmt.Errorf("ports.http must be positive")
	}
	if c.Ports.SSH <= 0 {
		return fmt.Errorf("ports.ssh must be positive")
	}
	if c.Database.Mode != "local" && c.Database.Mode != "external" {
		return fmt.Errorf("database.mode must be \"local\" or \"external\", got %q", c.Database.Mode)
	}
	if c.Database.Mode == "external" && c.Database.URL == "" {
		return fmt.Errorf("database.url is required when database.mode is \"external\"")
	}
	if c.Database.Mode == "external" {
		if _, err := url.Parse(c.Database.URL); err != nil {
			return fmt.Errorf("database.url is not a valid URL: %w", err)
		}
	}
	switch c.Auth.Mode {
	case "local", "oidc":
	default:
		return fmt.Errorf("auth.mode must be \"local\" or \"oidc\", got %q", c.Auth.Mode)
	}
	if c.Auth.Mode == "oidc" {
		if c.Auth.OIDC.Issuer == "" {
			return fmt.Errorf("auth.oidc.issuer is required when auth.mode is \"oidc\"")
		}
		if c.Auth.OIDC.ClientID == "" {
			return fmt.Errorf("auth.oidc.client_id is required when auth.mode is \"oidc\"")
		}
	}
	if c.Runtime.Prefix == "" {
		return fmt.Errorf("runtime.prefix is required")
	}
	if c.Runtime.Network == "" {
		return fmt.Errorf("runtime.network is required")
	}
	return nil
}

// Marshal serializes the config to YAML with comments.
func Marshal(c *Config) ([]byte, error) {
	return yaml.Marshal(c)
}

// ContainerName returns the full container name for a service.
func (c *Config) ContainerName(service string) string {
	if service == "cave" {
		return c.Runtime.Prefix
	}
	return c.Runtime.Prefix + "-" + service
}

// VolumeName returns the volume name for a service.
func (c *Config) VolumeName(suffix string) string {
	return c.Runtime.Prefix + "-" + suffix
}

// MailpitEnabled returns true when SMTP mode is "mailpit" (the default) and we
// should spawn a mailpit catcher container alongside cave.
func (c *Config) MailpitEnabled() bool {
	return c.SMTP.Mode == "" || c.SMTP.Mode == "mailpit"
}

// OIDCEnabled returns true if any OIDC provider is configured. Both "local"
// (embedded Usher) and "oidc" (external IdP) need OIDC config written.
func (c *Config) OIDCEnabled() bool {
	return c.Auth.Mode == "local" || c.Auth.Mode == "oidc"
}

// DBHost returns the database host for cave.conf generation.
func (c *Config) DBHost() string {
	if c.Database.Mode == "external" {
		u, err := url.Parse(c.Database.URL)
		if err != nil {
			return "localhost"
		}
		return u.Hostname()
	}
	return c.ContainerName("pg")
}

// DBPort returns the database port.
func (c *Config) DBPort() string {
	if c.Database.Mode == "external" {
		u, err := url.Parse(c.Database.URL)
		if err != nil || u.Port() == "" {
			return "5432"
		}
		return u.Port()
	}
	return "5432"
}

// DBUser returns the database user.
func (c *Config) DBUser() string {
	if c.Database.Mode == "external" {
		u, err := url.Parse(c.Database.URL)
		if err != nil || u.User == nil {
			return "cave"
		}
		return u.User.Username()
	}
	return "cave"
}

// DBPassword returns the database password.
func (c *Config) DBPassword() string {
	if c.Database.Mode == "external" {
		u, err := url.Parse(c.Database.URL)
		if err != nil || u.User == nil {
			return ""
		}
		pw, _ := u.User.Password()
		return pw
	}
	return c.Database.Password
}

// DBName returns the database name.
func (c *Config) DBName() string {
	if c.Database.Mode == "external" {
		u, err := url.Parse(c.Database.URL)
		if err != nil {
			return "cave"
		}
		return strings.TrimPrefix(u.Path, "/")
	}
	return "cave"
}

var envVarPattern = regexp.MustCompile(`\$\{([^}]+)\}`)

func expandEnvVars(s string) string {
	return envVarPattern.ReplaceAllStringFunc(s, func(match string) string {
		varName := match[2 : len(match)-1]
		if val, ok := os.LookupEnv(varName); ok {
			return val
		}
		return match
	})
}
