package quadlet

import (
	"strings"
	"testing"

	"moxielogic.com/cave/internal/cavectl/config"
)

func TestGeneratesSSHFrontEnd(t *testing.T) {
	cfg := config.Default()
	units := generate(cfg)
	ssh, ok := units[cfg.Runtime.Prefix+"-ssh.container"]
	if !ok {
		t.Fatalf("no ssh unit; got %v", keys(units))
	}
	for _, want := range []string{
		"Network=pasta:--map-host-loopback,169.254.1.3",
		"Environment=CAVE_ROLE=ssh",
		"PublishPort=127.0.0.1:9222:22",
		"Environment=CAVE_DB_HOST=169.254.1.3",
		"Environment=CAVE_DB_USER=cave",
		// git-shell authenticates against the database, so the front end needs
		// credentials too - now via its environment file, since a unit is
		// world-readable to anyone with shell (see TestUnitsCarryNoCredentials).
		"EnvironmentFile=",
		"Environment=CAVE_INTERNAL_URL=http://169.254.1.3:9080",
		// Requires= would stop the front end on every cave restart, taking
		// git over SSH down with each deploy.
		"Wants=cave.service",
	} {
		if !strings.Contains(ssh, want) {
			t.Errorf("ssh unit missing %q:\n%s", want, ssh)
		}
	}
	cave := units[cfg.Runtime.Prefix+".container"]
	if strings.Contains(cave, ":22") {
		t.Errorf("cave unit should no longer publish ssh:\n%s", cave)
	}
	pg := units[cfg.Runtime.Prefix+"-pg.container"]
	if !strings.Contains(pg, "PublishPort=127.0.0.1:9432:5432") {
		t.Errorf("pg unit should publish on the host loopback:\n%s", pg)
	}
}

func keys(m map[string]string) []string {
	var out []string
	for k := range m {
		out = append(out, k)
	}
	return out
}

// A unit is read by anyone who can run systemctl cat or podman inspect, and
// gets pasted into support threads. Credentials belong in the 0600 environment
// file beside it, never in the unit itself (cave-npw).
func TestUnitsCarryNoCredentials(t *testing.T) {
	cfg := config.Default()
	cfg.Database.Password = "db-password-sentinel"
	cfg.Cave.SecretKey = "secret-key-sentinel"
	cfg.Cave.InternalToken = "internal-token-sentinel"
	cfg.Auth.OIDC.ClientSecret = "oidc-secret-sentinel"
	cfg.SMTP.Password = "smtp-password-sentinel"

	for name, body := range generate(cfg) {
		for _, secret := range []string{
			cfg.Database.Password, cfg.Cave.SecretKey, cfg.Cave.InternalToken,
			cfg.Auth.OIDC.ClientSecret, cfg.SMTP.Password,
		} {
			if strings.Contains(body, secret) {
				t.Errorf("%s leaks a credential (%s):\n%s", name, secret, body)
			}
		}
	}

	// And the containers that need them do reference a file.
	for _, unit := range []string{cfg.Runtime.Prefix + ".container",
		cfg.Runtime.Prefix + "-pg.container",
		cfg.Runtime.Prefix + "-ssh.container"} {
		if !strings.Contains(generate(cfg)[unit], "EnvironmentFile=") {
			t.Errorf("%s should read its credentials from a file", unit)
		}
	}
}

func TestSecretEnvCoversEachContainer(t *testing.T) {
	cfg := config.Default()
	cfg.Cave.InternalToken = "t"
	// The front end authenticates to the database and to cave's internal
	// endpoints; without these it falls back to defaults and every push fails.
	ssh := secretEnv(cfg, "ssh")
	for _, k := range []string{"CAVE_DB_PASSWORD", "CAVE_INTERNAL_TOKEN"} {
		if _, ok := ssh[k]; !ok {
			t.Errorf("ssh front end needs %s", k)
		}
	}
	if _, ok := secretEnv(cfg, "pg")["POSTGRES_PASSWORD"]; !ok {
		t.Error("postgres needs its password")
	}
}
