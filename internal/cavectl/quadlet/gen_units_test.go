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
		"Environment=CAVE_INTERNAL_URL=http://169.254.1.3:9080",
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
