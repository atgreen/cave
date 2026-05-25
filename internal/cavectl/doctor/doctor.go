// Package doctor runs a suite of sanity checks against a cave deployment
// and the host it's running on. It's meant to surface the things that
// silently break a deploy — unprivileged port limits, missing DNS,
// containers that exited, schema drift, OIDC misconfiguration.
package doctor

import (
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/runtime"
)

type Status int

const (
	OK Status = iota
	Warn
	Fail
	Skip
)

func (s Status) Symbol() string {
	switch s {
	case OK:
		return "\033[32m✓\033[0m"
	case Warn:
		return "\033[33m!\033[0m"
	case Fail:
		return "\033[31m✗\033[0m"
	case Skip:
		return "\033[90m-\033[0m"
	}
	return "?"
}

type Result struct {
	Name   string
	Status Status
	Detail string
	Fix    string
}

func (r Result) Print() {
	fmt.Printf("  %s %s", r.Status.Symbol(), r.Name)
	if r.Detail != "" {
		fmt.Printf("  \033[90m— %s\033[0m", r.Detail)
	}
	fmt.Println()
	if r.Fix != "" && (r.Status == Fail || r.Status == Warn) {
		fmt.Printf("      \033[90mfix:\033[0m %s\n", r.Fix)
	}
}

// Run executes every check against cfg and returns the results in order.
func Run(cfg *config.Config, rt runtime.Runtime) []Result {
	results := []Result{
		checkRuntime(rt),
		checkSELinux(),
		checkUnprivPorts(cfg),
		checkDNS("base_url", cfg.Cave.BaseURL),
	}
	if cfg.KeycloakEnabled() {
		results = append(results, checkDNS("keycloak.public_url", cfg.Auth.Keycloak.PublicURL))
	}
	results = append(results, checkContainers(cfg, rt)...)
	results = append(results, checkSSHListening(cfg))
	results = append(results, checkKeycloakDB(cfg, rt))
	results = append(results, checkSchemaVersion(cfg, rt))
	results = append(results, checkRealmSMTP(cfg, rt))
	return results
}

// --- individual checks ---

func checkRuntime(rt runtime.Runtime) Result {
	if rt == nil {
		return Result{Name: "container runtime", Status: Fail, Detail: "no podman or docker on $PATH",
			Fix: "install podman: sudo dnf install -y podman"}
	}
	return Result{Name: "container runtime", Status: OK, Detail: rt.Name()}
}

func checkSELinux() Result {
	if _, err := os.Stat("/sys/fs/selinux"); err != nil {
		return Result{Name: "SELinux", Status: Skip, Detail: "not in use"}
	}
	out, err := exec.Command("getenforce").Output()
	if err != nil {
		return Result{Name: "SELinux", Status: Skip, Detail: "getenforce not found"}
	}
	mode := strings.TrimSpace(string(out))
	return Result{Name: "SELinux", Status: OK, Detail: mode}
}

func checkUnprivPorts(cfg *config.Config) Result {
	b, err := os.ReadFile("/proc/sys/net/ipv4/ip_unprivileged_port_start")
	if err != nil {
		return Result{Name: "unprivileged port range", Status: Skip, Detail: "cannot read sysctl"}
	}
	start, _ := strconv.Atoi(strings.TrimSpace(string(b)))
	min := minOf(cfg.Ports.HTTP, cfg.Ports.SSH, cfg.Ports.Keycloak, cfg.Ports.Mailpit)
	if min < start {
		return Result{
			Name: "unprivileged port range", Status: Fail,
			Detail: fmt.Sprintf("ip_unprivileged_port_start=%d; configured port %d won't bind under rootless podman", start, min),
			Fix:    fmt.Sprintf("sudo sysctl net.ipv4.ip_unprivileged_port_start=0 && echo 'net.ipv4.ip_unprivileged_port_start=0' | sudo tee /etc/sysctl.d/99-cave.conf"),
		}
	}
	return Result{Name: "unprivileged port range", Status: OK, Detail: fmt.Sprintf("start=%d", start)}
}

func checkDNS(label, urlStr string) Result {
	if urlStr == "" {
		return Result{Name: label + " resolves", Status: Skip, Detail: "not set"}
	}
	host := hostnameFromURL(urlStr)
	if host == "" || host == "localhost" {
		return Result{Name: label + " resolves", Status: Skip, Detail: host}
	}
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	ips, err := net.DefaultResolver.LookupHost(ctx, host)
	if err != nil {
		return Result{
			Name: label + " resolves", Status: Fail,
			Detail: fmt.Sprintf("%s: %v", host, err),
			Fix:    "add an A/AAAA record at your DNS provider pointing " + host + " at this host's public IP",
		}
	}
	return Result{Name: label + " resolves", Status: OK, Detail: fmt.Sprintf("%s -> %s", host, strings.Join(ips, ","))}
}

func checkContainers(cfg *config.Config, rt runtime.Runtime) []Result {
	if rt == nil {
		return nil
	}
	want := []string{"pg", "cave"}
	if cfg.KeycloakEnabled() {
		want = append(want, "keycloak")
	}
	if cfg.Zoekt.Enabled {
		want = append(want, "zoekt-web")
	}
	if cfg.MailpitEnabled() {
		want = append(want, "mailpit")
	}
	var out []Result
	for _, svc := range want {
		name := cfg.ContainerName(svc)
		info, err := rt.Inspect(name)
		switch {
		case err != nil || info == nil || info.Status == "not-found":
			out = append(out, Result{
				Name: "container " + name, Status: Fail, Detail: "not found",
				Fix: "cavectl apply",
			})
		case !strings.HasPrefix(strings.ToLower(info.Status), "running") && !strings.HasPrefix(strings.ToLower(info.Status), "up"):
			out = append(out, Result{
				Name: "container " + name, Status: Fail, Detail: info.Status,
				Fix: "podman logs " + name + "  (then: cavectl apply)",
			})
		default:
			out = append(out, Result{Name: "container " + name, Status: OK, Detail: info.Status})
		}
	}
	return out
}

func checkSSHListening(cfg *config.Config) Result {
	if cfg.Ports.SSH == 0 {
		return Result{Name: "cave SSH listening", Status: Skip}
	}
	addr := "127.0.0.1"
	if cfg.Ports.SSHBind != "" && cfg.Ports.SSHBind != "127.0.0.1" {
		addr = "0.0.0.0"
	}
	d := net.Dialer{Timeout: 2 * time.Second}
	c, err := d.Dial("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(cfg.Ports.SSH)))
	if err != nil {
		return Result{
			Name: "cave SSH listening", Status: Fail,
			Detail: fmt.Sprintf("nothing on :%d (%v)", cfg.Ports.SSH, err),
			Fix:    "cavectl apply, then check `podman logs cave`",
		}
	}
	c.Close()
	return Result{Name: "cave SSH listening", Status: OK, Detail: fmt.Sprintf("%s:%d", addr, cfg.Ports.SSH)}
}

func checkKeycloakDB(cfg *config.Config, rt runtime.Runtime) Result {
	if !cfg.KeycloakEnabled() {
		return Result{Name: "keycloak DB", Status: Skip}
	}
	if rt == nil {
		return Result{Name: "keycloak DB", Status: Skip, Detail: "no runtime"}
	}
	pg := cfg.ContainerName("pg")
	out, err := rt.Exec(pg, []string{"sh", "-c",
		`psql -U cave -tAc "SELECT 1 FROM pg_database WHERE datname='keycloak'"`})
	if err != nil {
		return Result{Name: "keycloak DB", Status: Warn, Detail: "could not query postgres"}
	}
	if strings.TrimSpace(out) != "1" {
		return Result{Name: "keycloak DB", Status: Fail, Detail: "missing",
			Fix: "podman exec " + pg + " psql -U cave -c 'CREATE DATABASE keycloak'"}
	}
	return Result{Name: "keycloak DB", Status: OK}
}

func checkSchemaVersion(cfg *config.Config, rt runtime.Runtime) Result {
	if rt == nil {
		return Result{Name: "cave schema", Status: Skip}
	}
	pg := cfg.ContainerName("pg")
	out, err := rt.Exec(pg, []string{"sh", "-c",
		`psql -U cave -d cave -tAc "SELECT COALESCE(MAX(version), 0) FROM cave_schema_version"`})
	if err != nil {
		return Result{Name: "cave schema", Status: Warn, Detail: "cannot read schema version"}
	}
	v := strings.TrimSpace(out)
	if v == "0" || v == "" {
		return Result{Name: "cave schema", Status: Fail, Detail: "no migrations applied",
			Fix: "podman exec " + cfg.ContainerName("cave") + " cave-server migrate --config /etc/cave.conf"}
	}
	return Result{Name: "cave schema", Status: OK, Detail: "version " + v}
}

func checkRealmSMTP(cfg *config.Config, rt runtime.Runtime) Result {
	if !cfg.KeycloakEnabled() || rt == nil {
		return Result{Name: "keycloak realm SMTP", Status: Skip}
	}
	pg := cfg.ContainerName("pg")
	out, err := rt.Exec(pg, []string{"sh", "-c",
		`psql -U cave -d keycloak -tAc "SELECT value FROM realm_smtp_config WHERE realm_id=(SELECT id FROM realm WHERE name='cave') AND name='host'" 2>/dev/null`})
	if err != nil || strings.TrimSpace(out) == "" {
		return Result{Name: "keycloak realm SMTP", Status: Warn, Detail: "smtpHost not set in realm",
			Fix: "drop the keycloak DB and re-apply so the realm imports with current SMTP env"}
	}
	host := strings.TrimSpace(out)
	if strings.Contains(host, "__") {
		return Result{Name: "keycloak realm SMTP", Status: Fail, Detail: "still has placeholder: " + host,
			Fix: "the cave-keycloak entrypoint didn't substitute; check SMTP_HOST env on cave-keycloak"}
	}
	return Result{Name: "keycloak realm SMTP", Status: OK, Detail: "host=" + host}
}

// --- helpers ---

func minOf(vals ...int) int {
	m := 0
	for _, v := range vals {
		if v == 0 {
			continue
		}
		if m == 0 || v < m {
			m = v
		}
	}
	return m
}

func hostnameFromURL(u string) string {
	u = strings.TrimSpace(u)
	for _, pfx := range []string{"https://", "http://"} {
		if strings.HasPrefix(u, pfx) {
			u = u[len(pfx):]
			break
		}
	}
	if i := strings.IndexAny(u, "/:"); i >= 0 {
		u = u[:i]
	}
	return u
}
