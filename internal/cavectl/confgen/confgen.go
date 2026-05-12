package confgen

import (
	"fmt"
	"strings"

	"moxielogic.com/cave/internal/cavectl/config"
)

// Generate produces a cave.conf s-expression from the YAML config.
func Generate(cfg *config.Config) string {
	var b strings.Builder

	w := func(key, value string) {
		fmt.Fprintf(&b, " :%s %s\n", key, value)
	}
	ws := func(key, value string) {
		fmt.Fprintf(&b, " :%s %q\n", key, value)
	}

	b.WriteString("(")
	w("http-port", "8080")
	w("ssh-port", "22")
	ws("ssh-user", "cave")
	ws("data-dir", "/var/lib/cave")
	ws("db-host", cfg.DBHost())
	w("db-port", cfg.DBPort())
	ws("db-name", cfg.DBName())
	ws("db-user", cfg.DBUser())
	ws("db-password", cfg.DBPassword())
	ws("secret-key", cfg.Cave.SecretKey)
	ws("base-url", cfg.Cave.BaseURL)
	ws("authorized-keys-path", "/home/cave/.ssh/authorized_keys")
	ws("cave-shell", "/usr/bin/cave-shell.sh")

	// OIDC config
	if cfg.OIDCEnabled() {
		switch cfg.Auth.Mode {
		case "keycloak":
			internalURL := fmt.Sprintf("http://%s:8080/realms/cave", cfg.ContainerName("keycloak"))
			ws("oidc-issuer", cfg.Cave.BaseURL+"/realms/cave") // placeholder — user sets base URL
			ws("oidc-issuer-internal", internalURL)
			ws("oidc-client-id", "cave")
			ws("oidc-client-secret", "")
		case "oidc":
			ws("oidc-issuer", cfg.Auth.OIDC.Issuer)
			ws("oidc-issuer-internal", cfg.Auth.OIDC.Issuer)
			ws("oidc-client-id", cfg.Auth.OIDC.ClientID)
			ws("oidc-client-secret", cfg.Auth.OIDC.ClientSecret)
		}
	}

	// Zoekt
	if cfg.Zoekt.Enabled {
		w("zoekt-enabled", "t")
		ws("zoekt-web-url", fmt.Sprintf("http://%s:6070", cfg.ContainerName("zoekt-web")))
		ws("zoekt-index-dir", "/data/zoekt-index")
	} else {
		w("zoekt-enabled", "nil")
	}

	// Chamber
	w("chamber-enabled", "t")
	if len(cfg.Chamber.Nodes) > 1 {
		var nodes strings.Builder
		nodes.WriteString("(")
		for i, n := range cfg.Chamber.Nodes {
			if i > 0 {
				nodes.WriteString("\n                   ")
			}
			fmt.Fprintf(&nodes, "(:name %q :address %q)", n.Name, n.Address)
		}
		nodes.WriteString(")")
		w("chamber-nodes", nodes.String())
	}

	b.WriteString(")")
	return b.String()
}
