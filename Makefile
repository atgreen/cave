SBCL ?= /usr/bin/sbcl
LISP := $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'

VERSION ?= $(shell git describe --tags --always --dirty 2>/dev/null || echo dev)
QUADLET_DIR = $(HOME)/.config/containers/systemd

.PHONY: help build cave load lint clean test test-smoke test-workflow \
       podman-up podman-down podman-rebuild podman-logs \
       observability-up observability-down \
       runner-image tag release prod-install prod-uninstall prod-start prod-stop prod-logs prod-status \
       prod-backup prod-restore prod-rollback prod-images

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-16s\033[0m %s\n", $$1, $$2}'

build: cave-server cave cavectl zoekt-git-index ## Build the server, CLI, and cavectl

cave: ## Build the Go CLI client (talks to a cave-server)
	go build -o cave ./cli/cave

cavectl: ## Build the cavectl deployment tool
	go build -o cavectl ./cli/cavectl

zoekt-git-index: ## Build zoekt-git-index from sourcegraph/zoekt source
	@if [ ! -d _zoekt ]; then git clone --depth 1 https://github.com/sourcegraph/zoekt.git _zoekt; fi
	cd _zoekt && CGO_ENABLED=0 go build -o ../zoekt-git-index ./cmd/zoekt-git-index

cave-server: src/*.lisp *.asd
	$(LISP) --eval '(asdf:make :cave)'
	chmod +x cave-server

load: ## Load-test without building image
	$(LISP) --eval '(asdf:load-system :cave)' --eval '(format t "~%OK~%")'

lint: ## Run ocicl lint on source
	ocicl lint src/*.lisp

clean: ## Remove build artifacts
	rm -rf *~ cave-server cave zoekt-git-index _zoekt test-results playwright-report

test: ## Run all Playwright tests (requires running cave)
	npx playwright test

test-smoke: ## Run smoke tests only
	npx playwright test tests/smoke.spec.js

test-workflow: ## Run org/repo workflow tests only
	npx playwright test tests/org-repo.spec.js

# --- Podman (local dev) ---

podman-up: cave-server cave ## Build container and start cave-server + postgres + keycloak via podman
	podman network exists cave-net 2>/dev/null || podman network create cave-net
	podman container exists cave-pg 2>/dev/null || \
		podman run -d --name cave-pg --network cave-net \
			-e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
			postgres:16-alpine
	@echo "Waiting for PostgreSQL..."; \
		for i in $$(seq 1 30); do \
			podman exec cave-pg pg_isready -U cave -q 2>/dev/null && break; \
			sleep 1; \
		done
	podman exec cave-pg psql -U cave -tc "SELECT 1 FROM pg_database WHERE datname='keycloak'" | grep -q 1 || \
		podman exec cave-pg psql -U cave -c "CREATE DATABASE keycloak"
	podman container exists mailpit 2>/dev/null || \
		podman run -d --name mailpit --network cave-net \
			-p 8025:8025 \
			docker.io/axllent/mailpit:latest
	podman container exists cave-keycloak 2>/dev/null || \
		podman run -d --name cave-keycloak --network cave-net \
			-p 8180:8080 \
			-e KC_DB=postgres \
			-e KC_DB_URL=jdbc:postgresql://cave-pg:5432/keycloak \
			-e KC_DB_USERNAME=cave \
			-e KC_DB_PASSWORD=cave \
			-e KC_HOSTNAME=http://localhost:8180 \
			-e KC_HOSTNAME_STRICT=false \
			-e KC_HOSTNAME_BACKCHANNEL_DYNAMIC=true \
			-e KC_HTTP_ENABLED=true \
			-e KC_BOOTSTRAP_ADMIN_USERNAME=admin \
			-e KC_BOOTSTRAP_ADMIN_PASSWORD=admin \
			-v $(CURDIR)/keycloak/cave-realm.json:/opt/keycloak/data/import/cave-realm.json:ro,z \
			-v $(CURDIR)/keycloak/themes/cave:/opt/keycloak/themes/cave:ro,z \
			quay.io/keycloak/keycloak:26.0 start-dev --import-realm
	$(CURDIR)/keycloak/configure-realm.sh http://localhost:8180
	podman build -t cave -f Containerfile.local .
	podman stop cave 2>/dev/null; podman rm cave 2>/dev/null; true
	podman run -d --name cave --network cave-net \
		-p 8080:8080 -p 2222:22 \
		-e CAVE_DB_HOST=cave-pg \
		-e CAVE_OIDC_ISSUER=http://localhost:8180/realms/cave \
		-e CAVE_OIDC_ISSUER_INTERNAL=http://cave-keycloak:8080/realms/cave \
		-e CAVE_OIDC_CLIENT_ID=cave \
		-e CAVE_OIDC_CLIENT_SECRET=cave-dev-secret \
		-e CAVE_BASE_URL=http://localhost:8080 \
		-v cave-data:/var/lib/cave cave:latest
	@echo "\n  Cave:     http://localhost:8080"
	@echo "  Keycloak: http://localhost:8180  (admin/admin)"
	@echo "  Mailpit:  http://localhost:8025"

podman-down: ## Stop and remove cave + postgres + keycloak + mailpit containers
	podman stop cave cave-keycloak mailpit cave-pg 2>/dev/null; true
	podman rm cave cave-keycloak mailpit cave-pg 2>/dev/null; true

podman-rebuild: podman-down podman-up ## Tear down and rebuild everything

podman-logs: ## Tail cave container logs
	podman logs -f cave

# --- Observability ---

observability-up: ## Start Prometheus + Grafana + postgres_exporter
	podman network exists cave-net 2>/dev/null || podman network create cave-net
	podman container exists prometheus 2>/dev/null || \
		podman run -d --name prometheus --network cave-net \
			-p 9090:9090 \
			-v $(CURDIR)/observability/prometheus.yml:/etc/prometheus/prometheus.yml:ro \
			docker.io/prom/prometheus:latest
	podman container exists postgres-exporter 2>/dev/null || \
		podman run -d --name postgres-exporter --network cave-net \
			-e DATA_SOURCE_NAME="postgresql://cave:cave@cave-pg:5432/cave?sslmode=disable" \
			docker.io/prometheuscommunity/postgres-exporter:latest
	podman container exists grafana 2>/dev/null || \
		podman run -d --name grafana --network cave-net \
			-p 3000:3000 \
			-e GF_SECURITY_ADMIN_USER=admin \
			-e GF_SECURITY_ADMIN_PASSWORD=admin \
			-e GF_AUTH_ANONYMOUS_ENABLED=true \
			-e GF_AUTH_ANONYMOUS_ORG_ROLE=Viewer \
			-v $(CURDIR)/observability/grafana/provisioning:/etc/grafana/provisioning:ro \
			-v $(CURDIR)/observability/grafana/dashboards:/var/lib/grafana/dashboards:ro \
			docker.io/grafana/grafana:latest
	@echo "\n  Prometheus: http://localhost:9090"
	@echo "  Grafana:    http://localhost:3000  (admin/admin)"

observability-down: ## Stop and remove observability containers
	podman stop prometheus postgres-exporter grafana 2>/dev/null; true
	podman rm prometheus postgres-exporter grafana 2>/dev/null; true

# --- Production (quadlet/systemd) ---

runner-image: cave ## Build the runner container image
	podman build -t cave-runner:latest -f Containerfile.runner .

tag: ## Tag current commit as a release (e.g., make tag V=0.2.0)
	@if [ -z "$(V)" ]; then echo "Usage: make tag V=0.2.0"; exit 1; fi
	git tag -a "v$(V)" -m "Release $(V)"
	@echo "Tagged v$(V). Run 'make release' to build and deploy."

release: build ## Build prod image from current tree, tag as cave:prod
	podman build -t cave:$(VERSION) -f Containerfile.local .
	-podman tag cave:prod cave:prod-previous 2>/dev/null
	podman tag cave:$(VERSION) cave:prod
	podman build -t cave-zoekt:$(VERSION) -f Containerfile.zoekt .
	-podman tag cave-zoekt:prod cave-zoekt:prod-previous 2>/dev/null
	podman tag cave-zoekt:$(VERSION) cave-zoekt:prod
	@echo "\n  Built cave:$(VERSION) + cave-zoekt:$(VERSION), tagged as :prod"
	@echo "  Previous prod saved as :prod-previous"
	@echo "  Run 'make prod-start' or 'systemctl --user restart cave' to deploy"

prod-rollback: ## Roll back prod to the previous release
	@podman image exists cave:prod-previous 2>/dev/null || (echo "No previous release to roll back to."; exit 1)
	podman tag cave:prod cave:prod-failed
	podman tag cave:prod-previous cave:prod
	systemctl --user restart cave
	@echo "Rolled back to previous release. Failed build saved as cave:prod-failed."

prod-images: ## List available cave images for rollback
	@podman images --format "table {{.Repository}}:{{.Tag}}\t{{.ID}}\t{{.Created}}" | grep cave:

prod-install: ## Install quadlet units for production (systemd --user)
	mkdir -p $(QUADLET_DIR)
	cp deploy/quadlet/*.container deploy/quadlet/*.volume deploy/quadlet/*.network $(QUADLET_DIR)/
	systemctl --user daemon-reload
	@echo "Quadlet units installed. Run 'make prod-start' to start."
	@echo "\n  Cave:     http://localhost:9080"
	@echo "  Keycloak: http://localhost:9180"
	@echo "  Mailpit:  http://localhost:9025"
	@echo "  SSH:      port 9222"

prod-uninstall: prod-stop ## Remove quadlet units
	rm -f $(QUADLET_DIR)/cave*.container $(QUADLET_DIR)/cave*.volume $(QUADLET_DIR)/cave*.network
	systemctl --user daemon-reload
	@echo "Quadlet units removed."

prod-start: ## Start production Cave via systemd
	systemctl --user start cave-pg cave-mailpit cave-init cave-keycloak cave
	$(CURDIR)/keycloak/configure-realm.sh http://localhost:9180 cave-prod-mailpit
	@echo "\n  Cave:     http://localhost:9080"
	@echo "  Keycloak: http://localhost:9180"
	@echo "  Mailpit:  http://localhost:9025"

prod-stop: ## Stop production Cave
	systemctl --user stop cave cave-keycloak cave-mailpit cave-init cave-pg 2>/dev/null; true

prod-logs: ## Tail production Cave logs
	journalctl --user -u cave -f

prod-backup: ## Back up production data (Postgres + repos + config)
	$(CURDIR)/deploy/backup.sh $(HOME)/cave-backups cave-prod

prod-restore: ## Restore from backup (usage: make prod-restore F=path/to/archive.tar.gz)
	@if [ -z "$(F)" ]; then echo "Usage: make prod-restore F=~/cave-backups/cave-2026-05-02-120000.tar.gz"; exit 1; fi
	$(CURDIR)/deploy/restore.sh $(F) cave-prod

prod-status: ## Show production service status
	@systemctl --user is-active cave-pg cave-keycloak cave cave-mailpit 2>/dev/null || true
	@echo "---"
	@systemctl --user status cave --no-pager -l 2>/dev/null | head -15 || echo "cave: not installed"
