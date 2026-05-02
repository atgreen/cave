SBCL ?= /usr/bin/sbcl
LISP := $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: help build load lint clean test test-smoke test-workflow \
       podman-up podman-down podman-rebuild podman-logs \
       observability-up observability-down

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-16s\033[0m %s\n", $$1, $$2}'

build: cave ## Build the cave binary

cave: src/*.lisp *.asd
	$(LISP) --eval '(asdf:make :cave)'
	chmod +x cave

load: ## Load-test without building image
	$(LISP) --eval '(asdf:load-system :cave)' --eval '(format t "~%OK~%")'

lint: ## Run ocicl lint on source
	ocicl lint src/*.lisp

clean: ## Remove build artifacts
	rm -rf *~ cave test-results playwright-report

test: ## Run all Playwright tests (requires running cave)
	npx playwright test

test-smoke: ## Run smoke tests only
	npx playwright test tests/smoke.spec.js

test-workflow: ## Run org/repo workflow tests only
	npx playwright test tests/org-repo.spec.js

# --- Podman (local dev) ---

podman-up: cave ## Build container and start cave + postgres via podman
	podman network exists cave-net 2>/dev/null || podman network create cave-net
	podman container exists cave-pg 2>/dev/null || \
		podman run -d --name cave-pg --network cave-net \
			-e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
			postgres:16-alpine
	podman build -t cave -f Containerfile.local .
	podman stop cave 2>/dev/null; podman rm cave 2>/dev/null; true
	podman run -d --name cave --network cave-net \
		-p 8080:8080 -p 2222:22 \
		-e CAVE_DB_HOST=cave-pg \
		-e CAVE_ADMIN_USER=admin -e CAVE_ADMIN_PASSWORD=admin \
		-v cave-data:/var/lib/cave cave:latest
	@echo "\n  Cave running at http://localhost:8080"

podman-down: ## Stop and remove cave + postgres containers
	podman stop cave cave-pg 2>/dev/null; true
	podman rm cave cave-pg 2>/dev/null; true

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
