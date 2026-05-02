SBCL ?= /usr/bin/sbcl
LISP := $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: help build load lint clean test test-smoke test-workflow podman-up podman-down podman-rebuild podman-logs

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
