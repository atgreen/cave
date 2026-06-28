;;; db.lisp — PostgreSQL database layer via postmodern
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

(defvar *db-connected* nil)
(defvar *db-spec* nil
  "Connection spec list for postmodern: (name user password host :port port :pooled-p t).")

(defun connect-db (&key (host (config-value :db-host))
                        (port (config-value :db-port))
                        (name (config-value :db-name))
                        (user (config-value :db-user))
                        (password (config-value :db-password)))
  "Connect to the database.
   Sets up both a toplevel connection (for CLI commands) and a pool spec
   (for per-request connections in the HTTP server)."
  (setf *db-spec* (list name user password host :port port :pooled-p t))
  (postmodern:connect-toplevel name user password host :port port)
  (setf *db-connected* t)
  (llog:info "Connected to database" :db name :host host :port port))

(defun disconnect-db ()
  "Disconnect and clear the connection pool."
  (when *db-connected*
    (postmodern:disconnect-toplevel)
    (postmodern:clear-connection-pool)
    (setf *db-connected* nil)
    (llog:info "Disconnected from database")))

(defun db-query (sql &rest params)
  "Execute a SQL query with parameters. Returns list of rows as plists."
  (postmodern:query (apply #'format nil sql params) :plists))

;;; --- Migrations ---

(defparameter *migrations*
  '((1 . "-- Schema version tracking
CREATE TABLE IF NOT EXISTS cave_schema_version (
  version INTEGER NOT NULL,
  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);")

    (2 . "-- Users
CREATE TABLE cave_users (
  id BIGSERIAL PRIMARY KEY,
  username VARCHAR(64) NOT NULL UNIQUE,
  password_hash VARCHAR(256) NOT NULL,
  display_name VARCHAR(128),
  email VARCHAR(256),
  is_admin BOOLEAN NOT NULL DEFAULT FALSE,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_users_username ON cave_users (username);")

    (3 . "-- SSH keys
CREATE TABLE cave_ssh_keys (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  public_key TEXT NOT NULL UNIQUE,
  fingerprint VARCHAR(128) NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_ssh_keys_fingerprint ON cave_ssh_keys (fingerprint);
CREATE INDEX idx_ssh_keys_user_id ON cave_ssh_keys (user_id);")

    (4 . "-- API tokens
CREATE TABLE cave_api_tokens (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  token_hash VARCHAR(256) NOT NULL UNIQUE,
  token_prefix VARCHAR(8) NOT NULL,
  last_used_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_api_tokens_hash ON cave_api_tokens (token_hash);
CREATE INDEX idx_api_tokens_user_id ON cave_api_tokens (user_id);")

    (5 . "-- Organizations
CREATE TABLE cave_orgs (
  id BIGSERIAL PRIMARY KEY,
  name VARCHAR(64) NOT NULL UNIQUE,
  display_name VARCHAR(128),
  description TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_orgs_name ON cave_orgs (name);")

    (6 . "-- Org membership
CREATE TABLE cave_org_members (
  id BIGSERIAL PRIMARY KEY,
  org_id BIGINT NOT NULL REFERENCES cave_orgs(id) ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  role VARCHAR(16) NOT NULL DEFAULT 'member' CHECK (role IN ('admin', 'member')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(org_id, user_id)
);
CREATE INDEX idx_org_members_org ON cave_org_members (org_id);
CREATE INDEX idx_org_members_user ON cave_org_members (user_id);")

    (7 . "-- Repositories
CREATE TABLE cave_repos (
  id BIGSERIAL PRIMARY KEY,
  org_id BIGINT NOT NULL REFERENCES cave_orgs(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  description TEXT,
  is_private BOOLEAN NOT NULL DEFAULT FALSE,
  default_branch VARCHAR(256) NOT NULL DEFAULT 'main',
  next_number INTEGER NOT NULL DEFAULT 1,
  -- Merge policy
  required_approvals INTEGER NOT NULL DEFAULT 1,
  allow_stale_approvals BOOLEAN NOT NULL DEFAULT FALSE,
  allow_self_approval BOOLEAN NOT NULL DEFAULT FALSE,
  concerns_count_as_approval BOOLEAN NOT NULL DEFAULT TRUE,
  require_zero_unresolved_concerns BOOLEAN NOT NULL DEFAULT FALSE,
  block_on_request_changes BOOLEAN NOT NULL DEFAULT TRUE,
  required_checks_pass BOOLEAN NOT NULL DEFAULT TRUE,
  allow_direct_push_to_default BOOLEAN NOT NULL DEFAULT FALSE,
  auto_delete_branch BOOLEAN NOT NULL DEFAULT TRUE,
  -- Deploy config (admin-managed)
  deploy_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  deploy_branch VARCHAR(256) NOT NULL DEFAULT 'main',
  container_port INTEGER,
  host_port INTEGER,
  build_timeout INTEGER NOT NULL DEFAULT 600,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(org_id, name)
);
CREATE INDEX idx_repos_org ON cave_repos (org_id);")

    (8 . "-- Repo membership
CREATE TABLE cave_repo_members (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  role VARCHAR(16) NOT NULL DEFAULT 'writer' CHECK (role IN ('writer', 'reviewer', 'admin')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, user_id)
);
CREATE INDEX idx_repo_members_repo ON cave_repo_members (repo_id);
CREATE INDEX idx_repo_members_user ON cave_repo_members (user_id);")

    (9 . "-- Issues and changesets share a number sequence per repo (next_number on repos).
-- Issues
CREATE TABLE cave_issues (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  number INTEGER NOT NULL,
  author_id BIGINT NOT NULL REFERENCES cave_users(id),
  title VARCHAR(512) NOT NULL,
  body TEXT,
  status VARCHAR(16) NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'closed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  closed_at TIMESTAMPTZ,
  UNIQUE(repo_id, number)
);
CREATE INDEX idx_issues_repo ON cave_issues (repo_id);
CREATE INDEX idx_issues_status ON cave_issues (repo_id, status);")

    (10 . "-- Issue labels
CREATE TABLE cave_issue_labels (
  id BIGSERIAL PRIMARY KEY,
  issue_id BIGINT NOT NULL REFERENCES cave_issues(id) ON DELETE CASCADE,
  label VARCHAR(64) NOT NULL,
  UNIQUE(issue_id, label)
);
CREATE INDEX idx_issue_labels_issue ON cave_issue_labels (issue_id);

-- Issue assignees
CREATE TABLE cave_issue_assignees (
  id BIGSERIAL PRIMARY KEY,
  issue_id BIGINT NOT NULL REFERENCES cave_issues(id) ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  UNIQUE(issue_id, user_id)
);")

    (11 . "-- Changesets
CREATE TABLE cave_changesets (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  number INTEGER NOT NULL,
  author_id BIGINT NOT NULL REFERENCES cave_users(id),
  source_branch VARCHAR(256) NOT NULL,
  target_branch VARCHAR(256) NOT NULL,
  head_commit VARCHAR(64),
  version INTEGER NOT NULL DEFAULT 1,
  is_merged BOOLEAN NOT NULL DEFAULT FALSE,
  is_closed BOOLEAN NOT NULL DEFAULT FALSE,
  stack_id BIGINT,
  stack_order INTEGER,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  merged_at TIMESTAMPTZ,
  closed_at TIMESTAMPTZ,
  UNIQUE(repo_id, number)
);
CREATE INDEX idx_changesets_repo ON cave_changesets (repo_id);
CREATE INDEX idx_changesets_branch ON cave_changesets (repo_id, source_branch);
CREATE INDEX idx_changesets_stack ON cave_changesets (stack_id);")

    (12 . "-- Stacks
CREATE TABLE cave_stacks (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  name VARCHAR(256) NOT NULL,
  base_branch VARCHAR(256) NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, name)
);
-- Now add the FK from changesets to stacks
ALTER TABLE cave_changesets
  ADD CONSTRAINT fk_changesets_stack
  FOREIGN KEY (stack_id) REFERENCES cave_stacks(id) ON DELETE SET NULL;")

    (13 . "-- Reviews
CREATE TABLE cave_reviews (
  id BIGSERIAL PRIMARY KEY,
  changeset_id BIGINT NOT NULL REFERENCES cave_changesets(id) ON DELETE CASCADE,
  reviewer_id BIGINT NOT NULL REFERENCES cave_users(id),
  state VARCHAR(32) NOT NULL CHECK (state IN ('approve', 'approve_with_concerns', 'request_changes', 'comment')),
  body TEXT,
  changeset_version INTEGER NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_reviews_changeset ON cave_reviews (changeset_id);
CREATE INDEX idx_reviews_reviewer ON cave_reviews (reviewer_id);")

    (14 . "-- Concerns
CREATE TABLE cave_concerns (
  id BIGSERIAL PRIMARY KEY,
  review_id BIGINT NOT NULL REFERENCES cave_reviews(id) ON DELETE CASCADE,
  changeset_id BIGINT NOT NULL REFERENCES cave_changesets(id) ON DELETE CASCADE,
  author_id BIGINT NOT NULL REFERENCES cave_users(id),
  body TEXT NOT NULL,
  status VARCHAR(16) NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'resolved')),
  resolved_by_id BIGINT REFERENCES cave_users(id),
  resolved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_concerns_changeset ON cave_concerns (changeset_id);
CREATE INDEX idx_concerns_status ON cave_concerns (changeset_id, status);")

    (15 . "-- Deploy secrets (encrypted)
CREATE TABLE cave_deploy_secrets (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  encrypted_value TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, name)
);")

    (16 . "-- Deploy records
CREATE TABLE cave_deploys (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  commit_sha VARCHAR(64) NOT NULL,
  image_tag VARCHAR(256) NOT NULL,
  previous_image_tag VARCHAR(256),
  status VARCHAR(32) NOT NULL DEFAULT 'pending'
    CHECK (status IN ('pending', 'building', 'running', 'succeeded', 'failed', 'rolled_back')),
  log TEXT,
  triggered_by_id BIGINT REFERENCES cave_users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  finished_at TIMESTAMPTZ
);
CREATE INDEX idx_deploys_repo ON cave_deploys (repo_id);
CREATE INDEX idx_deploys_status ON cave_deploys (repo_id, status);")

    (17 . "-- Event log (instrumentation)
CREATE TABLE cave_events (
  id BIGSERIAL PRIMARY KEY,
  event_type VARCHAR(64) NOT NULL,
  user_id BIGINT,
  repo_id BIGINT,
  entity_type VARCHAR(32),
  entity_id BIGINT,
  metadata JSONB,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_events_type ON cave_events (event_type);
CREATE INDEX idx_events_repo ON cave_events (repo_id);
CREATE INDEX idx_events_created ON cave_events (created_at);")

    (18 . "-- Sessions (web login)
CREATE TABLE cave_sessions (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  session_token VARCHAR(128) NOT NULL UNIQUE,
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_sessions_token ON cave_sessions (session_token);
CREATE INDEX idx_sessions_expires ON cave_sessions (expires_at);")

    (19 . "-- Check configs (server-side, per-repo)
CREATE TABLE cave_check_configs (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  command TEXT NOT NULL,
  timeout_seconds INTEGER NOT NULL DEFAULT 60,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, name)
);
-- Whether repo-stored checks (.cave/checks.toml) are allowed
ALTER TABLE cave_repos ADD COLUMN allow_repo_checks BOOLEAN NOT NULL DEFAULT FALSE;")

    (20 . "-- Allow repos to be owned by a user instead of an org
ALTER TABLE cave_repos ALTER COLUMN org_id DROP NOT NULL;
ALTER TABLE cave_repos ADD COLUMN owner_id BIGINT REFERENCES cave_users(id) ON DELETE CASCADE;
ALTER TABLE cave_repos ADD CONSTRAINT repo_has_owner CHECK (
  (org_id IS NOT NULL AND owner_id IS NULL) OR
  (org_id IS NULL AND owner_id IS NOT NULL)
);
CREATE INDEX idx_repos_owner ON cave_repos (owner_id);
CREATE UNIQUE INDEX idx_repos_owner_name ON cave_repos (owner_id, name) WHERE owner_id IS NOT NULL;")

    (21 . "-- OIDC: add oidc_sub, make password_hash nullable
ALTER TABLE cave_users ADD COLUMN oidc_sub VARCHAR(256);
CREATE UNIQUE INDEX idx_users_oidc_sub ON cave_users (oidc_sub) WHERE oidc_sub IS NOT NULL;
ALTER TABLE cave_users ALTER COLUMN password_hash DROP NOT NULL;")

    (22 . "-- Issue comments
CREATE TABLE cave_issue_comments (
  id BIGSERIAL PRIMARY KEY,
  issue_id BIGINT NOT NULL REFERENCES cave_issues(id) ON DELETE CASCADE,
  author_id BIGINT NOT NULL REFERENCES cave_users(id),
  body TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_issue_comments_issue ON cave_issue_comments (issue_id);")

    (23 . "-- Inline diff comments on pull requests
CREATE TABLE cave_diff_comments (
  id BIGSERIAL PRIMARY KEY,
  changeset_id BIGINT NOT NULL REFERENCES cave_changesets(id) ON DELETE CASCADE,
  author_id BIGINT NOT NULL REFERENCES cave_users(id),
  file_path VARCHAR(1024) NOT NULL,
  line_number INTEGER NOT NULL,
  side VARCHAR(3) NOT NULL DEFAULT 'new' CHECK (side IN ('old', 'new')),
  body TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_diff_comments_changeset ON cave_diff_comments (changeset_id);
CREATE INDEX idx_diff_comments_file ON cave_diff_comments (changeset_id, file_path);")

    (24 . "-- Repo archival
ALTER TABLE cave_repos ADD COLUMN is_archived BOOLEAN NOT NULL DEFAULT FALSE;")

    (25 . "-- Repo mirrors
CREATE TABLE cave_repo_mirrors (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  direction VARCHAR(4) NOT NULL CHECK (direction IN ('push', 'pull')),
  remote_url TEXT NOT NULL,
  auth_token TEXT,
  interval_minutes INTEGER NOT NULL DEFAULT 60,
  last_sync_at TIMESTAMPTZ,
  last_error TEXT,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_mirrors_repo ON cave_repo_mirrors (repo_id);
CREATE INDEX idx_mirrors_direction ON cave_repo_mirrors (direction, enabled);")

    (26 . "-- User themes
ALTER TABLE cave_users ADD COLUMN theme VARCHAR(64) NOT NULL DEFAULT 'terminal-warmth';
CREATE TABLE cave_user_themes (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  name VARCHAR(64) NOT NULL,
  definition TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(user_id, name)
);")

    (27 . "-- Webhooks
CREATE TABLE cave_webhooks (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  url TEXT NOT NULL,
  secret VARCHAR(256),
  events TEXT NOT NULL DEFAULT 'push,pull_request,issue',
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  last_status INTEGER,
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_webhooks_repo ON cave_webhooks (repo_id);")

    (28 . "-- Commit statuses (external CI reports)
CREATE TABLE cave_commit_statuses (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  commit_sha VARCHAR(64) NOT NULL,
  state VARCHAR(16) NOT NULL CHECK (state IN ('pending', 'success', 'failure', 'error')),
  context VARCHAR(128) NOT NULL DEFAULT 'default',
  description TEXT,
  target_url TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_commit_statuses_repo ON cave_commit_statuses (repo_id, commit_sha);
CREATE UNIQUE INDEX idx_commit_statuses_unique ON cave_commit_statuses (repo_id, commit_sha, context);")

    (29 . "-- Automation definitions
CREATE TABLE cave_automation_definitions (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  trigger VARCHAR(32) NOT NULL,
  command TEXT NOT NULL,
  runner_labels TEXT DEFAULT '',
  timeout_seconds INTEGER NOT NULL DEFAULT 60,
  concurrency_key VARCHAR(256),
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  source VARCHAR(8) NOT NULL DEFAULT 'ui' CHECK (source IN ('ui', 'repo')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, name)
);
CREATE INDEX idx_automation_defs_repo ON cave_automation_definitions (repo_id);
CREATE INDEX idx_automation_defs_trigger ON cave_automation_definitions (repo_id, trigger);")

    (30 . "-- Automation runs
CREATE TABLE cave_automation_runs (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  definition_id BIGINT REFERENCES cave_automation_definitions(id) ON DELETE SET NULL,
  definition_name VARCHAR(128) NOT NULL,
  trigger_event VARCHAR(32) NOT NULL,
  commit_sha VARCHAR(64),
  ref VARCHAR(256),
  status VARCHAR(16) NOT NULL DEFAULT 'queued'
    CHECK (status IN ('queued','assigned','running','success','failure','cancelled','timed_out')),
  runner_id BIGINT,
  triggered_by_id BIGINT REFERENCES cave_users(id),
  log TEXT DEFAULT '',
  started_at TIMESTAMPTZ,
  finished_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_automation_runs_repo ON cave_automation_runs (repo_id);
CREATE INDEX idx_automation_runs_status ON cave_automation_runs (status);")

    (31 . "-- Runners and registration tokens
CREATE TABLE cave_runners (
  id BIGSERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL,
  scope VARCHAR(8) NOT NULL DEFAULT 'instance' CHECK (scope IN ('instance', 'org', 'repo')),
  scope_id BIGINT,
  labels TEXT DEFAULT '',
  auth_token VARCHAR(256) NOT NULL UNIQUE,
  ephemeral BOOLEAN NOT NULL DEFAULT FALSE,
  status VARCHAR(16) NOT NULL DEFAULT 'offline' CHECK (status IN ('online', 'offline', 'disabled')),
  last_seen_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE cave_runner_registration_tokens (
  id BIGSERIAL PRIMARY KEY,
  token VARCHAR(256) NOT NULL UNIQUE,
  scope VARCHAR(8) NOT NULL DEFAULT 'instance',
  scope_id BIGINT,
  created_by_id BIGINT REFERENCES cave_users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMPTZ
);")

    (32 . "-- Add 'user' scope for personal runners
ALTER TABLE cave_runners DROP CONSTRAINT IF EXISTS cave_runners_scope_check;
ALTER TABLE cave_runners ADD CONSTRAINT cave_runners_scope_check CHECK (scope IN ('instance', 'org', 'repo', 'user'));")

    (33 . "-- Workflow runs
CREATE TABLE cave_workflow_runs (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  workflow_name VARCHAR(256) NOT NULL,
  workflow_file VARCHAR(512) NOT NULL,
  trigger_event VARCHAR(32) NOT NULL,
  commit_sha VARCHAR(64),
  ref VARCHAR(256),
  status VARCHAR(16) NOT NULL DEFAULT 'queued'
    CHECK (status IN ('queued','running','success','failure','cancelled')),
  triggered_by_id BIGINT REFERENCES cave_users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  started_at TIMESTAMPTZ,
  finished_at TIMESTAMPTZ
);
CREATE INDEX idx_workflow_runs_repo ON cave_workflow_runs (repo_id);")

    (34 . "-- Workflow jobs
CREATE TABLE cave_workflow_jobs (
  id BIGSERIAL PRIMARY KEY,
  workflow_run_id BIGINT NOT NULL REFERENCES cave_workflow_runs(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  image VARCHAR(512) NOT NULL,
  needs TEXT DEFAULT '',
  status VARCHAR(16) NOT NULL DEFAULT 'queued'
    CHECK (status IN ('queued','blocked','assigned','running','success','failure','cancelled','skipped')),
  runner_id BIGINT,
  started_at TIMESTAMPTZ,
  finished_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_workflow_jobs_run ON cave_workflow_jobs (workflow_run_id);")

    (35 . "-- Workflow steps
CREATE TABLE cave_workflow_steps (
  id BIGSERIAL PRIMARY KEY,
  job_id BIGINT NOT NULL REFERENCES cave_workflow_jobs(id) ON DELETE CASCADE,
  step_order INTEGER NOT NULL,
  name VARCHAR(256),
  command TEXT NOT NULL,
  status VARCHAR(16) NOT NULL DEFAULT 'pending'
    CHECK (status IN ('pending','running','success','failure','skipped')),
  log TEXT DEFAULT '',
  exit_code INTEGER,
  started_at TIMESTAMPTZ,
  finished_at TIMESTAMPTZ
);
CREATE INDEX idx_workflow_steps_job ON cave_workflow_steps (job_id);")

    (36 . "ALTER TABLE cave_workflow_jobs ADD COLUMN runs_on TEXT DEFAULT '';")

    (37 . "ALTER TABLE cave_workflow_jobs ADD COLUMN timeout_seconds INTEGER NOT NULL DEFAULT 0;
ALTER TABLE cave_workflow_steps ADD COLUMN timeout_seconds INTEGER NOT NULL DEFAULT 0;")

    (38 . "ALTER TABLE cave_workflow_jobs ADD COLUMN continue_on_error BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE cave_workflow_steps ADD COLUMN continue_on_error BOOLEAN NOT NULL DEFAULT FALSE;")

    (39 . "CREATE TABLE cave_chamber_nodes (
  id BIGSERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL UNIQUE,
  address VARCHAR(256) NOT NULL,
  status VARCHAR(16) NOT NULL DEFAULT 'healthy'
    CHECK (status IN ('healthy', 'suspect', 'dead')),
  last_seen_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);")

    (40 . "CREATE TABLE cave_repo_assignments (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  node_id BIGINT NOT NULL REFERENCES cave_chamber_nodes(id) ON DELETE CASCADE,
  role VARCHAR(8) NOT NULL DEFAULT 'primary'
    CHECK (role IN ('primary', 'secondary')),
  generation BIGINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, node_id)
);
CREATE INDEX idx_repo_assignments_repo ON cave_repo_assignments (repo_id);
CREATE INDEX idx_repo_assignments_node ON cave_repo_assignments (node_id);
CREATE UNIQUE INDEX idx_repo_assignments_primary
  ON cave_repo_assignments (repo_id) WHERE role = 'primary';")

    (41 . "ALTER TABLE cave_repos ADD COLUMN last_pushed_at TIMESTAMPTZ;
UPDATE cave_repos SET last_pushed_at = updated_at WHERE last_pushed_at IS NULL;")

    (42 . "-- Per-page-view rows for the repo Pulse tab. Aggregated at query time.
CREATE TABLE cave_page_views (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  ip_hash VARCHAR(64),         -- sha256(ip + secret), never the raw IP
  user_id BIGINT,              -- NULL for anon
  referer_host VARCHAR(256),   -- hostname only, NULL when none
  viewed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_page_views_repo_time ON cave_page_views (repo_id, viewed_at);
CREATE INDEX idx_page_views_time ON cave_page_views (viewed_at);")

    (43 . "-- Releases: each row keyed by repo + git tag name.
CREATE TABLE cave_releases (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  tag_name VARCHAR(256) NOT NULL,
  name VARCHAR(512),
  body TEXT,
  is_prerelease BOOLEAN NOT NULL DEFAULT FALSE,
  is_draft BOOLEAN NOT NULL DEFAULT FALSE,
  created_by BIGINT REFERENCES cave_users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  published_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, tag_name)
);
CREATE INDEX idx_releases_repo ON cave_releases (repo_id, published_at DESC);

CREATE TABLE cave_release_assets (
  id BIGSERIAL PRIMARY KEY,
  release_id BIGINT NOT NULL REFERENCES cave_releases(id) ON DELETE CASCADE,
  name VARCHAR(512) NOT NULL,
  content_type VARCHAR(256),
  size BIGINT NOT NULL,
  storage_path VARCHAR(1024) NOT NULL,
  download_count BIGINT NOT NULL DEFAULT 0,
  uploaded_by BIGINT REFERENCES cave_users(id),
  uploaded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(release_id, name)
);
CREATE INDEX idx_release_assets_release ON cave_release_assets (release_id);")

    (44 . "-- Commit signature verification cache, populated by the post-receive hook.
CREATE TABLE cave_commit_signatures (
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  commit_sha VARCHAR(64) NOT NULL,
  verified BOOLEAN NOT NULL,
  scheme VARCHAR(16),         -- 'ssh' or 'gpg', NULL for unsigned
  fingerprint VARCHAR(128),   -- key fingerprint when known
  signer_user_id BIGINT REFERENCES cave_users(id),
  signed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (repo_id, commit_sha)
);
CREATE INDEX idx_commit_sigs_repo ON cave_commit_signatures (repo_id);")

    (45 . "-- Admin approval gate for self-registration. Existing rows default
-- to 'approved' so the migration doesn't lock anyone out. New OIDC users
-- land as 'pending' (set explicitly by provision-oidc-user).
ALTER TABLE cave_users ADD COLUMN approval_status TEXT NOT NULL DEFAULT 'approved';
CREATE INDEX idx_users_approval_pending ON cave_users (approval_status) WHERE approval_status = 'pending';")

    (46 . "-- Resolved dependency graph, populated from SBOMs on push (default ref).
CREATE TABLE cave_repo_deps (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  ref VARCHAR(256) NOT NULL,
  manifest_path VARCHAR(1024) NOT NULL,
  ecosystem VARCHAR(64) NOT NULL,
  package_name VARCHAR(512) NOT NULL,
  version VARCHAR(256) NOT NULL,
  purl VARCHAR(1024) NOT NULL,
  is_direct BOOLEAN NOT NULL DEFAULT TRUE,
  scope VARCHAR(32),
  generation BIGINT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, ref, manifest_path, purl)
);
CREATE INDEX idx_repo_deps_repo ON cave_repo_deps (repo_id, ref);
CREATE INDEX idx_repo_deps_pkg ON cave_repo_deps (ecosystem, package_name);")

    (47 . "-- OSV advisories, mirrored by cave deps sync-advisories.
CREATE TABLE cave_advisories (
  id BIGSERIAL PRIMARY KEY,
  osv_id VARCHAR(64) NOT NULL UNIQUE,
  summary TEXT,
  details TEXT,
  aliases TEXT[] NOT NULL DEFAULT '{}',
  severity VARCHAR(16),
  cvss_score NUMERIC(3,1),
  refs JSONB NOT NULL DEFAULT '[]',
  published_at TIMESTAMPTZ,
  modified_at TIMESTAMPTZ,
  withdrawn_at TIMESTAMPTZ
);
CREATE INDEX idx_advisories_modified ON cave_advisories (modified_at);
CREATE INDEX idx_advisories_aliases ON cave_advisories USING GIN (aliases);
-- OSV affected ranges flattened to introduced/fixed pairs for SQL matching.
CREATE TABLE cave_advisory_affected (
  id BIGSERIAL PRIMARY KEY,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  ecosystem VARCHAR(64) NOT NULL,
  package_name VARCHAR(512) NOT NULL,
  range_type VARCHAR(16) NOT NULL,
  introduced VARCHAR(256),
  fixed VARCHAR(256),
  last_affected VARCHAR(256)
);
CREATE INDEX idx_advisory_affected_pkg ON cave_advisory_affected (ecosystem, package_name);")

    (48 . "-- A vulnerable dependency occurrence, recomputed by the matcher.
CREATE TABLE cave_dep_alerts (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  dep_id BIGINT NOT NULL REFERENCES cave_repo_deps(id) ON DELETE CASCADE,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  state VARCHAR(16) NOT NULL DEFAULT 'open'
    CHECK (state IN ('open', 'dismissed', 'fixed', 'auto_fixed')),
  fix_version VARCHAR(256),
  fix_kind VARCHAR(20)
    CHECK (fix_kind IS NULL OR fix_kind IN ('manifest', 'lockfile', 'override', 'transitive_parent', 'none')),
  fix_pr_id BIGINT REFERENCES cave_changesets(id) ON DELETE SET NULL,
  reachable BOOLEAN,
  detected_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(dep_id, advisory_id)
);
CREATE INDEX idx_dep_alerts_repo ON cave_dep_alerts (repo_id, state);")

    (49 . "-- Durable suppressions, keyed by stable coordinate so they survive
-- graph rebuilds and version churn.
CREATE TABLE cave_dep_suppressions (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  ecosystem VARCHAR(64) NOT NULL,
  package_name VARCHAR(512) NOT NULL,
  advisory_id BIGINT NOT NULL REFERENCES cave_advisories(id) ON DELETE CASCADE,
  reason VARCHAR(32) NOT NULL
    CHECK (reason IN ('not_used', 'no_fix', 'risk_accepted')),
  note TEXT,
  created_by BIGINT REFERENCES cave_users(id),
  expires_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(repo_id, ecosystem, package_name, advisory_id)
);
CREATE INDEX idx_dep_suppressions_repo ON cave_dep_suppressions (repo_id);
-- Org policy caps per-repo .cave/deps.yml. Repo config can only narrow it.
CREATE TABLE cave_org_dep_policy (
  org_id BIGINT PRIMARY KEY REFERENCES cave_orgs(id) ON DELETE CASCADE,
  allowed_ecosystems TEXT[],
  license_allow TEXT[],
  license_deny TEXT[],
  automerge_ceiling VARCHAR(8) NOT NULL DEFAULT 'none'
    CHECK (automerge_ceiling IN ('none', 'patch', 'minor', 'major')),
  security_always_on BOOLEAN NOT NULL DEFAULT TRUE,
  freeze_windows JSONB NOT NULL DEFAULT '[]',
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);")

    (50 . "ALTER TABLE cave_workflow_jobs ADD COLUMN privileged BOOLEAN NOT NULL DEFAULT FALSE;")
    (51 . "CREATE TABLE cave_dep_fix_attempts (
  id BIGSERIAL PRIMARY KEY,
  alert_id BIGINT NOT NULL REFERENCES cave_dep_alerts(id) ON DELETE CASCADE,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  branch VARCHAR(256) NOT NULL,
  commit_sha VARCHAR(64) NOT NULL,
  state VARCHAR(16) NOT NULL DEFAULT 'building'
    CHECK (state IN ('building','opened','build_failed','no_ci','error')),
  pr_id BIGINT REFERENCES cave_changesets(id) ON DELETE SET NULL,
  detail TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(alert_id)
);
CREATE INDEX idx_dep_fix_attempts_commit ON cave_dep_fix_attempts (repo_id, commit_sha)")
    (52 . "ALTER TABLE cave_org_dep_policy ADD COLUMN auto_fix_security BOOLEAN NOT NULL DEFAULT TRUE;")
    (53 . "-- OSV GIT-range advisories (e.g. the cl-sec Common Lisp feed) identify
-- affected software by a source repo plus a commit range, often with a null
-- package. Store the repo. Also cache each ocicl project's upstream repo, and
-- tag ocicl deps with their project so they can be linked to it (the dep
-- version already carries the commit).
ALTER TABLE cave_advisory_affected ADD COLUMN repo VARCHAR(512);
CREATE INDEX idx_advisory_affected_repo ON cave_advisory_affected (repo);
ALTER TABLE cave_repo_deps ADD COLUMN ocicl_project VARCHAR(256);
CREATE TABLE cave_ocicl_projects (
  name VARCHAR(256) PRIMARY KEY,
  source_repo VARCHAR(512),
  source_commit VARCHAR(64),
  systems TEXT[] NOT NULL DEFAULT '{}',
  resolved_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);")
    (54 . "-- Last-run timestamps for the in-process periodic scheduler. The
-- conditional UPDATE ... RETURNING acts as an atomic lease so only one server
-- instance runs each task per interval.
CREATE TABLE cave_scheduler_runs (
  task_name VARCHAR(64) PRIMARY KEY,
  last_run_at TIMESTAMPTZ
);")

    (55 . "-- Registered GPG public keys, used to verify GPG-signed commits.
-- Mirrors cave_ssh_keys. key_id holds the primary-key fingerprint (upper hex).
CREATE TABLE cave_gpg_keys (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  name VARCHAR(128) NOT NULL,
  public_key TEXT NOT NULL,
  key_id VARCHAR(64) NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_gpg_keys_keyid ON cave_gpg_keys (key_id);
CREATE INDEX idx_gpg_keys_user_id ON cave_gpg_keys (user_id);")

    (56 . "-- In-app notifications, repo watches, and issue milestones.
CREATE TABLE cave_notifications (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  repo_id BIGINT REFERENCES cave_repos(id) ON DELETE CASCADE,
  kind VARCHAR(32) NOT NULL,
  subject VARCHAR(512) NOT NULL,
  link VARCHAR(1024) NOT NULL,
  is_read BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_notifications_user ON cave_notifications (user_id, is_read, created_at DESC);

CREATE TABLE cave_repo_watches (
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES cave_users(id) ON DELETE CASCADE,
  PRIMARY KEY (repo_id, user_id)
);

CREATE TABLE cave_milestones (
  id BIGSERIAL PRIMARY KEY,
  repo_id BIGINT NOT NULL REFERENCES cave_repos(id) ON DELETE CASCADE,
  title VARCHAR(256) NOT NULL,
  description TEXT,
  due_on TIMESTAMPTZ,
  state VARCHAR(8) NOT NULL DEFAULT 'open' CHECK (state IN ('open', 'closed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_milestones_repo ON cave_milestones (repo_id, state);

ALTER TABLE cave_issues ADD COLUMN milestone_id BIGINT REFERENCES cave_milestones(id) ON DELETE SET NULL;"))
  "Ordered list of (version . sql) migration pairs.")

(defun current-schema-version ()
  "Get the current schema version from the database. Returns 0 if table doesn't exist."
  (handler-case
      (let ((result (postmodern:query
                     "SELECT COALESCE(MAX(version), 0) FROM cave_schema_version"
                     :single)))
        (or result 0))
    (error () 0)))

(defun split-sql-statements (sql)
  "Split a SQL string into individual statements on semicolons.
   Naive split — doesn't handle semicolons inside strings, but fine for DDL."
  (let ((stmts (uiop:split-string sql :separator ";")))
    (remove-if (lambda (s) (every (lambda (c) (member c '(#\Space #\Newline #\Tab #\Return)))
                                  s))
               (mapcar (lambda (s) (string-trim '(#\Space #\Newline #\Tab #\Return) s))
                       stmts))))

(defun run-migrations ()
  "Run all pending database migrations."
  (let ((current (current-schema-version))
        (applied 0))
    (llog:info "Current schema version" :version current)
    (dolist (migration *migrations*)
      (destructuring-bind (version . sql) migration
        (when (> version current)
          (llog:info "Applying migration" :version version)
          (dolist (stmt (split-sql-statements sql))
            (postmodern:execute stmt))
          (postmodern:execute
           (format nil "INSERT INTO cave_schema_version (version) VALUES (~A)" version))
          (incf applied))))
    (if (zerop applied)
        (llog:info "Database is up to date" :version (length *migrations*))
        (llog:info "Migrations applied" :count applied :version (length *migrations*)))
    applied))

(defun expected-schema-version ()
  "The schema version this binary expects."
  (first (first (last *migrations*))))

(defun check-schema-version ()
  "Ensure the DB schema matches what this binary expects. Signal error if not."
  (let ((current (current-schema-version))
        (expected (expected-schema-version)))
    (unless (= current expected)
      (error "Database schema version mismatch: have ~A, need ~A. ~
              Run 'cave-server migrate' to update." current expected))))
