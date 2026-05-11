# Proposed Features and Improvements for Cave

Based on a review of the current codebase and the project's documentation (PRD and README), here are the recommended next steps for development.

## 1. Functional Improvements & "Finishing" Features

*   **Complete the Workflow System:** 
    *   **Current State:** `src/workflow.lisp` parses GitHub-style YAML workflows, but the gRPC runner service only handles basic `automation-runs`.
    *   **Improvement:** Update the runner gRPC service and the runner agent to fetch and execute `workflow-jobs`. This enables job dependencies (the `needs` keyword) and multi-step CI/CD.
*   **Implement Stack Landing:**
    *   **Current State:** Stacks are tracked and displayed, but the "Landing Algorithm" (rebasing the entire stack onto the base branch and merging as a unit) is not yet implemented.
    *   **Improvement:** Add a "Land Stack" action in the PR view to execute the multi-step rebase and merge logic across all dependent changesets.
*   **Implement Container Deployment (PaaS):**
    *   **Current State:** README lists this as a feature and DB tables exist, but the implementation is missing.
    *   **Improvement:** Implement the P0 deployment sequence: building images via Podman, managing container lifecycles (stop/rm/run), and handling secrets.

## 2. Code Browser Enhancements

*   **Git Blame View:**
    *   **Improvement:** Integrate `git blame` into the backend and add a UI view to show which commits and authors last touched each line of a file.
*   **Global & Repo-Scoped Search:**
    *   **Improvement:** Implement code search using `git grep` and metadata search (issues/PRs) using PostgreSQL's full-text search capabilities.

## 3. Security & Administration

*   **Formal Audit Logs:**
    *   **Improvement:** Create a dedicated, immutable audit log for administrative actions (permission changes, user management, secret updates) to satisfy compliance needs for small teams.
*   **GPG Signature Verification:**
    *   **Improvement:** Support GPG key uploads and display "Verified" badges for signed commits in the commit log and PR views.

## 4. UI/UX & Real-time Features

*   **Real-time Updates:**
    *   **Improvement:** Use Server-Sent Events (SSE) to provide real-time streaming of runner logs and CI status updates on the PR and Automation pages.
*   **Dark Mode / Theme Toggle:**
    *   **Improvement:** Add a quick-access toggle in the header to switch between light and dark themes or sync with system-level preferences.

## 5. Maintenance

*   **Documentation Alignment:** 
    *   **Action:** Sync the `README.md` with the current implementation status, specifically regarding "Podman deployment" and "Webhooks" to ensure user expectations match reality.
*   **Automated Migration Testing:**
    *   **Action:** Add tests that specifically verify the database migration path from version 1 to the current version.
