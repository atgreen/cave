;;; job-tokens.lisp — per-job git credentials (cave-72c)
;;;
;;; A workflow job that checks out its own repo has to authenticate like anyone
;;; else, and a private repo answers an anonymous fetch with 404. The runner
;;; used to pass an empty job token, so every private checkout failed with
;;; "repository not found". These cover the credential itself: minted per job,
;;; scoped to that job's repo, read-only, and dead once the job is.
;;;
;;; Needs a throwaway PostgreSQL (it runs every migration and writes fixtures):
;;;   podman run -d --name cave-test-pg -p 55432:5432 \
;;;     -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
;;;     docker.io/library/postgres:16-alpine
;;;   make test-job-tokens
;;;
;;; SPDX-License-Identifier: MIT

(asdf:load-system :cave)

(in-package #:cl-user)

(defparameter *test-db-port*
  (parse-integer (or (uiop:getenv "CAVE_TEST_DB_PORT") "55432"))
  "Port of the throwaway PostgreSQL this suite writes to.")

(defun connect-test-db ()
  (cave::connect-db :host "localhost" :port *test-db-port*
                    :name "cave" :user "cave" :password "cave"))

(defparameter +user-id+ 999101)
(defparameter +repo-id+ 999101)
(defparameter +other-repo-id+ 999102)
(defparameter +run-id+ 999101)
(defparameter +job-id+ 999101)

(defun seed-job ()
  "One private repo with a queued job, plus a second repo to prove scoping."
  (postmodern:execute "DELETE FROM cave_job_tokens WHERE job_id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_jobs WHERE id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_runs WHERE id = $1" +run-id+)
  (postmodern:execute "DELETE FROM cave_repos WHERE id IN ($1, $2)"
                      +repo-id+ +other-repo-id+)
  (postmodern:execute
   "INSERT INTO cave_users (id, username) VALUES ($1, 'job-token-test-user')
    ON CONFLICT (id) DO NOTHING"
   +user-id+)
  (postmodern:execute
   "INSERT INTO cave_repos (id, name, owner_id, is_private)
    VALUES ($1, 'job-token-test-repo', $2, true)"
   +repo-id+ +user-id+)
  (postmodern:execute
   "INSERT INTO cave_repos (id, name, owner_id, is_private)
    VALUES ($1, 'job-token-other-repo', $2, true)"
   +other-repo-id+ +user-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_runs (id, repo_id, workflow_name, workflow_file,
                                    trigger_event, status, created_at)
    VALUES ($1, $2, 'job-token-test', 'job-token-test.yml', 'push', 'running', now())"
   +run-id+ +repo-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_jobs (id, workflow_run_id, name, image, status, created_at)
    VALUES ($1, $2, 'scan', 'test-image', 'running', now())"
   +job-id+ +run-id+))

(defun cleanup-fixture ()
  (postmodern:execute "DELETE FROM cave_job_tokens WHERE job_id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_jobs WHERE id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_runs WHERE id = $1" +run-id+)
  (postmodern:execute "DELETE FROM cave_repos WHERE id IN ($1, $2)"
                      +repo-id+ +other-repo-id+)
  (postmodern:execute "DELETE FROM cave_users WHERE id = $1" +user-id+))

(defun basic-header (user &optional (password ""))
  (format nil "Basic ~A"
          (cl-base64:string-to-base64-string (format nil "~A:~A" user password))))

(connect-test-db)
(cave::run-migrations)

(unwind-protect
     (progn
       (seed-job)

       ;; --- The credential round-trips, and carries its scope with it ---
       (let* ((token (cave::create-job-token +job-id+ +repo-id+))
              (claim (cave::validate-job-token token)))
         (assert (and token (plusp (length token))) ()
                 "create-job-token should return a usable token, got ~S" token)
         (assert claim () "a freshly minted token should validate, got ~S" claim)
         (assert (= (getf claim :job-id) +job-id+) ()
                 "token should carry its job, got ~S" (getf claim :job-id))
         (assert (= (getf claim :repo-id) +repo-id+) ()
                 "token should carry its repo, got ~S" (getf claim :repo-id))

         ;; --- Only the hash is stored, so a database leak is not a credential leak ---
         (let ((stored (postmodern:query
                        "SELECT token_hash FROM cave_job_tokens WHERE job_id = $1"
                        +job-id+ :single)))
           (assert (not (equal stored token)) ()
                   "the clear token must not be stored, found ~S" stored))

         ;; --- Scoped to one repo: it is not a key to the rest of the instance ---
         (assert (cave::job-token-grants-repo-p claim +repo-id+) ()
                 "token should grant its own repo")
         (assert (not (cave::job-token-grants-repo-p claim +other-repo-id+)) ()
                 "token must not grant a different repo")

         ;; --- Garbage does not validate ---
         (assert (not (cave::validate-job-token "cavjt_not-a-real-token")) ()
                 "an unknown token must not validate")
         (assert (not (cave::validate-job-token "")) ()
                 "an empty token must not validate - the old runner sent exactly this")
         (assert (not (cave::validate-job-token nil)) ()
                 "a missing token must not validate"))

       ;; --- Expiry: a token outlives neither its ttl nor its job ---
       (let ((token (cave::create-job-token +job-id+ +repo-id+ :ttl-seconds 1)))
         (postmodern:execute
          "UPDATE cave_job_tokens SET expires_at = now() - interval '1 second'
           WHERE job_id = $1" +job-id+)
         (assert (not (cave::validate-job-token token)) ()
                 "an expired token must not validate"))

       (let ((token (cave::create-job-token +job-id+ +repo-id+)))
         (cave::revoke-job-tokens +job-id+)
         (assert (not (cave::validate-job-token token)) ()
                 "a revoked token must not validate - jobs end, credentials go with them"))

       ;; --- How the credential arrives: cave embeds it as the URL userinfo,
       ;;     so git presents it as the Basic auth username ---
       (let ((token (cave::create-job-token +job-id+ +repo-id+)))
         (assert (equal token (cave::request-job-token-string (basic-header token))) ()
                 "a token sent as the Basic username should be read back")
         (assert (equal token (cave::request-job-token-string
                               (basic-header "x-access-token" token)))
                 () "a token sent as the Basic password should be read back")
         (assert (null (cave::request-job-token-string nil)) ()
                 "no header means no token")
         (assert (null (cave::request-job-token-string "Bearer something")) ()
                 "a bearer header is an API token, not a job token")
         (assert (null (cave::request-job-token-string "Basic !!!not-base64!!!")) ()
                 "a malformed header must not signal, just decline"))

       ;; --- The decision the git-http route actually makes ---
       (let* ((token (cave::create-job-token +job-id+ +repo-id+))
              (repo (cave::find-repo-by-id +repo-id+))
              (other (cave::find-repo-by-id +other-repo-id+))
              (header (basic-header token)))
         (assert (cave::git-http-job-token-repo-p repo header) ()
                 "a job's own private repo must be fetchable with its token")
         (assert (not (cave::git-http-job-token-repo-p other header)) ()
                 "the token must not open a different private repo")
         (assert (not (cave::git-http-job-token-repo-p repo nil)) ()
                 "no credentials means no access - this is the anonymous fetch ~
                  that used to 404 the runner")
         (assert (not (cave::git-http-job-token-repo-p repo (basic-header "")))
                 () "an empty Basic username must not grant anything")
         (cave::revoke-job-tokens +job-id+)
         (assert (not (cave::git-http-job-token-repo-p repo header)) ()
                 "once the job ends its token opens nothing"))

       (format t "~&Job token tests passed.~%"))
  (cleanup-fixture))
