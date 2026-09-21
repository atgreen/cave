;;; runner-cleanup.lisp — what happens to a runner that stops heartbeating (cave-wb4)
;;;
;;; A runner that misses its heartbeat for 60 seconds used to be marked offline
;;; and then deleted outright. Deleting the row destroys the auth token the
;;; runner is still holding, so every later WatchTasks call from that runner was
;;; rejected; the client registers only once at process start, so it could never
;;; recover. One cave restart therefore stranded the whole fleet until a human
;;; restarted every runner container - which is exactly what happened on coop on
;;; 2026-09-20, and again on each redeploy after it.
;;;
;;; Ephemeral runners are a different case and are reaped separately.
;;;
;;; Needs a throwaway PostgreSQL (it runs every migration and writes fixtures):
;;;   podman run -d --name cave-test-pg -p 55432:5432 \
;;;     -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
;;;     docker.io/library/postgres:16-alpine
;;;   make test-runner-cleanup
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

(defparameter +user-id+ 999301)
(defparameter +repo-id+ 999301)
(defparameter +run-id+ 999301)
(defparameter +job-id+ 999301)
(defparameter +runner-id+ 999301)

(defun seed-runner (&key (ephemeral nil) (seen-seconds-ago 0) (status "online"))
  "One runner, last seen SEEN-SECONDS-AGO, holding a known auth token."
  (postmodern:execute "DELETE FROM cave_runners WHERE id = $1" +runner-id+)
  (postmodern:execute
   "INSERT INTO cave_runners (id, name, scope, auth_token, status, ephemeral, last_seen_at)
    VALUES ($1, 'cleanup-test-runner', 'instance', 'cleanup-test-token', $2, $3,
            now() - make_interval(secs => $4::int))"
   +runner-id+ status ephemeral seen-seconds-ago))

(defun seed-assigned-job ()
  "A job claimed by that runner, so the requeue path is exercised too."
  (postmodern:execute "DELETE FROM cave_workflow_jobs WHERE id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_runs WHERE id = $1" +run-id+)
  (postmodern:execute
   "INSERT INTO cave_users (id, username) VALUES ($1, 'cleanup-test-user')
    ON CONFLICT (id) DO NOTHING"
   +user-id+)
  (postmodern:execute
   "INSERT INTO cave_repos (id, name, owner_id) VALUES ($1, 'cleanup-test-repo', $2)
    ON CONFLICT (id) DO NOTHING"
   +repo-id+ +user-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_runs (id, repo_id, workflow_name, workflow_file,
                                    trigger_event, status, created_at)
    VALUES ($1, $2, 'cleanup-test', 'cleanup-test.yml', 'push', 'running', now())"
   +run-id+ +repo-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_jobs (id, workflow_run_id, name, image, status,
                                    runner_id, created_at, assigned_at)
    VALUES ($1, $2, 'scan', 'img', 'assigned', $3, now(), now())"
   +job-id+ +run-id+ +runner-id+))

(defun runner-row ()
  (postmodern:query
   "SELECT status, auth_token FROM cave_runners WHERE id = $1" +runner-id+ :row))

(defun cleanup-fixture ()
  (postmodern:execute "DELETE FROM cave_workflow_jobs WHERE id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_runs WHERE id = $1" +run-id+)
  (postmodern:execute "DELETE FROM cave_runners WHERE id = $1" +runner-id+)
  (postmodern:execute "DELETE FROM cave_repos WHERE id = $1" +repo-id+)
  (postmodern:execute "DELETE FROM cave_users WHERE id = $1" +user-id+))

(connect-test-db)
(cave::run-migrations)

(unwind-protect
     (progn
       ;; --- The outage: a missed heartbeat must not destroy the runner's identity ---
       (seed-runner :seen-seconds-ago 120)
       (cave::cleanup-offline-runners)
       (destructuring-bind (&optional status token) (or (runner-row) '())
         (assert status ()
                 "a runner that missed its heartbeat must keep its row - deleting it ~
                  destroys the auth token the runner still holds, and the client ~
                  registers only once, so it can never come back")
         (assert (equal status "offline") ()
                 "a runner past the heartbeat window should be marked offline, got ~S"
                 status)
         (assert (equal token "cleanup-test-token") ()
                 "its auth token must survive, or reconnecting cannot work, got ~S"
                 token))

       ;; --- Which means it can authenticate again the moment it reconnects ---
       (assert (cave::authenticate-runner "cleanup-test-token") ()
               "an offline runner's token must still authenticate: that is what lets ~
                it rejoin by itself after the server restarts")

       ;; --- A live runner is left alone ---
       (seed-runner :seen-seconds-ago 5)
       (cave::cleanup-offline-runners)
       (destructuring-bind (&optional status token) (or (runner-row) '())
         (declare (ignore token))
         (assert (equal status "online") ()
                 "a runner heartbeating normally must stay online, got ~S" status))

       ;; --- Work claimed by a runner that went away still gets requeued ---
       (seed-runner :seen-seconds-ago 120)
       (seed-assigned-job)
       (cave::cleanup-offline-runners)
       (let ((job (postmodern:query
                   "SELECT status, runner_id FROM cave_workflow_jobs WHERE id = $1"
                   +job-id+ :row)))
         (assert (equal (first job) "queued") ()
                 "a job assigned to a vanished runner must be requeued, got ~S"
                 (first job))
         (assert (member (second job) '(nil :null)) ()
                 "requeue must release the claim, got ~S" (second job)))

       ;; --- Ephemeral runners are still reaped: they exist for one job ---
       (seed-runner :ephemeral t :seen-seconds-ago 180)
       (cave::cleanup-offline-runners)
       (assert (null (runner-row)) ()
               "a stale ephemeral runner should still be deleted")

       ;; --- Long-dead runners are eventually retired, so the table stays bounded ---
       (seed-runner :seen-seconds-ago (* 40 24 60 60) :status "offline")
       (cave::cleanup-offline-runners)
       (assert (null (runner-row)) ()
               "an offline runner untouched for well past the retention window ~
                should be retired - registration inserts a fresh row each time a ~
                runner process starts, so something has to bound the table")

       (format t "~&Runner cleanup tests passed.~%"))
  (cleanup-fixture))
