;;; reap-stale-jobs.lisp — regression tests for reap-stale-workflow-jobs
;;;
;;; Covers the wedge that took the coop runner fleet down: a job left 'assigned'
;;; to a runner that stays online and heartbeating but never starts it. The
;;; requeue branch knew about that case and the fail branch did not, so once the
;;; job exhausted its attempts it matched neither and blocked its runner via the
;;; one-task-per-runner check until the hard timeout.
;;;
;;; Needs a throwaway PostgreSQL (it runs every migration and writes fixtures):
;;;   podman run -d --name cave-test-pg -p 55432:5432 \
;;;     -e POSTGRES_USER=cave -e POSTGRES_PASSWORD=cave -e POSTGRES_DB=cave \
;;;     docker.io/library/postgres:16-alpine
;;;   make test-db
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

(defparameter +runner-id+ 999001)
(defparameter +run-id+ 999001)
(defparameter +job-id+ 999001)
(defparameter +repo-id+ 999001)
(defparameter +user-id+ 999001)

(defun seed-wedged-job (&key assigned-minutes-ago attempts)
  "Install one job ASSIGNED to an online, freshly-heartbeating runner, claimed
   ASSIGNED-MINUTES-AGO and never started. Replaces any previous fixture."
  (postmodern:execute "DELETE FROM cave_workflow_jobs WHERE id = $1" +job-id+)
  (postmodern:execute "DELETE FROM cave_workflow_runs WHERE id = $1" +run-id+)
  (postmodern:execute "DELETE FROM cave_runners WHERE id = $1" +runner-id+)
  (postmodern:execute
   "INSERT INTO cave_runners (id, name, scope, auth_token, status, last_seen_at)
    VALUES ($1, 'reap-test-runner', 'instance', 'reap-test-token', 'online', now())"
   +runner-id+)
  ;; cave_repos carries a repo_has_owner check, so the fixture needs an owner.
  (postmodern:execute
   "INSERT INTO cave_users (id, username) VALUES ($1, 'reap-test-user')
    ON CONFLICT (id) DO NOTHING"
   +user-id+)
  (postmodern:execute
   "INSERT INTO cave_repos (id, name, owner_id) VALUES ($1, 'reap-test-repo', $2)
    ON CONFLICT (id) DO NOTHING"
   +repo-id+ +user-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_runs (id, repo_id, workflow_name, workflow_file,
                                    trigger_event, status, created_at)
    VALUES ($1, $2, 'reap-test', 'reap-test.yml', 'push', 'running',
            now() - interval '90 minutes')"
   +run-id+ +repo-id+)
  (postmodern:execute
   "INSERT INTO cave_workflow_jobs (id, workflow_run_id, name, image, status,
                                    runner_id, started_at, created_at, attempts,
                                    assigned_at)
    VALUES ($1, $2, 'scan', 'test-image', 'assigned', $3, NULL,
            now() - interval '90 minutes', $4,
            now() - make_interval(mins => $5::int))"
   +job-id+ +run-id+ +runner-id+ attempts assigned-minutes-ago))

(defun sql-null-p (value)
  "True for a SQL NULL as postmodern hands it back (:NULL, not NIL)."
  (or (null value) (eq value :null)))

(defun job-row ()
  (postmodern:query
   "SELECT status, runner_id, attempts, assigned_at FROM cave_workflow_jobs WHERE id = $1"
   +job-id+ :row))

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
       ;; The wedge itself: attempts exhausted, runner alive. Before the fix this
       ;; matched neither branch and sat 'assigned' forever.
       (seed-wedged-job :assigned-minutes-ago 20 :attempts 3)
       (cave::reap-stale-workflow-jobs)
       (destructuring-bind (status runner-id attempts assigned-at) (job-row)
         (declare (ignore runner-id attempts assigned-at))
         (assert (equal status "failure") ()
                 "Job assigned to a live runner past grace with no attempts left ~
                  should be failed, got ~S" status))

       ;; Same job with retries left: requeued, and the claim fully released so
       ;; another runner can take it.
       (seed-wedged-job :assigned-minutes-ago 20 :attempts 0)
       (cave::reap-stale-workflow-jobs)
       (destructuring-bind (status runner-id attempts assigned-at) (job-row)
         (assert (equal status "queued") ()
                 "Job past grace with attempts left should be requeued, got ~S" status)
         (assert (sql-null-p runner-id) ()
                 "Requeue must clear runner_id, got ~S" runner-id)
         (assert (= attempts 1) () "Requeue must count the attempt, got ~S" attempts)
         (assert (sql-null-p assigned-at) ()
                 "Requeue must clear assigned_at so the retry gets a full grace ~
                  window, got ~S" assigned-at))

       ;; The grace clock runs from assignment, not from job creation. This job
       ;; was created 90 min ago but claimed 2 min ago, so it is still the
       ;; runner's to start — measuring from created_at would reap it instantly
       ;; and burn every attempt inside one grace window.
       (seed-wedged-job :assigned-minutes-ago 2 :attempts 0)
       (cave::reap-stale-workflow-jobs)
       (destructuring-bind (status runner-id attempts assigned-at) (job-row)
         (declare (ignore runner-id attempts assigned-at))
         (assert (equal status "assigned") ()
                 "Job claimed inside the grace window must be left alone, got ~S"
                 status))

       ;; A runner that has gone silent still loses its claim, grace or not.
       (seed-wedged-job :assigned-minutes-ago 1 :attempts 0)
       (postmodern:execute
        "UPDATE cave_runners SET last_seen_at = now() - interval '10 minutes' WHERE id = $1"
        +runner-id+)
       (cave::reap-stale-workflow-jobs)
       (destructuring-bind (status runner-id attempts assigned-at) (job-row)
         (declare (ignore runner-id attempts assigned-at))
         (assert (equal status "queued") ()
                 "Job held by a silent runner should be requeued, got ~S" status)))
  (cleanup-fixture)
  (cave::disconnect-db))

(format t "~&Stale workflow job reaping tests passed.~%")
