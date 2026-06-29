;;; runner-service.lisp — gRPC runner service for Cave automations
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; --- Proto message compilation ---
;;; Compile runner.proto at load time to generate message classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((proto-path (merge-pathnames "src/runner.proto" (asdf:system-source-directory :cave))))
    (when (probe-file proto-path)
      (ag-proto:compile-proto-file proto-path :load t :package (find-package :cave)))))

;;; --- gRPC Server ---

(defvar *grpc-server* nil "The Cave gRPC server instance.")

(defun get-runner-from-ctx (ctx)
  "Authenticate runner from gRPC context metadata. Returns runner plist or NIL."
  (let* ((auth (ag-grpc:context-metadata ctx "authorization"))
         (token (when (and auth (>= (length auth) 7)
                           (string-equal "Bearer " (subseq auth 0 7)))
                  (subseq auth 7))))
    (when token
      (postmodern:with-connection *db-spec*
        (authenticate-runner token)))))

(defun require-runner-from-ctx (ctx)
  "Authenticate runner metadata, or fail the RPC."
  (or (get-runner-from-ctx ctx)
      (error "unauthenticated runner")))

;;; --- RPC Handlers ---

(defun handle-register-runner (request ctx)
  "Register a new runner."
  ;; All DB access (incl. token validation) must run inside with-connection so it
  ;; uses a pooled, per-thread connection. Validating on the shared toplevel
  ;; connection raced with other gRPC handlers' queries (a PostgreSQL connection
  ;; is a single socket and not thread-safe), corrupting the result so a valid
  ;; token read back as NIL -> spurious "invalid registration token".
  (postmodern:with-connection *db-spec*
    (let* ((reg-token (slot-value request 'cave::registration-token))
           ;; Single-use: consume (delete) the token atomically so it can
           ;; register exactly one runner, even under concurrent calls.
           (token-record (consume-registration-token reg-token)))
      (unless token-record
        (error "invalid registration token"))
      (let ((runner (register-runner
                     :name (slot-value request 'cave::name)
                     :labels (slot-value request 'cave::runner-labels)
                     :ephemeral (slot-value request 'cave::ephemeral)
                     :scope (getf token-record :scope)
                     :scope-id (let ((sid (getf token-record :scope-id)))
                                 (unless (eq sid :null) sid)))))
        (llog:info "Runner registered" :name (getf runner :name)
                                       :id (getf runner :id))
        (make-instance 'cave::register-runner-response
                       :runner-id (getf runner :id)
                       :auth-token (getf runner :auth-token))))))

(defun handle-declare-runner (request ctx)
  "Runner heartbeat / declaration."
  (let ((runner-id (getf (get-runner-from-ctx ctx) :id)))
    (when runner-id
      (postmodern:with-connection *db-spec*
        (update-runner-heartbeat runner-id
                                 :labels (slot-value request 'cave::runner-labels)))))
  (make-instance 'cave::declare-runner-response :ok t))

(defun handle-fetch-task (request ctx)
  "Fetch a queued task for this runner."
  (declare (ignore request))
  (let ((runner-id (getf (get-runner-from-ctx ctx) :id)))
    (unless runner-id
      (return-from handle-fetch-task
        (make-instance 'cave::fetch-task-response :has-task nil)))
    (postmodern:with-connection *db-spec*
      (let* ((runner (postmodern:query
                      (:select '* :from 'cave-runners :where (:= 'id runner-id))
                      :plist))
             (run (when runner
                    (fetch-queued-run runner-id
                                     (getf runner :labels)
                                     (getf runner :scope)
                                     (let ((sid (getf runner :scope-id)))
                                       (unless (eq sid :null) sid))))))
        (if run
            (let* ((repo (find-repo-by-id (getf run :repo-id)))
                   (owner-name (when repo (repo-owner-name repo)))
                   (def-id (getf run :definition-id))
                   (def (when (and def-id (not (eq def-id :null)))
                          (postmodern:query
                           (:select '* :from 'cave-automation-definitions
                            :where (:= 'id def-id))
                           :plist)))
                   (command (if def (getf def :command) (getf run :definition-name))))
              (make-instance 'cave::fetch-task-response
                             :has-task t
                             :run-id (getf run :id)
                             :repo-owner (or owner-name "")
                             :repo-name (if repo (getf repo :name) "")
                             :command (or command "")
                             :commit-sha (let ((sha (getf run :commit-sha)))
                                           (if (eq sha :null) "" sha))
                             :ref (let ((r (getf run :ref)))
                                    (if (eq r :null) "" r))
                             :timeout-seconds (if def (getf def :timeout-seconds) 60)))
            (make-instance 'cave::fetch-task-response :has-task nil))))))

(defun handle-append-task-log (request ctx)
  "Append log chunk to a running task."
  (let ((runner (require-runner-from-ctx ctx)))
    (postmodern:with-connection *db-spec*
      (unless (append-run-log-for-runner (slot-value request 'cave::run-id)
                                         (getf runner :id)
                                         (slot-value request 'cave::chunk))
        (error "runner is not assigned to this task"))))
  (make-instance 'cave::append-task-log-response :ok t))

(defun handle-update-task-status (request ctx)
  "Update task status. Handles both automation runs and workflow jobs.
   For workflow jobs, run-id=0 and job-id is in the status field prefix."
  (let* ((runner (require-runner-from-ctx ctx))
         (runner-id (getf runner :id))
         (run-id (slot-value request 'cave::run-id))
         (status (slot-value request 'cave::status))
         (terminal (member status '("success" "failure" "cancelled" "timed_out")
                           :test #'equal)))
    (postmodern:with-connection *db-spec*
      (if (plusp run-id)
          ;; Simple automation run
          (progn
            (unless (update-run-status-for-runner run-id runner-id status)
              (error "runner is not assigned to this task"))
            (when (and terminal (getf runner :ephemeral))
              (delete-runner (getf runner :id))
              (llog:info "Ephemeral runner cleaned up" :id (getf runner :id))))
          ;; Workflow job — run-id=0, status format: "job:<job-id>:<status>"
          (when (uiop:string-prefix-p "job:" status)
            (let* ((parts (uiop:split-string status :separator '(#\:)))
                   (job-id (parse-integer (second parts) :junk-allowed t))
                   (job-status (third parts)))
              (when (and job-id job-status)
                (unless (update-job-status-for-runner job-id runner-id job-status)
                  (error "runner is not assigned to this workflow job"))
                (let ((job (postmodern:query
                            (:select '* :from 'cave-workflow-jobs :where (:= 'id job-id))
                            :plist)))
                  (when job
                    (check-workflow-job-completion job)))))))))
  (make-instance 'cave::update-task-status-response :ok t))

(defun make-automation-task-event (run)
  "Build a TaskEvent for a simple automation run."
  (let* ((repo (find-repo-by-id (getf run :repo-id)))
         (owner-name (when repo (repo-owner-name repo)))
         (def-id (getf run :definition-id))
         (def (when (and def-id (not (eq def-id :null)))
                (postmodern:query
                 (:select '* :from 'cave-automation-definitions
                  :where (:= 'id def-id))
                 :plist)))
         (command (if def (getf def :command) (getf run :definition-name))))
    (make-instance 'cave::task-event
                   :run-id (getf run :id)
                   :repo-owner (or owner-name "")
                   :repo-name (if repo (getf repo :name) "")
                   :command (or command "")
                   :commit-sha (let ((sha (getf run :commit-sha)))
                                 (if (eq sha :null) "" sha))
                   :ref (let ((r (getf run :ref)))
                          (if (eq r :null) "" r))
                   :timeout-seconds (if def (getf def :timeout-seconds) 60))))

(defun %cache-volume-name (repo-id path)
  "Deterministic, repo-scoped, podman-safe volume name for a cache PATH.
Scoped by REPO-ID so different repos sharing a runner never share a cache."
  (let ((digest (ironclad:byte-array-to-hex-string
                 (ironclad:digest-sequence
                  :sha256 (sb-ext:string-to-octets path :external-format :utf-8)))))
    (format nil "cave-cache-~A-~A" repo-id (subseq digest 0 16))))

(defun %job-cache-mounts (repo-id cache-paths-str)
  "Map a job's declared cache paths to repo-scoped persistent podman volumes.
Returns a newline-joined string of VOLUME<TAB>CONTAINER-PATH lines, or \"\"."
  (if (and repo-id (stringp cache-paths-str) (plusp (length cache-paths-str)))
      (let ((lines
              (loop for raw in (uiop:split-string cache-paths-str :separator '(#\Newline))
                    for clean = (string-trim '(#\Space #\Tab #\Return) raw)
                    when (plusp (length clean))
                      collect (format nil "~A~C~A"
                                      (%cache-volume-name repo-id clean) #\Tab clean))))
        (if lines (format nil "~{~A~^~%~}" lines) ""))
      ""))

(defun %github-context-env (run repo owner-name repo-name job)
  "GitHub-Actions context env for a workflow job, as newline KEY=VALUE. The
runner injects these (and adds a CAVE_* twin for every GITHUB_* key, plus the
RUNNER_*/file-protocol vars)."
  (declare (ignore repo))
  (let* ((ref (let ((r (getf run :ref))) (if (eq r :null) "" (or r ""))))
         (sha (let ((s (getf run :commit-sha))) (if (eq s :null) "" (or s ""))))
         (ref-name (cond ((uiop:string-prefix-p "refs/heads/" ref) (subseq ref 11))
                         ((uiop:string-prefix-p "refs/tags/" ref) (subseq ref 10))
                         (t ref)))
         (ref-type (cond ((uiop:string-prefix-p "refs/tags/" ref) "tag")
                         ((uiop:string-prefix-p "refs/heads/" ref) "branch")
                         (t "")))
         (actor (let* ((id (getf run :triggered-by-id))
                       (u (when (and id (not (eq id :null))) (find-user-by-id id))))
                  (or (and u (getf u :username)) "")))
         (event (or (trigger-to-yaml-event (getf run :trigger-event)) "push"))
         (base (config-value :base-url "")))
    (with-output-to-string (s)
      (format s "GITHUB_ACTIONS=true~%CI=true~%")
      (format s "GITHUB_REPOSITORY=~A/~A~%" owner-name repo-name)
      (format s "GITHUB_REPOSITORY_OWNER=~A~%" owner-name)
      (format s "GITHUB_WORKFLOW=~A~%" (or (getf run :workflow-name) ""))
      (format s "GITHUB_JOB=~A~%" (let ((n (getf job :name))) (if (eq n :null) "" (or n ""))))
      (format s "GITHUB_RUN_ID=~A~%GITHUB_RUN_NUMBER=~A~%" (getf run :id) (getf run :id))
      (format s "GITHUB_RUN_ATTEMPT=~A~%" (1+ (or (getf job :attempts) 0)))
      (format s "GITHUB_SHA=~A~%GITHUB_REF=~A~%" sha ref)
      (format s "GITHUB_REF_NAME=~A~%GITHUB_REF_TYPE=~A~%" ref-name ref-type)
      (format s "GITHUB_ACTOR=~A~%GITHUB_EVENT_NAME=~A~%" actor event)
      (format s "GITHUB_SERVER_URL=~A~%GITHUB_API_URL=~A/api/v1~%" base base)
      (format s "GITHUB_WORKSPACE=/workspace~%"))))

(defun make-workflow-task-event (job)
  "Build a TaskEvent for a workflow job."
  (let* ((run (find-workflow-run (getf job :workflow-run-id)))
         (repo (when run (find-repo-by-id (getf run :repo-id))))
         (owner-name (when repo (repo-owner-name repo)))
         (repo-name (when repo (getf repo :name)))
         (steps (list-workflow-steps (getf job :id)))
         (step-specs (mapcar (lambda (s)
                               (make-instance 'cave::step-spec
                                              :step-id (getf s :id)
                                              :step-order (getf s :step-order)
                                              :name (let ((n (getf s :name)))
                                                      (if (eq n :null) "" n))
                                              :command (getf s :command)
                                              :timeout-seconds (getf s :timeout-seconds 0)
                                              :continue-on-error (eq (getf s :continue-on-error) t)
                                              :env (or (getf s :env) "")
                                              :id-name (or (getf s :id-name) "")
                                              :if-cond (or (getf s :if-cond) "")))
                             steps)))
    ;; Mark workflow run as running if it's still queued
    (when (and run (equal (getf run :status) "queued"))
      (update-workflow-run-status (getf run :id) "running"))
    (make-instance 'cave::task-event
                   :run-id 0
                   :job-id (getf job :id)
                   :repo-owner (or owner-name "")
                   :repo-name (or repo-name "")
                   :command ""
                   :image (getf job :image)
                   :privileged (eq (getf job :privileged) t)
                   :steps step-specs
                   :commit-sha (if (and run (not (eq (getf run :commit-sha) :null)))
                                   (getf run :commit-sha) "")
                   :ref (if (and run (not (eq (getf run :ref) :null)))
                            (getf run :ref) "")
                   :clone-url (if (and owner-name repo-name)
                                  (format nil "~A/~A/~A.git"
                                          ;; Internal runners (e.g. the on-host
                                          ;; cave-runner pod) can't reach the public
                                          ;; base-url — it resolves to loopback inside
                                          ;; the container. :runner-clone-base-url, when
                                          ;; set, overrides with an internally-reachable
                                          ;; URL (e.g. http://cave-grpc:8080).
                                          (let ((rb (config-value :runner-clone-base-url "")))
                                            (if (string= rb "")
                                                (config-value :base-url "http://localhost:8080")
                                                rb))
                                          owner-name repo-name)
                                  "")
                   :timeout-seconds (let ((t-s (getf job :timeout-seconds 0)))
                                      (if (and t-s (plusp t-s)) t-s 300))
                   :secrets-env (if repo
                                    (secrets-env-string (secrets-for-repo repo))
                                    "")
                   :cache-mounts (%job-cache-mounts (and run (getf run :repo-id))
                                                    (getf job :cache-paths ""))
                   ;; GITHUB_*/CAVE_* context + the merged workflow/job `env:`.
                   :context-env (if (and run repo)
                                    (concatenate 'string
                                                 (%github-context-env run repo owner-name repo-name job)
                                                 (let ((e (getf job :env))) (if (and e (not (eq e :null))) e "")))
                                    "")
                   :matrix-json (let ((m (getf job :matrix))) (if (and m (not (eq m :null))) m "")))))

(defun handle-update-step-status (request ctx)
  "Update a workflow step's status."
  (let ((runner (require-runner-from-ctx ctx)))
    (postmodern:with-connection *db-spec*
      (let ((step-id (slot-value request 'cave::step-id))
            (status (slot-value request 'cave::status))
            (exit-code (slot-value request 'cave::exit-code)))
        (unless (update-step-status-for-runner step-id (getf runner :id) status
                                               :exit-code (when (plusp exit-code) exit-code))
          (error "runner is not assigned to this workflow step")))))
  (make-instance 'cave::update-step-status-response :ok t))

(defun handle-append-step-log (request ctx)
  "Append log chunk to a workflow step."
  (let ((runner (require-runner-from-ctx ctx)))
    (postmodern:with-connection *db-spec*
      (unless (append-step-log-for-runner (slot-value request 'cave::step-id)
                                          (getf runner :id)
                                          (slot-value request 'cave::chunk))
        (error "runner is not assigned to this workflow step"))))
  (make-instance 'cave::append-step-log-response :ok t))

(defun handle-watch-tasks (request ctx stream)
  "Server-streaming: push tasks to runner as they become available.
   Checks both simple automations and workflow jobs."
  (let ((runner (get-runner-from-ctx ctx)))
    (unless runner
      (error "unauthenticated"))
    (let ((runner-id (getf runner :id))
          (runner-labels (handler-case (slot-value request 'cave::runner-labels)
                           (error () ""))))
      (handler-case
        (postmodern:with-connection *db-spec*
          (update-runner-heartbeat runner-id :labels runner-labels))
        (error () nil))
      (unwind-protect
       (loop
        ;; Stop when the client is gone — otherwise this handler thread spins
        ;; forever heartbeating a dead runner, leaving it falsely "online" so
        ;; cleanup-offline-runners never reaps it and the scheduler may assign
        ;; it jobs. context-check-cancelled catches a graceful cancel/deadline;
        ;; an abruptly killed runner (the common systemd-restart case) only
        ;; surfaces as a closed HTTP/2 connection, which the per-connection
        ;; reader thread marks on EOF.
        (when (or (ag-grpc:context-check-cancelled ctx)
                  (let ((conn (ignore-errors (ag-grpc::server-stream-connection stream))))
                    (and conn (member (ag-http2:connection-state conn)
                                      '(:closing :closed)))))
          (return))
        (handler-case
        (postmodern:with-connection *db-spec*
          (update-runner-heartbeat runner-id)
          (let* ((runner-rec (postmodern:query
                              (:select '* :from 'cave-runners :where (:= 'id runner-id))
                              :plist))
                 (scope (getf runner-rec :scope))
                 (scope-id (let ((sid (getf runner-rec :scope-id)))
                             (unless (eq sid :null) sid))))
            ;; Try simple automation first
            (let ((run (when runner-rec
                         (fetch-queued-run runner-id (getf runner-rec :labels)
                                           scope scope-id))))
              (cond
                (run
                 ;; If delivery fails (dead stream), requeue so the task isn't
                 ;; wedged 'assigned', and end this RPC so the runner reconnects.
                 (handler-case
                     (ag-grpc:stream-send stream (make-automation-task-event run))
                   (error ()
                     (requeue-automation-run (getf run :id))
                     (return))))
                ;; Try workflow job
                (t
                 (let ((job (fetch-queued-workflow-job runner-id (getf runner-rec :labels) scope scope-id)))
                   (when job
                     (handler-case
                         (ag-grpc:stream-send stream (make-workflow-task-event job))
                       (error ()
                         (requeue-workflow-job (getf job :id))
                         (return))))))))))
          (error (e)
            (llog:error "WatchTasks loop error" :runner-id runner-id
                                                 :error (princ-to-string e))))
        (sleep 3))
       ;; Cleanup on disconnect
       (handler-case
           (postmodern:with-connection *db-spec*
             (postmodern:execute
              (:update 'cave-runners :set 'status "offline"
               :where (:= 'id runner-id)))
             (llog:info "Runner went offline" :runner-id runner-id))
         (error () nil))))))

;;; --- Server Lifecycle ---

(defun start-grpc-server (port)
  "Start the gRPC runner service."
  (setf *grpc-server* (ag-grpc:make-grpc-server port))

  ;; Register handlers
  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/RegisterRunner"
    #'handle-register-runner
    :request-type 'cave::register-runner-request
    :response-type 'cave::register-runner-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/DeclareRunner"
    #'handle-declare-runner
    :request-type 'cave::declare-runner-request
    :response-type 'cave::declare-runner-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/FetchTask"
    #'handle-fetch-task
    :request-type 'cave::fetch-task-request
    :response-type 'cave::fetch-task-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/AppendTaskLog"
    #'handle-append-task-log
    :request-type 'cave::append-task-log-request
    :response-type 'cave::append-task-log-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/UpdateTaskStatus"
    #'handle-update-task-status
    :request-type 'cave::update-task-status-request
    :response-type 'cave::update-task-status-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/UpdateStepStatus"
    #'handle-update-step-status
    :request-type 'cave::update-step-status-request
    :response-type 'cave::update-step-status-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/AppendStepLog"
    #'handle-append-step-log
    :request-type 'cave::append-step-log-request
    :response-type 'cave::append-step-log-response)

  (ag-grpc:server-register-handler *grpc-server*
    "/cave.runner.RunnerService/WatchTasks"
    #'handle-watch-tasks
    :request-type 'cave::watch-tasks-request
    :response-type 'cave::task-event
    :server-streaming t)

  ;; Runner cleanup thread (mark offline, delete stale ephemeral)
  (bt2:make-thread
   (lambda ()
     (loop
       (sleep 30)
       (handler-case
           (postmodern:with-connection *db-spec*
             (cleanup-offline-runners))
         (error () nil))))
   :name "cave-runner-cleanup")

  ;; Start in a background thread
  (bt2:make-thread
   (lambda ()
     (handler-case (ag-grpc:server-start *grpc-server*)
       (error (e)
         (llog:error "gRPC server error" :error (princ-to-string e)))))
   :name "cave-grpc-server")

  (llog:info "gRPC runner service started" :port port))

(defun stop-grpc-server ()
  "Stop the gRPC runner service."
  (when *grpc-server*
    (ag-grpc:server-stop *grpc-server* :graceful t)
    (setf *grpc-server* nil)))
