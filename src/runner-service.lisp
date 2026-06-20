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
           (token-record (validate-registration-token reg-token)))
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
  (declare (ignore ctx))
  (postmodern:with-connection *db-spec*
    (append-run-log (slot-value request 'cave::run-id)
                    (slot-value request 'cave::chunk)))
  (make-instance 'cave::append-task-log-response :ok t))

(defun handle-update-task-status (request ctx)
  "Update task status. Handles both automation runs and workflow jobs.
   For workflow jobs, run-id=0 and job-id is in the status field prefix."
  (let* ((runner (get-runner-from-ctx ctx))
         (run-id (slot-value request 'cave::run-id))
         (status (slot-value request 'cave::status))
         (terminal (member status '("success" "failure" "cancelled" "timed_out")
                           :test #'equal)))
    (postmodern:with-connection *db-spec*
      (if (plusp run-id)
          ;; Simple automation run
          (progn
            (update-run-status run-id status)
            (when (and terminal runner (getf runner :ephemeral))
              (delete-runner (getf runner :id))
              (llog:info "Ephemeral runner cleaned up" :id (getf runner :id))))
          ;; Workflow job — run-id=0, status format: "job:<job-id>:<status>"
          (when (uiop:string-prefix-p "job:" status)
            (let* ((parts (uiop:split-string status :separator '(#\:)))
                   (job-id (parse-integer (second parts) :junk-allowed t))
                   (job-status (third parts)))
              (when (and job-id job-status)
                (update-job-status job-id job-status)
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
                                              :continue-on-error (eq (getf s :continue-on-error) t)))
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
                                          (config-value :base-url "http://localhost:8080")
                                          owner-name repo-name)
                                  "")
                   :timeout-seconds (let ((t-s (getf job :timeout-seconds 0)))
                                      (if (and t-s (plusp t-s)) t-s 300)))))

(defun handle-update-step-status (request ctx)
  "Update a workflow step's status."
  (declare (ignore ctx))
  (postmodern:with-connection *db-spec*
    (let ((step-id (slot-value request 'cave::step-id))
          (status (slot-value request 'cave::status))
          (exit-code (slot-value request 'cave::exit-code)))
      (update-step-status step-id status
                          :exit-code (when (plusp exit-code) exit-code))))
  (make-instance 'cave::update-step-status-response :ok t))

(defun handle-append-step-log (request ctx)
  "Append log chunk to a workflow step."
  (declare (ignore ctx))
  (postmodern:with-connection *db-spec*
    (append-step-log (slot-value request 'cave::step-id)
                     (slot-value request 'cave::chunk)))
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
