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
  (let* ((reg-token (slot-value request 'cave::registration-token))
         (token-record (validate-registration-token reg-token)))
    (unless token-record
      (error "invalid registration token"))
    (postmodern:with-connection *db-spec*
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
                   (owner-name (when repo (repo-owner-name repo))))
              (make-instance 'cave::fetch-task-response
                             :has-task t
                             :run-id (getf run :id)
                             :repo-owner (or owner-name "")
                             :repo-name (if repo (getf repo :name) "")
                             :command (or (getf run :definition-name) "")
                             :commit-sha (let ((sha (getf run :commit-sha)))
                                           (if (eq sha :null) "" sha))
                             :ref (let ((r (getf run :ref)))
                                    (if (eq r :null) "" r))
                             :timeout-seconds 60))
            (make-instance 'cave::fetch-task-response :has-task nil))))))

(defun handle-append-task-log (request ctx)
  "Append log chunk to a running task."
  (declare (ignore ctx))
  (postmodern:with-connection *db-spec*
    (append-run-log (slot-value request 'cave::run-id)
                    (slot-value request 'cave::chunk)))
  (make-instance 'cave::append-task-log-response :ok t))

(defun handle-update-task-status (request ctx)
  "Update task status. Deletes ephemeral runners after terminal status."
  (let* ((runner (get-runner-from-ctx ctx))
         (run-id (slot-value request 'cave::run-id))
         (status (slot-value request 'cave::status))
         (terminal (member status '("success" "failure" "cancelled" "timed_out")
                           :test #'equal)))
    (postmodern:with-connection *db-spec*
      (update-run-status run-id status)
      ;; Delete ephemeral runner after task completes
      (when (and terminal runner (getf runner :ephemeral))
        (delete-runner (getf runner :id))
        (llog:info "Ephemeral runner cleaned up" :id (getf runner :id)))))
  (make-instance 'cave::update-task-status-response :ok t))

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

  ;; Runner cleanup thread (mark offline, delete stale ephemeral)
  (bt:make-thread
   (lambda ()
     (loop
       (sleep 30)
       (handler-case
           (postmodern:with-connection *db-spec*
             (cleanup-offline-runners))
         (error () nil))))
   :name "cave-runner-cleanup")

  ;; Start in a background thread
  (bt:make-thread
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
