;;; chamber-router.lisp — Multi-chamber routing layer
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Praefect-style router: sits between chamber-client and multiple
;;; Chamber gRPC servers. Routes reads to any healthy replica, writes
;;; to the primary + async replication to secondaries.
;;;
;;; When only one node is configured (or none), falls through to
;;; single-channel behavior in chamber-client.

(in-package #:cave)

;;; --- State ---

(defvar *chamber-nodes* nil
  "Cached list of chamber node plists from DB.")

(defvar *chamber-channels* (make-hash-table :test 'equal)
  "Map of node-id → ag-grpc channel.")

(defvar *chamber-channels-lock* (bt2:make-lock :name "chamber-channels"))

(defvar *chamber-health-thread* nil
  "Background health checker thread.")

;;; --- Predicates ---

(defun multi-chamber-p ()
  "True when multi-chamber routing is active."
  (and (chamber-enabled-p)
       (> (length *chamber-nodes*) 1)))

;;; --- Channel pool ---

(defun get-node-channel (node)
  "Get or create a gRPC channel for NODE (a plist with :address)."
  (let ((node-id (getf node :id)))
    (bt2:with-lock-held (*chamber-channels-lock*)
      (or (gethash node-id *chamber-channels*)
          (let* ((addr (getf node :address))
                 (colon (position #\: addr :from-end t))
                 (host (if colon (subseq addr 0 colon) addr))
                 (port (if colon
                           (parse-integer (subseq addr (1+ colon)) :junk-allowed t)
                           9444)))
            (setf (gethash node-id *chamber-channels*)
                  (ag-grpc:make-channel host (or port 9444)
                                        :timeout (config-value :chamber-rpc-timeout 10))))))))

(defun close-all-channels ()
  "Close all cached gRPC channels."
  (bt2:with-lock-held (*chamber-channels-lock*)
    (maphash (lambda (k ch)
               (declare (ignore k))
               (handler-case (ag-grpc:channel-close ch) (error () nil)))
             *chamber-channels*)
    (clrhash *chamber-channels*)))

;;; --- Node selection ---

(defun refresh-chamber-nodes ()
  "Reload node list from DB."
  (handler-case
      (setf *chamber-nodes* (list-chamber-nodes))
    (error (e)
      (llog:warn "Failed to refresh chamber nodes" :error (princ-to-string e)))))

(defun pick-read-node (repo-id)
  "Pick a healthy node for reading REPO-ID. Random among healthy replicas.
   Falls back to primary."
  (let ((healthy (repo-healthy-nodes repo-id)))
    (if healthy
        (nth (random (length healthy)) healthy)
        (repo-primary-node repo-id))))

(defun pick-write-node (repo-id)
  "Return the primary node for REPO-ID. Signals error if dead or unassigned."
  (let ((primary (repo-primary-node repo-id)))
    (unless primary
      (error "No primary chamber node assigned for repo ~A" repo-id))
    (when (string= (getf primary :status) "dead")
      (error "Primary chamber node ~A is dead for repo ~A"
             (getf primary :name) repo-id))
    primary))

;;; --- Routing ---

(defun router-call (method request response-type &key owner repo-name write-p)
  "Route a unary gRPC call to the correct chamber node."
  (let* ((repo (find-repo owner repo-name))
         (repo-id (when repo (getf repo :id))))
    (unless repo-id
      (error "Repository ~A/~A not found for chamber routing" owner repo-name))
    ;; Ensure repo has a node assignment
    (ensure-repo-assigned repo-id)
    (if write-p
        (router-write-and-replicate method request response-type
                                    :owner owner :repo-name repo-name
                                    :repo-id repo-id)
        (let ((node (pick-read-node repo-id)))
          (unless node
            (error "No healthy chamber node for ~A/~A" owner repo-name))
          (ag-grpc:grpc-call (get-node-channel node) method request
                              :response-type response-type)))))

(defun router-write-and-replicate (method request response-type
                                   &key owner repo-name repo-id)
  "Execute write on primary, return result, async replicate to secondaries."
  (let* ((primary (pick-write-node repo-id))
         (result (ag-grpc:grpc-call (get-node-channel primary) method request
                                     :response-type response-type)))
    ;; Bump generation on primary
    (handler-case (bump-repo-generation repo-id (getf primary :id))
      (error () nil))
    ;; Async: replicate to secondaries + invalidate caches
    (let ((secondaries (repo-secondary-nodes repo-id)))
      (when secondaries
        (bt2:make-thread
         (lambda ()
           (dolist (node secondaries)
             (handler-case
                 (progn
                   (ag-grpc:grpc-call (get-node-channel node) method request
                                       :response-type response-type)
                   (bump-repo-generation repo-id (getf node :id)))
               (error (e)
                 (llog:warn "Chamber replication failed"
                            :node (getf node :name)
                            :method method
                            :error (princ-to-string e))))))
         :name "chamber-replicate")))
    ;; Broadcast cache invalidation to all nodes (including primary)
    (broadcast-invalidate-cache owner repo-name)
    result))

(defun broadcast-invalidate-cache (owner repo-name &key exclude-node-id)
  "Send InvalidateCache to all chamber nodes."
  (dolist (node *chamber-nodes*)
    (unless (and exclude-node-id (= (getf node :id) exclude-node-id))
      (handler-case
          (ag-grpc:grpc-call
           (get-node-channel node)
           "/cave.chamber.Chamber/InvalidateCache"
           (make-instance 'cave::invalidate-cache-request
                          :owner owner :repo-name repo-name)
           :response-type 'cave::invalidate-cache-response)
        (error (e)
          (llog:warn "Cache invalidation failed"
                     :node (getf node :name)
                     :error (princ-to-string e)))))))

;;; --- Health checker ---

(defun start-chamber-health-checker ()
  "Start background thread that pings all chamber nodes periodically."
  (when *chamber-health-thread*
    (return-from start-chamber-health-checker))
  (setf *chamber-health-thread*
        (bt2:make-thread
         (lambda ()
           (loop
             (sleep (config-value :chamber-health-interval 10))
             (refresh-chamber-nodes)
             (dolist (node *chamber-nodes*)
               (handler-case
                   (progn
                     (ag-grpc:grpc-call
                      (get-node-channel node)
                      "/cave.chamber.Chamber/IsEmpty"
                      (make-instance 'cave::is-empty-request
                                     :owner "__health" :repo-name "__ping")
                      :response-type 'cave::is-empty-response)
                     ;; Success: mark healthy
                     (unless (string= (getf node :status) "healthy")
                       (update-chamber-node-status (getf node :id) "healthy")
                       (llog:info "Chamber node recovered" :node (getf node :name))))
                 (error (e)
                   (declare (ignore e))
                   (cond
                     ((string= (getf node :status) "healthy")
                      (update-chamber-node-status (getf node :id) "suspect")
                      (llog:warn "Chamber node suspect" :node (getf node :name)))
                     ((string= (getf node :status) "suspect")
                      (update-chamber-node-status (getf node :id) "dead")
                      (llog:warn "Chamber node dead" :node (getf node :name)))))))))
         :name "chamber-health")))

;;; --- Initialization ---

(defun init-chamber-router (nodes-config)
  "Initialize the multi-chamber router from config.
   NODES-CONFIG is a list of plists: ((:name \"n1\" :address \"host:port\") ...)."
  (dolist (nc nodes-config)
    (upsert-chamber-node :name (getf nc :name) :address (getf nc :address)))
  (refresh-chamber-nodes)
  ;; Pre-open channels
  (dolist (node *chamber-nodes*)
    (handler-case (get-node-channel node)
      (error (e)
        (llog:warn "Failed to connect to chamber node"
                   :node (getf node :name)
                   :error (princ-to-string e)))))
  ;; Auto-assign any existing repos that have no node assignment
  (let ((unassigned (postmodern:query
                     (:select 'id :from 'cave-repos
                      :where (:not (:exists
                               (:select 1 :from 'cave-repo-assignments
                                :where (:= 'cave-repo-assignments.repo-id
                                            'cave-repos.id)))))
                     :column)))
    (when unassigned
      (dolist (repo-id unassigned)
        (handler-case (ensure-repo-assigned repo-id)
          (error (e)
            (llog:warn "Failed to assign existing repo"
                       :repo-id repo-id :error (princ-to-string e)))))
      (llog:info "Assigned existing repos to chamber nodes" :count (length unassigned))))
  (llog:info "Chamber router initialized" :nodes (length *chamber-nodes*)))

(defun stop-chamber-router ()
  "Stop the health checker and close channels."
  (when *chamber-health-thread*
    (handler-case (bt2:destroy-thread *chamber-health-thread*) (error () nil))
    (setf *chamber-health-thread* nil))
  (close-all-channels)
  (setf *chamber-nodes* nil))
