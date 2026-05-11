;;; chamber.lisp — Git storage gRPC service for Cave
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Chamber mediates ALL git repository access with:
;;; - Concurrency control (global semaphore + per-repo write locks)
;;; - LRU cache (keyed by git object SHA)
;;; - Timeouts on all git operations
;;; Runs in-process (single-node) or as a separate service (cluster).

(in-package #:cave)

;;; --- Proto compilation ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((proto-path (merge-pathnames "src/chamber.proto" (asdf:system-source-directory :cave))))
    (when (probe-file proto-path)
      (ag-proto:compile-proto-file proto-path :load t :package (find-package :cave)))))

;;; --- Concurrency control ---

(defvar *chamber-semaphore* nil "Semaphore limiting concurrent git processes.")
(defvar *chamber-repo-semas* (make-hash-table :test 'equal) "Per-repo write semaphores.")
(defvar *chamber-repo-semas-lock* (bt2:make-lock :name "chamber-repo-semas"))

(defun ensure-chamber-semaphore ()
  (unless *chamber-semaphore*
    (setf *chamber-semaphore*
          (bt2:make-semaphore :name "chamber-git"
                             :count (config-value :chamber-max-git-processes 64)))))

(defmacro with-git-read (&body body)
  "Execute BODY with the git semaphore held (concurrent reads OK)."
  `(progn
     (ensure-chamber-semaphore)
     (if (bt2:wait-on-semaphore *chamber-semaphore* :timeout 30)
         (unwind-protect (progn ,@body)
           (bt2:signal-semaphore *chamber-semaphore*))
         (error "Git read timed out waiting for semaphore"))))

(defun get-repo-write-sema (repo-key)
  "Get or create a per-repo binary semaphore (used by both Chamber RPCs and SSH pushes)."
  (bt2:with-lock-held (*chamber-repo-semas-lock*)
    (or (gethash repo-key *chamber-repo-semas*)
        (setf (gethash repo-key *chamber-repo-semas*)
              (bt2:make-semaphore :name (format nil "repo-write:~A" repo-key)
                                 :count 1)))))

(defmacro with-git-write (repo-key &body body)
  "Execute BODY with global semaphore + per-repo write semaphore.
   Serializes writes per repo, shared between Chamber RPCs and SSH pushes."
  (let ((key (gensym "KEY")) (sema (gensym "SEMA")))
    `(let ((,key ,repo-key))
       (with-git-read
         (let ((,sema (get-repo-write-sema ,key)))
           (if (bt2:wait-on-semaphore ,sema :timeout 30)
               (unwind-protect (progn ,@body)
                 (bt2:signal-semaphore ,sema))
               (error "Git write timed out waiting for repo lock")))))))

;;; --- Result cache ---

(defvar *chamber-cache* (make-hash-table :test 'equal))
(defvar *chamber-cache-lock* (bt2:make-lock :name "chamber-cache"))
(defvar *chamber-cache-bytes* 0)

(defun chamber-cache-max-bytes ()
  (* (config-value :chamber-cache-size-mb 128) 1024 1024))

(defun chamber-cache-get (key)
  (bt2:with-lock-held (*chamber-cache-lock*)
    (let ((entry (gethash key *chamber-cache*)))
      (when entry
        (setf (cddr entry) (get-universal-time)) ; update access time
        (car entry))))) ; return content

(defun chamber-cache-put (key content &optional (size-estimate 0))
  (let ((size (if (plusp size-estimate) size-estimate
                  (typecase content
                    (string (length content))
                    (vector (length content))
                    (t 100)))))
    (when (> size (* 4 1024 1024)) ; skip >4MB
      (return-from chamber-cache-put content))
    (bt2:with-lock-held (*chamber-cache-lock*)
      ;; Evict oldest if needed
      (loop while (> (+ *chamber-cache-bytes* size) (chamber-cache-max-bytes))
            do (let ((oldest-key nil) (oldest-time (get-universal-time)))
                 (maphash (lambda (k v)
                            (when (< (cddr v) oldest-time)
                              (setf oldest-key k oldest-time (cddr v))))
                          *chamber-cache*)
                 (if oldest-key
                     (let ((old (gethash oldest-key *chamber-cache*)))
                       (decf *chamber-cache-bytes* (cadr old))
                       (remhash oldest-key *chamber-cache*))
                     (return))))
      (setf (gethash key *chamber-cache*) (list content size (get-universal-time)))
      (incf *chamber-cache-bytes* size)))
  content)

(defun chamber-invalidate-repo (owner repo-name)
  "Remove all cached entries for a repository after a push."
  (let ((prefix (format nil "~A/~A:" owner repo-name)))
    (bt2:with-lock-held (*chamber-cache-lock*)
      (let ((keys-to-remove nil))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (search prefix k)
                     (push k keys-to-remove)))
                 *chamber-cache*)
        (dolist (k keys-to-remove)
          (let ((entry (gethash k *chamber-cache*)))
            (when entry
              (decf *chamber-cache-bytes* (cadr entry))
              (remhash k *chamber-cache*))))
        (when keys-to-remove
          (llog:info "Invalidated chamber cache" :repo (format nil "~A/~A" owner repo-name)
                                                  :entries (length keys-to-remove)))))))

(defmacro with-chamber-cache ((key) &body body)
  "Return cached result for KEY if available, otherwise execute BODY and cache."
  (let ((k (gensym "KEY")) (result (gensym "RESULT")))
    `(let ((,k ,key))
       (or (chamber-cache-get ,k)
           (let ((,result (progn ,@body)))
             (when ,result (chamber-cache-put ,k ,result))
             ,result)))))

;;; --- Helpers ---

(defun chamber-repo-path (owner repo-name)
  "Resolve owner/repo-name to on-disk bare repo path."
  (merge-pathnames (format nil "~A/~A.git/" owner repo-name) (repos-dir)))

(defun make-cache-key (op owner repo-name &rest args)
  (format nil "~A:~A/~A:~{~A~^:~}" op owner repo-name args))

;;; --- RPC Handlers ---

(defun handle-chamber-get-tree (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (ref (slot-value request 'cave::ref))
         (path (slot-value request 'cave::path))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((entries (with-chamber-cache ((make-cache-key "tree" owner repo-name ref path))
                       (git-tree disk-path :ref ref :path path))))
        (make-instance 'cave::get-tree-response
                       :entries (mapcar (lambda (e)
                                          (make-instance 'cave::tree-entry
                                                         :mode (or (getf e :mode) "")
                                                         :type (or (getf e :type) "")
                                                         :hash (or (getf e :hash) "")
                                                         :size (or (getf e :size) 0)
                                                         :name (or (getf e :name) "")))
                                        (or entries nil)))))))

(defun handle-chamber-get-blob (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (ref (slot-value request 'cave::ref))
         (path (slot-value request 'cave::path))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((content (with-chamber-cache ((make-cache-key "blob" owner repo-name ref path))
                       (git-blob disk-path ref path))))
        (make-instance 'cave::get-blob-response
                       :content (or content "")
                       :found (if content t nil))))))

(defun handle-chamber-get-blob-bytes (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (ref (slot-value request 'cave::ref))
         (path (slot-value request 'cave::path))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((content (git-blob-bytes disk-path ref path)))
        (make-instance 'cave::get-blob-bytes-response
                       :content (or content (make-array 0 :element-type '(unsigned-byte 8)))
                       :found (if content t nil))))))

(defun handle-chamber-get-blob-info (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (ref (slot-value request 'cave::ref))
         (path (slot-value request 'cave::path))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((hash (git-blob-hash disk-path ref path))
            (size (git-blob-size disk-path ref path)))
        (if hash
            (let ((content (git-blob disk-path ref path)))
              (make-instance 'cave::get-blob-info-response
                             :hash hash
                             :size (or size 0)
                             :is-binary (if (git-blob-binary-p content) t nil)
                             :found t))
            (make-instance 'cave::get-blob-info-response
                           :hash "" :size 0 :is-binary nil :found nil))))))

(defun handle-chamber-get-commit (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (hash (slot-value request 'cave::hash))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((commit (git-show-commit disk-path hash))
            (diff (git-commit-diff disk-path hash))
            (stat (git-commit-stat disk-path hash)))
        (if commit
            (make-instance 'cave::get-commit-response
                           :commit (make-instance 'cave::commit-info
                                                  :hash (or (getf commit :hash) "")
                                                  :short-hash (or (getf commit :short-hash) "")
                                                  :author (or (getf commit :author) "")
                                                  :date (or (getf commit :date) "")
                                                  :subject (or (getf commit :subject) ""))
                           :diff (or diff "")
                           :stat (or stat "")
                           :found t)
            (make-instance 'cave::get-commit-response
                           :commit (make-instance 'cave::commit-info
                                                  :hash "" :short-hash "" :author "" :date "" :subject "")
                           :diff "" :stat "" :found nil))))))

(defun handle-chamber-get-log (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (branch (slot-value request 'cave::branch))
         (limit (slot-value request 'cave::limit))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((commits (git-log disk-path
                              :limit (if (plusp limit) limit 20)
                              :branch (when (and branch (not (string= branch ""))) branch))))
        (make-instance 'cave::get-log-response
                       :commits (mapcar (lambda (c)
                                          (make-instance 'cave::commit-info
                                                         :hash (or (getf c :hash) "")
                                                         :short-hash (or (getf c :short-hash) "")
                                                         :author (or (getf c :author) "")
                                                         :date (or (getf c :date) "")
                                                         :subject (or (getf c :subject) "")))
                                        (or commits nil)))))))

(defun handle-chamber-get-branches (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::get-branches-response
                     :branches (or (git-branches disk-path) nil)))))

(defun handle-chamber-get-tags (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::get-tags-response
                     :tags (or (git-tags disk-path) nil)))))

(defun handle-chamber-get-default-branch (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::get-default-branch-response
                     :branch (or (git-default-branch disk-path) "main")))))

(defun handle-chamber-is-empty (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::is-empty-response
                     :empty (if (git-repo-empty-p disk-path) t nil)))))

(defun handle-chamber-get-diff (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (base-ref (slot-value request 'cave::base-ref))
         (head-ref (slot-value request 'cave::head-ref))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::get-diff-response
                     :base-ref base-ref
                     :head-ref head-ref))))

(defun handle-chamber-get-diff-merge-base (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (target (slot-value request 'cave::target))
         (source (slot-value request 'cave::source))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((diff (git-diff-merge-base disk-path target source)))
        (make-instance 'cave::get-diff-response
                       :base-ref (or diff "")
                       :head-ref "")))))

(defun handle-chamber-get-commit-count (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (branch (slot-value request 'cave::branch))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (make-instance 'cave::get-commit-count-response
                     :count (or (git-commit-count disk-path
                                  :branch (when (and branch (not (string= branch ""))) branch))
                                0)))))

(defun handle-chamber-find-readme (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (ref (slot-value request 'cave::ref))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (let ((entry (git-readme-path disk-path :ref ref)))
        (if entry
            (make-instance 'cave::find-readme-response
                           :name (getf entry :name)
                           :found t)
            (make-instance 'cave::find-readme-response
                           :name "" :found nil))))))

;; --- Write operations ---

(defun handle-chamber-merge-branch (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (target (slot-value request 'cave::target))
         (source (slot-value request 'cave::source))
         (author (slot-value request 'cave::author))
         (message (slot-value request 'cave::message))
         (squash (slot-value request 'cave::squash))
         (disk-path (chamber-repo-path owner repo-name))
         (repo-key (format nil "~A/~A" owner repo-name)))
    (with-git-write repo-key
      (multiple-value-bind (success-p err)
          (git-merge-branch disk-path target source
                            :author author :message message
                            :squash squash)
        (make-instance 'cave::merge-branch-response
                       :ok success-p
                       :error (or err ""))))))

(defun handle-chamber-delete-branch (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (branch (slot-value request 'cave::branch))
         (disk-path (chamber-repo-path owner repo-name))
         (repo-key (format nil "~A/~A" owner repo-name)))
    (with-git-write repo-key
      (make-instance 'cave::delete-branch-response
                     :ok (if (git-delete-branch disk-path branch) t nil)))))

;; --- Mirror/clone operations ---

(defun handle-chamber-clone-from-url (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (url (slot-value request 'cave::url))
         (auth-token (slot-value request 'cave::auth-token))
         (dest-path (chamber-repo-path owner repo-name))
         (repo-key (format nil "~A/~A" owner repo-name)))
    (with-git-write repo-key
      (multiple-value-bind (success-p err)
          (git-clone-bare-from-url url dest-path
                                   :auth-token (when (and auth-token (not (string= auth-token "")))
                                                 auth-token))
        (make-instance 'cave::clone-from-url-response
                       :ok success-p
                       :error (or err ""))))))

(defun handle-chamber-push-mirror (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (url (slot-value request 'cave::url))
         (auth-token (slot-value request 'cave::auth-token))
         (disk-path (chamber-repo-path owner repo-name)))
    (with-git-read
      (multiple-value-bind (success-p err)
          (git-push-mirror disk-path url
                           (when (and auth-token (not (string= auth-token "")))
                             auth-token))
        (make-instance 'cave::push-mirror-response
                       :ok success-p
                       :error (or err ""))))))

(defun handle-chamber-pull-mirror (request ctx)
  (declare (ignore ctx))
  (let* ((owner (slot-value request 'cave::owner))
         (repo-name (slot-value request 'cave::repo-name))
         (url (slot-value request 'cave::url))
         (auth-token (slot-value request 'cave::auth-token))
         (disk-path (chamber-repo-path owner repo-name))
         (repo-key (format nil "~A/~A" owner repo-name)))
    (with-git-write repo-key
      (multiple-value-bind (success-p err)
          (git-pull-mirror disk-path url
                           (when (and auth-token (not (string= auth-token "")))
                             auth-token))
        (make-instance 'cave::pull-mirror-response
                       :ok success-p
                       :error (or err ""))))))

;;; --- Streaming git protocol handlers ---

(defconstant +pack-chunk-size+ 32768 "Bytes per PackData chunk (32KB).")

(defun pump-client-to-process (stream proc-input)
  "Read PackData messages from gRPC stream, write data to git process stdin.
   Returns when client closes send (stream-recv returns NIL)."
  (handler-case
      (ag-grpc:do-stream-recv (msg stream)
        (let ((data (slot-value msg 'cave::data)))
          (when (and data (plusp (length data)))
            (write-sequence data proc-input)
            (force-output proc-input))))
    (error (e)
      (llog:warn "Client-to-git pump error" :error (princ-to-string e))))
  (handler-case (close proc-input) (error () nil)))

(defun pump-process-to-client (stream proc-output)
  "Read git process stdout, send as PackData messages on gRPC stream.
   Returns when git process closes stdout (EOF)."
  (let ((buf (make-array +pack-chunk-size+ :element-type '(unsigned-byte 8))))
    (handler-case
        (loop
          (let ((n (read-sequence buf proc-output)))
            (when (zerop n) (return))
            (let ((chunk (if (= n +pack-chunk-size+)
                             buf
                             (subseq buf 0 n))))
              (ag-grpc:stream-send stream
                (make-instance 'cave::pack-data :data chunk)))))
      (error (e)
        (llog:warn "Git-to-client pump error" :error (princ-to-string e))))))

(defun handle-chamber-receive-pack (ctx stream)
  "Bidirectional streaming handler: proxy git-receive-pack through gRPC."
  (declare (ignore ctx))
  ;; Read first message to get owner/repo
  (let ((init-msg (ag-grpc:stream-recv stream)))
    (unless init-msg (return-from handle-chamber-receive-pack))
    (let* ((owner (slot-value init-msg 'cave::owner))
           (repo-name (slot-value init-msg 'cave::repo-name))
           (disk-path (chamber-repo-path owner repo-name))
           (repo-key (format nil "~A/~A" owner repo-name))
           (timeout (config-value :chamber-stream-timeout 300)))
      (llog:info "ReceivePack started" :repo repo-key)
      ;; Acquire global semaphore + per-repo write semaphore
      (ensure-chamber-semaphore)
      (unless (bt2:wait-on-semaphore *chamber-semaphore* :timeout timeout)
        (error "ReceivePack timed out on global semaphore"))
      (let ((sema (get-repo-write-sema repo-key)))
        (unless (bt2:wait-on-semaphore sema :timeout timeout)
          (bt2:signal-semaphore *chamber-semaphore*)
          (error "ReceivePack timed out on repo lock"))
        (unwind-protect
            (let ((proc (uiop:launch-program
                         (list "git" "receive-pack" (namestring disk-path))
                         :input :stream :output :stream
                         :element-type '(unsigned-byte 8))))
              (unwind-protect
                  (let ((proc-input (uiop:process-info-input proc))
                        (proc-output (uiop:process-info-output proc)))
                    ;; Write initial data if any
                    (let ((init-data (slot-value init-msg 'cave::data)))
                      (when (and init-data (plusp (length init-data)))
                        (write-sequence init-data proc-input)
                        (force-output proc-input)))
                    ;; Pump in parallel: client→git in a thread, git→client here
                    (let ((pump-thread (bt2:make-thread
                                        (lambda () (pump-client-to-process stream proc-input))
                                        :name "receive-pack-pump")))
                      (pump-process-to-client stream proc-output)
                      (bt2:join-thread pump-thread)))
                ;; Wait for git process to finish
                (uiop:wait-process proc))
              ;; Invalidate cache after push
              (chamber-invalidate-repo owner repo-name)
              (llog:info "ReceivePack completed" :repo repo-key))
          ;; Release locks
          (bt2:signal-semaphore sema)
          (bt2:signal-semaphore *chamber-semaphore*))))))

(defun handle-chamber-upload-pack (ctx stream)
  "Bidirectional streaming handler: proxy git-upload-pack through gRPC."
  (declare (ignore ctx))
  (let ((init-msg (ag-grpc:stream-recv stream)))
    (unless init-msg (return-from handle-chamber-upload-pack))
    (let* ((owner (slot-value init-msg 'cave::owner))
           (repo-name (slot-value init-msg 'cave::repo-name))
           (disk-path (chamber-repo-path owner repo-name))
           (repo-key (format nil "~A/~A" owner repo-name)))
      (llog:info "UploadPack started" :repo repo-key)
      ;; Only acquire global semaphore (reads are concurrent)
      (ensure-chamber-semaphore)
      (unless (bt2:wait-on-semaphore *chamber-semaphore*
                                      :timeout (config-value :chamber-stream-timeout 300))
        (error "UploadPack timed out on global semaphore"))
      (unwind-protect
          (let ((proc (uiop:launch-program
                       (list "git" "upload-pack" (namestring disk-path))
                       :input :stream :output :stream
                       :element-type '(unsigned-byte 8))))
            (unwind-protect
                (let ((proc-input (uiop:process-info-input proc))
                      (proc-output (uiop:process-info-output proc)))
                  (let ((init-data (slot-value init-msg 'cave::data)))
                    (when (and init-data (plusp (length init-data)))
                      (write-sequence init-data proc-input)
                      (force-output proc-input)))
                  (let ((pump-thread (bt2:make-thread
                                      (lambda () (pump-client-to-process stream proc-input))
                                      :name "upload-pack-pump")))
                    (pump-process-to-client stream proc-output)
                    (bt2:join-thread pump-thread)))
              (uiop:wait-process proc))
            (llog:info "UploadPack completed" :repo repo-key))
        (bt2:signal-semaphore *chamber-semaphore*)))))

;;; --- Server lifecycle ---

(defvar *chamber-server* nil)

(defun start-chamber (port)
  "Start the Chamber gRPC service."
  (setf *chamber-server* (ag-grpc:make-grpc-server port))

  ;; Read operations
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetTree" #'handle-chamber-get-tree
    :request-type 'cave::get-tree-request :response-type 'cave::get-tree-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetBlob" #'handle-chamber-get-blob
    :request-type 'cave::get-blob-request :response-type 'cave::get-blob-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetBlobBytes" #'handle-chamber-get-blob-bytes
    :request-type 'cave::get-blob-bytes-request :response-type 'cave::get-blob-bytes-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetBlobInfo" #'handle-chamber-get-blob-info
    :request-type 'cave::get-blob-info-request :response-type 'cave::get-blob-info-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetCommit" #'handle-chamber-get-commit
    :request-type 'cave::get-commit-request :response-type 'cave::get-commit-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetLog" #'handle-chamber-get-log
    :request-type 'cave::get-log-request :response-type 'cave::get-log-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetBranches" #'handle-chamber-get-branches
    :request-type 'cave::get-branches-request :response-type 'cave::get-branches-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetTags" #'handle-chamber-get-tags
    :request-type 'cave::get-tags-request :response-type 'cave::get-tags-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetDefaultBranch" #'handle-chamber-get-default-branch
    :request-type 'cave::get-default-branch-request :response-type 'cave::get-default-branch-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/IsEmpty" #'handle-chamber-is-empty
    :request-type 'cave::is-empty-request :response-type 'cave::is-empty-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetDiff" #'handle-chamber-get-diff
    :request-type 'cave::get-diff-request :response-type 'cave::get-diff-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetDiffMergeBase" #'handle-chamber-get-diff-merge-base
    :request-type 'cave::get-diff-merge-base-request :response-type 'cave::get-diff-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/GetCommitCount" #'handle-chamber-get-commit-count
    :request-type 'cave::get-commit-count-request :response-type 'cave::get-commit-count-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/FindReadme" #'handle-chamber-find-readme
    :request-type 'cave::find-readme-request :response-type 'cave::find-readme-response)

  ;; Write operations
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/MergeBranch" #'handle-chamber-merge-branch
    :request-type 'cave::merge-branch-request :response-type 'cave::merge-branch-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/DeleteBranch" #'handle-chamber-delete-branch
    :request-type 'cave::delete-branch-request :response-type 'cave::delete-branch-response)

  ;; Mirror/clone
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/CloneFromURL" #'handle-chamber-clone-from-url
    :request-type 'cave::clone-from-url-request :response-type 'cave::clone-from-url-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/PushMirror" #'handle-chamber-push-mirror
    :request-type 'cave::push-mirror-request :response-type 'cave::push-mirror-response)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/PullMirror" #'handle-chamber-pull-mirror
    :request-type 'cave::pull-mirror-request :response-type 'cave::pull-mirror-response)

  ;; Git protocol streaming (bidirectional)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/ReceivePack" #'handle-chamber-receive-pack
    :request-type 'cave::pack-data :response-type 'cave::pack-data
    :client-streaming t :server-streaming t)
  (ag-grpc:server-register-handler *chamber-server*
    "/cave.chamber.Chamber/UploadPack" #'handle-chamber-upload-pack
    :request-type 'cave::pack-data :response-type 'cave::pack-data
    :client-streaming t :server-streaming t)

  ;; Start in background
  (bt2:make-thread
   (lambda ()
     (handler-case (ag-grpc:server-start *chamber-server*)
       (error (e)
         (llog:error "Chamber server error" :error (princ-to-string e)))))
   :name "chamber-grpc-server")

  (llog:info "Chamber started" :port port))

(defun stop-chamber ()
  (when *chamber-server*
    (handler-case (ag-grpc:server-stop *chamber-server* :graceful t)
      (error () nil))
    (setf *chamber-server* nil)))
