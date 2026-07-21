;;; chamber-client.lisp — Client for Chamber git storage service
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Provides chamber-* functions that mirror the git.lisp API.
;;; When Chamber is enabled, calls go through gRPC.
;;; When disabled, falls back to direct git.lisp calls (backward compat).

(in-package #:cave)

(defvar *chamber-channel* nil "gRPC channel to Chamber service.")

;;; --- Per-channel call serialization ---
;;;
;;; ag-grpc's client connection is NOT safe for concurrent calls: it starts no
;;; reader thread (connection-reader-thread-active-p stays nil) and takes no
;;; read-lock, so channel-receive-headers/-message each pull frames straight off
;;; the shared socket. Cave shares one channel per node across every Hunchentoot
;;; worker, so two workers calling grpc-call on the same channel interleave their
;;; frame reads and desync the HTTP/2 stream — surfacing as garbage frame lengths
;;; ("-N is not of type (MOD ...)" binding AVAILABLE) and torn buffers ("NIL is
;;; not of type BUFFER"), after which reset closes the socket under the others
;;; (broken pipe / closed stream). Serialize calls per channel until the library
;;; grows a real reader thread. Writes already take connection-write-lock; this
;;; extends that discipline to the whole request/response so reads can't overlap.
;;; The lock is per channel (keyed by identity), so calls to *different* nodes
;;; still run in parallel — only same-channel calls serialize.

(defvar *channel-call-locks* (make-hash-table :test 'eq :weakness :key)
  "Channel object -> lock serializing gRPC calls on that channel.
   Weak on the key so a closed/GC'd channel's lock is collected with it.")

(defvar *channel-call-locks-lock* (bt2:make-lock :name "channel-call-locks")
  "Guards *channel-call-locks* map mutation.")

(defun channel-call-lock (channel)
  "The call-serialization lock for CHANNEL, created on first use."
  (bt2:with-lock-held (*channel-call-locks-lock*)
    (or (gethash channel *channel-call-locks*)
        (setf (gethash channel *channel-call-locks*)
              (bt2:make-lock :name "chamber-channel-call")))))

(defun locked-grpc-call (channel method request &rest args)
  "ag-grpc:grpc-call serialized per CHANNEL. Use this instead of calling
   ag-grpc:grpc-call directly on a shared channel — see the note above."
  (bt2:with-lock-held ((channel-call-lock channel))
    (apply #'ag-grpc:grpc-call channel method request args)))

(defun chamber-enabled-p ()
  (config-value :chamber-enabled))

(defun ensure-chamber-channel ()
  "Get or create gRPC channel to Chamber."
  (unless *chamber-channel*
    (let* ((url (config-value :chamber-url))
           (host (if url
                     (let ((pos (search "://" url)))
                       (if pos (subseq url (+ pos 3)) url))
                     "127.0.0.1"))
           (port-str (let ((colon (position #\: host :from-end t)))
                       (when colon (subseq host (1+ colon)))))
           (host-only (let ((colon (position #\: host :from-end t)))
                        (if colon (subseq host 0 colon) host)))
           (port (if port-str
                     (parse-integer port-str :junk-allowed t)
                     (config-value :chamber-port 9444))))
      (setf *chamber-channel*
            (ag-grpc:make-channel host-only port
                                  :timeout (config-value :chamber-rpc-timeout 10)))))
  *chamber-channel*)

(define-condition chamber-rpc-error (error)
  ((method :initarg :method :reader chamber-rpc-error-method)
   (cause  :initarg :cause  :reader chamber-rpc-error-cause))
  (:report (lambda (c stream)
             (format stream "Chamber RPC failed (~A): ~A"
                     (chamber-rpc-error-method c)
                     (chamber-rpc-error-cause c)))))

(defun reset-chamber-channel ()
  "Drop the cached channel and try to close its connection, so the next
   call rebuilds it fresh. A poisoned channel can otherwise wedge every
   subsequent request."
  (let ((stale *chamber-channel*))
    (setf *chamber-channel* nil)
    (when stale
      (handler-case (ag-grpc:channel-close stale) (error () nil)))))

(defun chamber-call (method request response-type &key owner repo-name write-p)
  "Make a unary gRPC call to Chamber. Translates transport errors and
hard wall-clock timeouts into a chamber-rpc-error condition so callers
can fall back to direct git instead of crashing the server thread.

The bt2:with-timeout wrapper is a hard deadline: ag-grpc's own per-call
timeout only covers the receive path, not connection establishment or
internal stalls, so without this any wedge in the gRPC stack would block
the Hunchentoot worker forever."
  (let ((deadline (config-value :chamber-rpc-timeout 10)))
    (handler-case
        (bt2:with-timeout (deadline)
          (if (multi-chamber-p)
              (router-call method request response-type
                           :owner owner :repo-name repo-name :write-p write-p)
              (locked-grpc-call (ensure-chamber-channel) method request
                                :response-type response-type
                                :timeout deadline)))
      (bt2:timeout ()
        (llog:warn "Chamber RPC timed out" :method method :deadline deadline)
        (reset-chamber-channel)
        (error 'chamber-rpc-error :method method
                                  :cause (format nil "deadline exceeded (~As)" deadline)))
      (error (e)
        (llog:warn "Chamber RPC failed" :method method
                                         :error (princ-to-string e))
        (reset-chamber-channel)
        (error 'chamber-rpc-error :method method :cause e)))))

(defmacro with-chamber-fallback (fallback-form &body body)
  "Run BODY; if a chamber-rpc-error escapes, evaluate FALLBACK-FORM instead.
   Used to keep cave responsive when Chamber's RPC layer hiccups — read paths
   degrade gracefully to direct git calls on the local disk."
  `(handler-case (progn ,@body)
     (chamber-rpc-error () ,fallback-form)))

(defmacro chamber-or (chamber-form direct-form)
  "If chamber is enabled, evaluate CHAMBER-FORM. On chamber-rpc-error (or
   when chamber is disabled), evaluate DIRECT-FORM instead. The intended
   shape of every chamber-* read function."
  `(if (chamber-enabled-p)
       (handler-case ,chamber-form
         (chamber-rpc-error () ,direct-form))
       ,direct-form))

;;; --- Read operations ---

(defun chamber-get-tree (owner repo-name &key (ref "HEAD") (path ""))
  "Get directory listing. Returns list of plists or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetTree"
                                (make-instance 'cave::get-tree-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-tree-response
                                :owner owner :repo-name repo-name)))
        (mapcar (lambda (e)
                  (list :mode (slot-value e 'cave::mode)
                        :type (slot-value e 'cave::entry-type)
                        :hash (slot-value e 'cave::hash)
                        :size (slot-value e 'cave::size)
                        :name (slot-value e 'cave::name)))
                (coerce (slot-value resp 'cave::entries) 'list)))

      (git-tree (repo-disk-path owner repo-name) :ref ref :path path)))

;; Per-directory "last commit" info is immutable for a given tree-tip sha, so
;; cache it keyed by (owner repo tip-sha dir). No chamber RPC exists for this
;; yet; it runs git directly against the local bare repo and degrades to an
;; empty table (no commit column) on any error.
(defvar *tree-commits-cache* (make-hash-table :test 'equal))
(defvar *tree-commits-cache-lock* (bt2:make-lock :name "tree-commits-cache"))
(defparameter *tree-commits-cache-max* 512)

(defun chamber-tree-last-commits (owner repo-name ref path entry-names)
  "Return a hash-table mapping each immediate child NAME under PATH at REF to its
   most-recent-touching commit plist (:hash :short-hash :subject :author :time)."
  (handler-case
      (let* ((disk (repo-disk-path owner repo-name))
             (tip (git-rev-parse disk ref)))
        (if (null tip)
            (make-hash-table :test 'equal)
            (let ((key (list owner repo-name tip (or path ""))))
              (or (bt2:with-lock-held (*tree-commits-cache-lock*)
                    (gethash key *tree-commits-cache*))
                  (let ((res (git-tree-last-commits disk ref (or path "") entry-names)))
                    (bt2:with-lock-held (*tree-commits-cache-lock*)
                      (when (>= (hash-table-count *tree-commits-cache*)
                                *tree-commits-cache-max*)
                        (clrhash *tree-commits-cache*))
                      (setf (gethash key *tree-commits-cache*) res))
                    res)))))
    (error () (make-hash-table :test 'equal))))

;; Language byte totals are also immutable per tree-tip sha; same caching story.
(defvar *language-stats-cache* (make-hash-table :test 'equal))
(defvar *language-stats-cache-lock* (bt2:make-lock :name "language-stats-cache"))
(defparameter *language-stats-cache-max* 512)

(defun chamber-language-stats (owner repo-name ref)
  "Return a list of (name color bytes) for recognized languages at REF, cached
   by tree-tip sha. Empty list on any error."
  (handler-case
      (let* ((disk (repo-disk-path owner repo-name))
             (tip (git-rev-parse disk ref)))
        (if (null tip)
            nil
            (let ((key (list owner repo-name tip)))
              (multiple-value-bind (hit present)
                  (bt2:with-lock-held (*language-stats-cache-lock*)
                    (gethash key *language-stats-cache*))
                (if present
                    hit
                    (let ((res (git-language-stats disk ref)))
                      (bt2:with-lock-held (*language-stats-cache-lock*)
                        (when (>= (hash-table-count *language-stats-cache*)
                                  *language-stats-cache-max*)
                          (clrhash *language-stats-cache*))
                        (setf (gethash key *language-stats-cache*) res))
                      res))))))
    (error () nil)))

(defun chamber-get-blob (owner repo-name ref path)
  "Get file content as string. Returns string or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlob"
                                (make-instance 'cave::get-blob-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-response
                                :owner owner :repo-name repo-name)))
        (when (slot-value resp 'cave::found)
          (slot-value resp 'cave::content)))
      (git-blob (repo-disk-path owner repo-name) ref path)))

(defun chamber-get-blob-bytes (owner repo-name ref path)
  "Get file content as byte vector. Returns vector or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlobBytes"
                                (make-instance 'cave::get-blob-bytes-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-bytes-response
                                :owner owner :repo-name repo-name)))
        (when (slot-value resp 'cave::found)
          (slot-value resp 'cave::content)))
      (git-blob-bytes (repo-disk-path owner repo-name) ref path)))

(defun chamber-get-blob-info (owner repo-name ref path)
  "Get blob hash, size, is-binary. Returns plist or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlobInfo"
                                (make-instance 'cave::get-blob-info-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-info-response
                                :owner owner :repo-name repo-name)))
        (when (slot-value resp 'cave::found)
          (list :hash (slot-value resp 'cave::hash)
                :size (slot-value resp 'cave::size)
                :is-binary (slot-value resp 'cave::is-binary))))
      (let* ((disk-path (repo-disk-path owner repo-name))
             (hash (git-blob-hash disk-path ref path))
             (size (git-blob-size disk-path ref path)))
        (when hash
          (list :hash hash :size (or size 0)
                :is-binary (git-blob-binary-check disk-path ref path))))))

(defun chamber-get-commit (owner repo-name hash)
  "Get commit details + diff + stat. Returns plist or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetCommit"
                                (make-instance 'cave::get-commit-request
                                               :owner owner :repo-name repo-name
                                               :hash hash)
                                'cave::get-commit-response
                                :owner owner :repo-name repo-name)))
        (when (slot-value resp 'cave::found)
          (let ((c (slot-value resp 'cave::commit)))
            (list :commit (list :hash (slot-value c 'cave::hash)
                                :short-hash (slot-value c 'cave::short-hash)
                                :author (slot-value c 'cave::author)
                                :date (slot-value c 'cave::date)
                                :subject (slot-value c 'cave::subject))
                  :diff (slot-value resp 'cave::diff)
                  :stat (slot-value resp 'cave::stat)))))
      (let* ((disk-path (repo-disk-path owner repo-name))
             (commit (git-show-commit disk-path hash)))
        (when commit
          (list :commit commit
                :diff (git-commit-diff disk-path hash)
                :stat (git-commit-stat disk-path hash))))))

(defun chamber-get-log (owner repo-name &key (limit 20) branch)
  "Get commit log. Returns list of plists."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetLog"
                                (make-instance 'cave::get-log-request
                                               :owner owner :repo-name repo-name
                                               :branch (or branch "")
                                               :limit limit)
                                'cave::get-log-response
                                :owner owner :repo-name repo-name)))
        (mapcar (lambda (c)
                  (list :hash (slot-value c 'cave::hash)
                        :short-hash (slot-value c 'cave::short-hash)
                        :author (slot-value c 'cave::author)
                        :date (slot-value c 'cave::date)
                        :subject (slot-value c 'cave::subject)))
                (coerce (slot-value resp 'cave::commits) 'list)))
      (git-log (repo-disk-path owner repo-name) :limit limit :branch branch)))

(defun chamber-get-branches (owner repo-name)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBranches"
                                (make-instance 'cave::get-branches-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-branches-response
                                :owner owner :repo-name repo-name)))
        (coerce (slot-value resp 'cave::branches) 'list))
      (git-branches (repo-disk-path owner repo-name))))

(defun chamber-get-tags (owner repo-name)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetTags"
                                (make-instance 'cave::get-tags-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-tags-response
                                :owner owner :repo-name repo-name)))
        (coerce (slot-value resp 'cave::tags) 'list))
      (git-tags (repo-disk-path owner repo-name))))

(defun chamber-get-default-branch (owner repo-name)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetDefaultBranch"
                                (make-instance 'cave::get-default-branch-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-default-branch-response
                                :owner owner :repo-name repo-name)))
        (slot-value resp 'cave::branch))
      (git-default-branch (repo-disk-path owner repo-name))))

(defun chamber-is-empty (owner repo-name)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/IsEmpty"
                                (make-instance 'cave::is-empty-request
                                               :owner owner :repo-name repo-name)
                                'cave::is-empty-response
                                :owner owner :repo-name repo-name)))
        (slot-value resp 'cave::empty))
      (git-repo-empty-p (repo-disk-path owner repo-name))))

(defun chamber-get-diff-merge-base (owner repo-name target source)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetDiffMergeBase"
                                (make-instance 'cave::get-diff-merge-base-request
                                               :owner owner :repo-name repo-name
                                               :target target :source source)
                                'cave::get-diff-response
                                :owner owner :repo-name repo-name)))
        (slot-value resp 'cave::base-ref))
      (git-diff-merge-base (repo-disk-path owner repo-name) target source)))

(defun chamber-get-commit-count (owner repo-name &key branch)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetCommitCount"
                                (make-instance 'cave::get-commit-count-request
                                               :owner owner :repo-name repo-name
                                               :branch (or branch ""))
                                'cave::get-commit-count-response
                                :owner owner :repo-name repo-name)))
        (slot-value resp 'cave::commit-count))
      (git-commit-count (repo-disk-path owner repo-name) :branch branch)))

(defun chamber-find-readme (owner repo-name &key (ref "HEAD"))
  "Find README file. Returns plist with :name or NIL."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/FindReadme"
                                (make-instance 'cave::find-readme-request
                                               :owner owner :repo-name repo-name
                                               :ref ref)
                                'cave::find-readme-response
                                :owner owner :repo-name repo-name)))
        (when (slot-value resp 'cave::found)
          (list :name (slot-value resp 'cave::name))))
      (git-readme-path (repo-disk-path owner repo-name) :ref ref)))

;;; --- Write operations ---

(defun chamber-merge-branch (owner repo-name target source &key author message strategy)
  "Merge source into target with STRATEGY (\"merge\" (default, --no-ff),
   \"squash\", or \"fast-forward-only\"). Returns (VALUES success-p error-string)."
  (let ((strategy (or strategy "merge")))
    (chamber-or
        (let ((resp (chamber-call "/cave.chamber.Chamber/MergeBranch"
                                  (make-instance 'cave::merge-branch-request
                                                 :owner owner :repo-name repo-name
                                                 :target target :source source
                                                 :author (or author "")
                                                 :message (or message "")
                                                 :strategy strategy)
                                  'cave::merge-branch-response
                                  :owner owner :repo-name repo-name :write-p t)))
          (values (slot-value resp 'cave::ok)
                  (slot-value resp 'cave::error-message)))
        (git-merge-branch (repo-disk-path owner repo-name) target source
                           :author author :message message :strategy strategy))))

(defun chamber-delete-branch (owner repo-name branch)
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/DeleteBranch"
                                (make-instance 'cave::delete-branch-request
                                               :owner owner :repo-name repo-name
                                               :branch branch)
                                'cave::delete-branch-response
                                :owner owner :repo-name repo-name :write-p t)))
        (slot-value resp 'cave::ok))
      (git-delete-branch (repo-disk-path owner repo-name) branch)))

;;; --- Mirror/clone operations ---

(defun chamber-clone-from-url (owner repo-name url &key auth-token)
  "Clone bare repo from external URL. Returns (VALUES success-p error-string)."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/CloneFromURL"
                                (make-instance 'cave::clone-from-url-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::clone-from-url-response
                                :owner owner :repo-name repo-name :write-p t)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error-message)))
      (git-clone-bare-from-url url (repo-disk-path owner repo-name)
                                :auth-token auth-token)))

(defun chamber-push-mirror (owner repo-name url &optional auth-token)
  "Push all refs to remote. Returns (VALUES success-p error-string)."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/PushMirror"
                                (make-instance 'cave::push-mirror-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::push-mirror-response
                                :owner owner :repo-name repo-name :write-p t)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error-message)))
      (git-push-mirror (repo-disk-path owner repo-name) url auth-token)))

(defun chamber-pull-mirror (owner repo-name url &optional auth-token)
  "Fetch all refs from remote. Returns (VALUES success-p error-string)."
  (chamber-or
      (let ((resp (chamber-call "/cave.chamber.Chamber/PullMirror"
                                (make-instance 'cave::pull-mirror-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::pull-mirror-response
                                :owner owner :repo-name repo-name :write-p t)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error-message)))
      (git-pull-mirror (repo-disk-path owner repo-name) url auth-token)))
