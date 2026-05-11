;;; chamber-client.lisp — Client for Chamber git storage service
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Provides chamber-* functions that mirror the git.lisp API.
;;; When Chamber is enabled, calls go through gRPC.
;;; When disabled, falls back to direct git.lisp calls (backward compat).

(in-package #:cave)

(defvar *chamber-channel* nil "gRPC channel to Chamber service.")

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
            (ag-grpc:make-channel host-only port :timeout nil))))
  *chamber-channel*)

(defun chamber-call (method request response-type)
  "Make a unary gRPC call to Chamber."
  (ag-grpc:grpc-call (ensure-chamber-channel) method request
                      :response-type response-type))

;;; --- Read operations ---

(defun chamber-get-tree (owner repo-name &key (ref "HEAD") (path ""))
  "Get directory listing. Returns list of plists or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetTree"
                                (make-instance 'cave::get-tree-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-tree-response)))
        (mapcar (lambda (e)
                  (list :mode (slot-value e 'cave::mode)
                        :type (slot-value e 'cave::type)
                        :hash (slot-value e 'cave::hash)
                        :size (slot-value e 'cave::size)
                        :name (slot-value e 'cave::name)))
                (coerce (slot-value resp 'cave::entries) 'list)))
      ;; Fallback: direct git
      (git-tree (repo-disk-path owner repo-name) :ref ref :path path)))

(defun chamber-get-blob (owner repo-name ref path)
  "Get file content as string. Returns string or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlob"
                                (make-instance 'cave::get-blob-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-response)))
        (when (slot-value resp 'cave::found)
          (slot-value resp 'cave::content)))
      (git-blob (repo-disk-path owner repo-name) ref path)))

(defun chamber-get-blob-bytes (owner repo-name ref path)
  "Get file content as byte vector. Returns vector or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlobBytes"
                                (make-instance 'cave::get-blob-bytes-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-bytes-response)))
        (when (slot-value resp 'cave::found)
          (slot-value resp 'cave::content)))
      (git-blob-bytes (repo-disk-path owner repo-name) ref path)))

(defun chamber-get-blob-info (owner repo-name ref path)
  "Get blob hash, size, is-binary. Returns plist or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBlobInfo"
                                (make-instance 'cave::get-blob-info-request
                                               :owner owner :repo-name repo-name
                                               :ref ref :path path)
                                'cave::get-blob-info-response)))
        (when (slot-value resp 'cave::found)
          (list :hash (slot-value resp 'cave::hash)
                :size (slot-value resp 'cave::size)
                :is-binary (slot-value resp 'cave::is-binary))))
      (let* ((disk-path (repo-disk-path owner repo-name))
             (hash (git-blob-hash disk-path ref path))
             (size (git-blob-size disk-path ref path)))
        (when hash
          (let ((content (git-blob disk-path ref path)))
            (list :hash hash :size (or size 0)
                  :is-binary (git-blob-binary-p content)))))))

(defun chamber-get-commit (owner repo-name hash)
  "Get commit details + diff + stat. Returns plist or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetCommit"
                                (make-instance 'cave::get-commit-request
                                               :owner owner :repo-name repo-name
                                               :hash hash)
                                'cave::get-commit-response)))
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
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetLog"
                                (make-instance 'cave::get-log-request
                                               :owner owner :repo-name repo-name
                                               :branch (or branch "")
                                               :limit limit)
                                'cave::get-log-response)))
        (mapcar (lambda (c)
                  (list :hash (slot-value c 'cave::hash)
                        :short-hash (slot-value c 'cave::short-hash)
                        :author (slot-value c 'cave::author)
                        :date (slot-value c 'cave::date)
                        :subject (slot-value c 'cave::subject)))
                (coerce (slot-value resp 'cave::commits) 'list)))
      (git-log (repo-disk-path owner repo-name) :limit limit :branch branch)))

(defun chamber-get-branches (owner repo-name)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetBranches"
                                (make-instance 'cave::get-branches-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-branches-response)))
        (coerce (slot-value resp 'cave::branches) 'list))
      (git-branches (repo-disk-path owner repo-name))))

(defun chamber-get-tags (owner repo-name)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetTags"
                                (make-instance 'cave::get-tags-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-tags-response)))
        (coerce (slot-value resp 'cave::tags) 'list))
      (git-tags (repo-disk-path owner repo-name))))

(defun chamber-get-default-branch (owner repo-name)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetDefaultBranch"
                                (make-instance 'cave::get-default-branch-request
                                               :owner owner :repo-name repo-name)
                                'cave::get-default-branch-response)))
        (slot-value resp 'cave::branch))
      (git-default-branch (repo-disk-path owner repo-name))))

(defun chamber-is-empty (owner repo-name)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/IsEmpty"
                                (make-instance 'cave::is-empty-request
                                               :owner owner :repo-name repo-name)
                                'cave::is-empty-response)))
        (slot-value resp 'cave::empty))
      (git-repo-empty-p (repo-disk-path owner repo-name))))

(defun chamber-get-diff-merge-base (owner repo-name target source)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetDiffMergeBase"
                                (make-instance 'cave::get-diff-merge-base-request
                                               :owner owner :repo-name repo-name
                                               :target target :source source)
                                'cave::get-diff-response)))
        (slot-value resp 'cave::base-ref))
      (git-diff-merge-base (repo-disk-path owner repo-name) target source)))

(defun chamber-get-commit-count (owner repo-name &key branch)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/GetCommitCount"
                                (make-instance 'cave::get-commit-count-request
                                               :owner owner :repo-name repo-name
                                               :branch (or branch ""))
                                'cave::get-commit-count-response)))
        (slot-value resp 'cave::count))
      (git-commit-count (repo-disk-path owner repo-name) :branch branch)))

(defun chamber-find-readme (owner repo-name &key (ref "HEAD"))
  "Find README file. Returns plist with :name or NIL."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/FindReadme"
                                (make-instance 'cave::find-readme-request
                                               :owner owner :repo-name repo-name
                                               :ref ref)
                                'cave::find-readme-response)))
        (when (slot-value resp 'cave::found)
          (list :name (slot-value resp 'cave::name))))
      (git-readme-path (repo-disk-path owner repo-name) :ref ref)))

;;; --- Write operations ---

(defun chamber-merge-branch (owner repo-name target source &key author message squash)
  "Merge source into target. Returns (VALUES success-p error-string)."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/MergeBranch"
                                (make-instance 'cave::merge-branch-request
                                               :owner owner :repo-name repo-name
                                               :target target :source source
                                               :author (or author "")
                                               :message (or message "")
                                               :squash (if squash t nil))
                                'cave::merge-branch-response)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error)))
      (git-merge-branch (repo-disk-path owner repo-name) target source
                         :author author :message message :squash squash)))

(defun chamber-delete-branch (owner repo-name branch)
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/DeleteBranch"
                                (make-instance 'cave::delete-branch-request
                                               :owner owner :repo-name repo-name
                                               :branch branch)
                                'cave::delete-branch-response)))
        (slot-value resp 'cave::ok))
      (git-delete-branch (repo-disk-path owner repo-name) branch)))

;;; --- Mirror/clone operations ---

(defun chamber-clone-from-url (owner repo-name url &key auth-token)
  "Clone bare repo from external URL. Returns (VALUES success-p error-string)."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/CloneFromURL"
                                (make-instance 'cave::clone-from-url-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::clone-from-url-response)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error)))
      (git-clone-bare-from-url url (repo-disk-path owner repo-name)
                                :auth-token auth-token)))

(defun chamber-push-mirror (owner repo-name url &optional auth-token)
  "Push all refs to remote. Returns (VALUES success-p error-string)."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/PushMirror"
                                (make-instance 'cave::push-mirror-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::push-mirror-response)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error)))
      (git-push-mirror (repo-disk-path owner repo-name) url auth-token)))

(defun chamber-pull-mirror (owner repo-name url &optional auth-token)
  "Fetch all refs from remote. Returns (VALUES success-p error-string)."
  (if (chamber-enabled-p)
      (let ((resp (chamber-call "/cave.chamber.Chamber/PullMirror"
                                (make-instance 'cave::pull-mirror-request
                                               :owner owner :repo-name repo-name
                                               :url url
                                               :auth-token (or auth-token ""))
                                'cave::pull-mirror-response)))
        (values (slot-value resp 'cave::ok)
                (slot-value resp 'cave::error)))
      (git-pull-mirror (repo-disk-path owner repo-name) url auth-token)))
