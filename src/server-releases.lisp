(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Routes: Releases

(defparameter *release-asset-max-bytes* (* 100 1024 1024)
  "Per-asset upload cap. 100 MB.")

(defun release-asset-dir (repo-id release-id)
  "Absolute path to a release's asset directory. Created on demand."
  (let ((dir (merge-pathnames (format nil "releases/~A/~A/" repo-id release-id)
                              (data-dir))))
    (ensure-directories-exist dir)
    dir))

(defun sanitize-asset-filename (name)
  "Strip path components and disallowed chars from an uploaded filename."
  (let* ((bare (file-namestring (or name "asset")))
         (clean (with-output-to-string (s)
                  (loop for c across bare
                        do (write-char
                            (if (or (alphanumericp c)
                                    (find c ".-_+" :test #'char=))
                                c #\_)
                            s)))))
    (if (zerop (length clean)) "asset" clean)))

(defun member-of-repo-p (repo)
  (and *current-user-id* (repo-member-role (getf repo :id) *current-user-id*)))

(easy-routes:defroute releases-page ("/:owner/:repo-name/releases" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((releases (list-releases (getf repo :id)))
           (assets-by-release
            (let ((h (make-hash-table)))
              (dolist (r releases)
                (setf (gethash (getf r :id) h)
                      (list-release-assets (getf r :id))))
              h)))
      (html-response
       (view-releases :owner-name owner :repo repo
                      :releases releases
                      :assets-by-release assets-by-release
                      :can-create (and (member-of-repo-p repo) t))))))

(easy-routes:defroute new-release-page ("/:owner/:repo-name/releases/new" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (member-of-repo-p repo)
      (return-from new-release-page (not-found)))
    (let* ((disk-path (repo-disk-path owner repo-name))
           (existing-tags (git-tags disk-path)))
      (html-response
       (view-new-release :owner-name owner :repo repo :existing-tags existing-tags)))))

(easy-routes:defroute create-release-submit ("/:owner/:repo-name/releases/new" :method :post) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (member-of-repo-p repo)
      (return-from create-release-submit (not-found)))
    (let* ((tag-name (string-trim '(#\Space) (or (hunchentoot:post-parameter "tag_name") "")))
           (release-name (hunchentoot:post-parameter "name"))
           (body (or (hunchentoot:post-parameter "body") ""))
           (is-prerelease (equal (hunchentoot:post-parameter "is_prerelease") "1"))
           (disk-path (repo-disk-path owner repo-name))
           (default-branch (or (chamber-get-default-branch owner repo-name) "main")))
      (when (zerop (length tag-name))
        (return-from create-release-submit
          (html-response (view-new-release :owner-name owner :repo repo
                                            :existing-tags (git-tags disk-path)
                                            :error "Tag name is required."))))
      (when (find-release-by-tag (getf repo :id) tag-name)
        (return-from create-release-submit
          (html-response (view-new-release :owner-name owner :repo repo
                                            :existing-tags (git-tags disk-path)
                                            :error (format nil "Release ~A already exists." tag-name)))))
      ;; If the tag doesn't exist in git, create it at HEAD of the default branch.
      (unless (git-tag-exists-p disk-path tag-name)
        (unless (git-create-tag disk-path tag-name default-branch
                                :message (or release-name tag-name))
          (return-from create-release-submit
            (html-response (view-new-release :owner-name owner :repo repo
                                              :existing-tags (git-tags disk-path)
                                              :error (format nil "Could not create git tag ~A." tag-name))))))
      ;; Auto-generate notes from commits since the previous tag when the body
      ;; was left blank.
      (when (zerop (length (string-trim '(#\Space #\Newline #\Return #\Tab) body)))
        (setf body (or (git-release-notes disk-path tag-name) body)))
      (create-release :repo-id (getf repo :id)
                      :tag-name tag-name
                      :name release-name
                      :body body
                      :is-prerelease is-prerelease
                      :created-by *current-user-id*)
      (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag-name)))))

(easy-routes:defroute release-detail-page ("/:owner/:repo-name/releases/:tag" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from release-detail-page (not-found)))
      (html-response
       (view-release :owner-name owner :repo repo
                     :release release
                     :assets (list-release-assets (getf release :id))
                     :can-edit (and (member-of-repo-p repo) t))))))

(easy-routes:defroute delete-release-submit
    ("/:owner/:repo-name/releases/:tag/delete" :method :post) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (member-of-repo-p repo)
      (return-from delete-release-submit (not-found)))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (when release
        ;; Wipe assets on disk before the DB cascades the rows.
        (let ((dir (release-asset-dir (getf repo :id) (getf release :id))))
          (handler-case (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
            (error () nil)))
        (delete-release (getf release :id))))
    (hunchentoot:redirect (format nil "/~A/~A/releases" owner repo-name))))

(easy-routes:defroute upload-release-asset
    ("/:owner/:repo-name/releases/:tag/upload" :method :post) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (member-of-repo-p repo)
      (return-from upload-release-asset (not-found)))
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from upload-release-asset (not-found)))
      ;; Reject early on Content-Length if available.
      (let ((cl (hunchentoot:header-in* :content-length)))
        (when (and cl (> (parse-integer cl :junk-allowed t) *release-asset-max-bytes*))
          (setf (hunchentoot:return-code*) 413)
          (return-from upload-release-asset "Asset too large.")))
      (let ((upload (hunchentoot:post-parameter "asset")))
        (unless (consp upload)
          (return-from upload-release-asset
            (progn (setf (hunchentoot:return-code*) 400) "No file in upload.")))
        (let* ((temp-path (first upload))
               (orig-name (second upload))
               (content-type (third upload))
               (clean-name (sanitize-asset-filename orig-name))
               (size (with-open-file (s temp-path :element-type '(unsigned-byte 8))
                       (file-length s))))
          (when (> size *release-asset-max-bytes*)
            (ignore-errors (delete-file temp-path))
            (setf (hunchentoot:return-code*) 413)
            (return-from upload-release-asset "Asset too large."))
          (when (find-release-asset-by-name (getf release :id) clean-name)
            (ignore-errors (delete-file temp-path))
            (setf (hunchentoot:return-code*) 409)
            (return-from upload-release-asset "An asset with that name already exists."))
          (let* ((dir (release-asset-dir (getf repo :id) (getf release :id)))
                 (dest (merge-pathnames clean-name dir)))
            (uiop:rename-file-overwriting-target temp-path dest)
            (create-release-asset :release-id (getf release :id)
                                  :name clean-name
                                  :content-type content-type
                                  :size size
                                  :storage-path (namestring dest)
                                  :uploaded-by *current-user-id*))))
      (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag)))))

(easy-routes:defroute delete-release-asset-submit
    ("/:owner/:repo-name/releases/:tag/assets/:asset-id/delete" :method :post) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (member-of-repo-p repo)
      (return-from delete-release-asset-submit (not-found)))
    (let* ((aid (parse-integer asset-id :junk-allowed t))
           (asset (when aid (find-release-asset-by-id aid))))
      (when asset
        (ignore-errors (delete-file (getf asset :storage-path)))
        (delete-release-asset aid)))
    (hunchentoot:redirect (format nil "/~A/~A/releases/~A" owner repo-name tag))))

(easy-routes:defroute release-asset-download
    ("/:owner/:repo-name/releases/download/:tag/:filename" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let ((release (find-release-by-tag (getf repo :id) tag)))
      (unless release (return-from release-asset-download (not-found)))
      (let ((asset (find-release-asset-by-name (getf release :id) filename)))
        (unless (and asset (probe-file (getf asset :storage-path)))
          (return-from release-asset-download (not-found)))
        (increment-asset-download-count (getf asset :id))
        (setf (hunchentoot:content-type*) (or (getf asset :content-type)
                                              "application/octet-stream"))
        (setf (hunchentoot:header-out :content-disposition)
              (format nil "attachment; filename=\"~A\"" (getf asset :name)))
        (setf (hunchentoot:header-out :content-length) (princ-to-string (getf asset :size)))
        (let ((out (hunchentoot:send-headers)))
          (with-open-file (in (getf asset :storage-path) :element-type '(unsigned-byte 8))
            (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
              (loop for n = (read-sequence buf in)
                    while (plusp n)
                    do (write-sequence buf out :end n))))
          (finish-output out))
        nil))))

(defun %server-object-store ()
  "Object-store descriptor for the SERVER to read artifacts for download. Mirrors
   the runner's store: an rclone remote (:artifact-store-remote, with read creds)
   or a local dir (:artifact-store-dir — a volume shared with a co-located runner)."
  (let ((remote (config-value :artifact-store-remote "")))
    (if (plusp (length remote))
        (list :backend :s3 :base (string-right-trim "/" remote))
        (list :backend :dir :root (config-value :artifact-store-dir "/var/cache/cave-runner/store")))))

(easy-routes:defroute artifact-download-route
    ("/:owner/:repo-name/runs/w/:run-id/artifacts/:artifact-id" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((rid (parse-integer run-id :junk-allowed t))
           (aid (parse-integer artifact-id :junk-allowed t))
           (run (when rid (find-workflow-run rid)))
           (art (when aid (find-artifact aid))))
      (unless (and run art
                   (= (getf run :repo-id) (getf repo :id))
                   (eql (getf art :workflow-run-id) rid))
        (return-from artifact-download-route (not-found)))
      (let* ((tmp (format nil "/tmp/cave-art-~A-~A.tar.gz" rid aid))
             (got (ignore-errors (%store-get (%server-object-store) (getf art :object-path) tmp))))
        (if (not got)
            (progn (setf (hunchentoot:return-code*) 404)
                   "artifact data is not available to this server (check :artifact-store-* config)")
            (progn
              (setf (hunchentoot:header-out "Content-Disposition")
                    (format nil "attachment; filename=\"~A.tar.gz\""
                            (substitute #\_ #\" (getf art :name))))
              (prog1 (hunchentoot:handle-static-file tmp "application/gzip")
                (ignore-errors (delete-file tmp)))))))))

(easy-routes:defroute workflow-run-detail-page
    ("/:owner/:repo-name/runs/w/:run-id" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((rid (parse-integer run-id :junk-allowed t))
           (run (when rid (find-workflow-run rid))))
      (unless (and run (= (getf run :repo-id) (getf repo :id)))
        (return-from workflow-run-detail-page (not-found)))
      (let ((jobs (list-workflow-jobs rid)))
        (html-response
         (view-workflow-run :owner-name owner :repo repo :run run
                            :jobs (mapcar (lambda (j)
                                            (list :job j
                                                  :steps (list-workflow-steps (getf j :id))))
                                          jobs)
                            :artifacts (list-run-artifacts rid)))))))

(easy-routes:defroute rerun-workflow-route
    ("/:owner/:repo-name/runs/w/:run-id/rerun" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from rerun-workflow-route (not-found)))
      (unless (repo-member-role (getf repo :id) *current-user-id*)
        (setf (hunchentoot:return-code*) 403)
        (return-from rerun-workflow-route "Forbidden"))
      (let* ((rid (parse-integer run-id :junk-allowed t))
             (run (when rid (find-workflow-run rid))))
        (unless (and run (= (getf run :repo-id) (getf repo :id)))
          (return-from rerun-workflow-route (not-found)))
        (handler-case (rerun-workflow rid)
          (error (e) (llog:warn "rerun-workflow failed"
                                :run rid :error (princ-to-string e))))
        (hunchentoot:redirect (format nil "/~A/~A/runs" owner repo-name))
        nil))))

(defun handle-workflow-logs-sse (uri)
  "Handle SSE streaming for workflow run logs. Called from acceptor dispatch."
  ;; Parse owner/repo and run-id from URI: /:owner/:repo/runs/w/:id/logs
  (let* ((w-pos (search "/runs/w/" uri))
         (prefix (subseq uri 1 w-pos))
         (prefix-slash (position #\/ prefix))
         (owner (when prefix-slash (subseq prefix 0 prefix-slash)))
         (repo-name (when prefix-slash (subseq prefix (1+ prefix-slash))))
         (id-start (+ w-pos 8))
         (id-end (position #\/ uri :start id-start))
         (run-id (parse-integer (subseq uri id-start id-end) :junk-allowed t))
         (run (when run-id (find-workflow-run run-id)))
         (repo (when (and owner repo-name) (find-repo owner repo-name))))
    ;; Authorization: the run must exist, belong to a repo the caller can see,
    ;; and actually be the run for the repo named in the URL (no cross-repo
    ;; access by guessing run-ids). Mirrors workflow-run-detail-page.
    (unless (and run repo
                 (repo-visible-p repo)
                 (= (getf run :repo-id) (getf repo :id)))
      (return-from handle-workflow-logs-sse nil))
    ;; Send SSE headers
    (setf (hunchentoot:content-type*) "text/event-stream")
    (setf (hunchentoot:header-out "Cache-Control") "no-cache")
    (setf (hunchentoot:header-out "X-Accel-Buffering") "no")
    (let ((stream (hunchentoot:send-headers))
          (sent-lengths (make-hash-table))
          (prev-statuses (make-hash-table :test #'equal)))
      (flet ((sse-send (event data)
               (write-sequence
                (flexi-streams:string-to-octets
                 (format nil "event: ~A~%data: ~A~%~%" event data)
                 :external-format :utf-8)
                stream)
               (force-output stream)))
        (handler-case
            (loop repeat 600
                  do (let* ((refreshed-run (find-workflow-run run-id))
                            (run-status (getf refreshed-run :status))
                            (jobs (list-workflow-jobs run-id))
                            (any-active nil))
                       ;; Send run status changes
                       (unless (equal run-status (gethash "run" prev-statuses))
                         (setf (gethash "run" prev-statuses) run-status)
                         (sse-send "run-status" run-status))
                       ;; Send step updates
                       (dolist (job jobs)
                         (dolist (step (list-workflow-steps (getf job :id)))
                           (let* ((step-id (getf step :id))
                                  (log-text (getf step :log))
                                  (log-len (if (and log-text (not (eq log-text :null)))
                                               (length log-text) 0))
                                  (prev-len (gethash step-id sent-lengths 0))
                                  (status (getf step :status))
                                  (status-key (format nil "s~A" step-id)))
                             ;; New log content
                             (when (> log-len prev-len)
                               (let* ((new-text (subseq log-text prev-len))
                                      (escaped (with-output-to-string (s)
                                                 (loop for ch across new-text
                                                       do (if (char= ch #\Newline)
                                                              (write-string "\\n" s)
                                                              (write-char ch s))))))
                                 (sse-send "step-log" (format nil "~A ~A" step-id escaped))
                                 (setf (gethash step-id sent-lengths) log-len)))
                             ;; Status changes
                             (unless (equal status (gethash status-key prev-statuses))
                               (setf (gethash status-key prev-statuses) status)
                               (sse-send "step-status" (format nil "~A ~A" step-id status)))
                             (when (member status '("pending" "running") :test #'equal)
                               (setf any-active t)))))
                       ;; Done?
                       (when (and (member run-status '("success" "failure" "cancelled")
                                          :test #'equal)
                                  (not any-active))
                         (sse-send "done" run-status)
                         (return)))
                     (sleep 1))
          (error () nil))))))

;; Automation definition management
(easy-routes:defroute repo-add-automation-submit
    ("/:owner/:repo-name/settings/automations" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-add-automation-submit)
    (let ((name (hunchentoot:post-parameter "name"))
          (trigger (hunchentoot:post-parameter "trigger"))
          (command (hunchentoot:post-parameter "command"))
          (runner-labels (hunchentoot:post-parameter "runner_labels"))
          (timeout (parse-integer (or (hunchentoot:post-parameter "timeout") "60")
                                  :junk-allowed t)))
      (when (and name command (not (uiop:emptyp name)) (not (uiop:emptyp command)))
        (handler-case
            (create-automation-definition
             :repo-id (getf repo :id)
             :name name :trigger trigger :command command
             :runner-labels (or runner-labels "")
             :timeout-seconds (or timeout 60))
          (error () nil))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-delete-automation-submit
    ("/:owner/:repo-name/settings/automations/:auto-id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-delete-automation-submit)
    (let ((aid (parse-integer auto-id :junk-allowed t)))
      (when aid (delete-automation-definition aid (getf repo :id))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

;; Repo-scoped runner management
(easy-routes:defroute repo-create-runner-token
    ("/:owner/:repo-name/settings/runners/token" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-create-runner-token)
    (let ((token (create-registration-token :scope "repo" :scope-id (getf repo :id)
                                            :created-by-id *current-user-id*)))
      (html-response
       (view-repo-settings :owner-name owner :repo repo
                           :members (list-repo-members (getf repo :id))
                           :checks (list-check-configs (getf repo :id))
                           :mirrors (list-mirrors (getf repo :id))
                           :webhooks (list-webhooks (getf repo :id))
                           :automations (list-automation-definitions (getf repo :id))
                           :runners (list-runners :scope "repo" :scope-id (getf repo :id))
                           :secrets (list-secret-names "repo" (getf repo :id))
                           :protected-branches (list-protected-branches (getf repo :id))
                           :deploy-keys (list-deploy-keys (getf repo :id))
                           :registration-token token)))))

(easy-routes:defroute repo-delete-runner
    ("/:owner/:repo-name/settings/runners/:runner-id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-delete-runner)
    (let ((rid (parse-integer runner-id :junk-allowed t)))
      (when rid (delete-runner rid)))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-add-webhook-submit
    ("/:owner/:repo-name/settings/webhooks" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-add-webhook-submit)
    (let ((url (hunchentoot:post-parameter "url"))
          (secret (hunchentoot:post-parameter "secret"))
          (events (hunchentoot:post-parameter "events")))
      (when (and url (not (uiop:emptyp url)))
        (create-webhook :repo-id (getf repo :id)
                        :url url
                        :secret (unless (uiop:emptyp secret) secret)
                        :events (or events "push,pull_request,issue"))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-delete-webhook-submit
    ("/:owner/:repo-name/settings/webhooks/:webhook-id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-delete-webhook-submit)
    (let ((wid (parse-integer webhook-id :junk-allowed t)))
      (when wid (delete-webhook wid (getf repo :id))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-add-mirror-submit
    ("/:owner/:repo-name/settings/mirrors" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-add-mirror-submit)
    (let ((direction (hunchentoot:post-parameter "direction"))
          (remote-url (hunchentoot:post-parameter "remote_url"))
          (auth-token (hunchentoot:post-parameter "auth_token"))
          (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                   :junk-allowed t)))
      (when (and direction remote-url (not (uiop:emptyp remote-url)))
        (let ((mirror (create-mirror :repo-id (getf repo :id)
                                     :direction direction
                                     :remote-url remote-url
                                     :auth-token (unless (uiop:emptyp auth-token) auth-token)
                                     :interval-minutes (or interval 60))))
          ;; Immediately sync pull mirrors
          (when (equal direction "pull")
            (let ((token (unless (uiop:emptyp auth-token) auth-token)))
              (multiple-value-bind (ok err)
                  (chamber-pull-mirror owner repo-name remote-url token)
                (update-mirror-sync (getf mirror :id)
                                    :error (unless ok err))))))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-delete-mirror-submit
    ("/:owner/:repo-name/settings/mirrors/:mirror-id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-delete-mirror-submit)
    (let ((mid (parse-integer mirror-id :junk-allowed t)))
      (when mid (delete-mirror mid (getf repo :id))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-add-check-submit
    ("/:owner/:repo-name/settings/checks" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-add-check-submit)
    (let ((name (hunchentoot:post-parameter "name"))
          (command (hunchentoot:post-parameter "command"))
          (timeout (parse-integer (or (hunchentoot:post-parameter "timeout") "60")
                                  :junk-allowed t)))
      (when (and name command (not (uiop:emptyp name)) (not (uiop:emptyp command)))
        (handler-case
            (create-check-config :repo-id (getf repo :id)
                                 :name name :command command
                                 :timeout-seconds (or timeout 60))
          (error () nil))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-delete-check-submit
    ("/:owner/:repo-name/settings/checks/:check-id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-delete-check-submit)
    (let ((cid (parse-integer check-id :junk-allowed t)))
      (when cid (delete-check-config cid (getf repo :id))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

