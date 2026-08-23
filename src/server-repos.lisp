(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Routes: Repos

;; Overview (default landing — README + clone URL)
(easy-routes:defroute repo-page ("/:owner/:repo-name" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((empty (chamber-is-empty owner repo-name))
           (default-branch (unless empty (or (chamber-get-default-branch owner repo-name) "main")))
           (branches (unless empty (chamber-get-branches owner repo-name)))
           (tags (unless empty (chamber-get-tags owner repo-name)))
           ;; Honor ?ref= so the README follows the switcher selection; fall back
           ;; to the default branch when absent or unknown.
           (ref-param (hunchentoot:get-parameter "ref"))
           (ref (if (and ref-param (member ref-param (append branches tags) :test #'equal))
                    ref-param
                    default-branch))
           (readme-entry (unless empty (chamber-find-readme owner repo-name :ref ref)))
           (raw-base-url (when readme-entry
                           (format nil "~A/~A/~A/raw/~A?path="
                                   (config-value :base-url "http://localhost:8080")
                                   owner repo-name
                                   (or ref "HEAD"))))
           ;; Cheap pre-check: lookup the README's blob sha via get-blob-info,
           ;; then consult the rendered-HTML cache before we ever read or render.
           (readme-info (when readme-entry
                          (chamber-get-blob-info owner repo-name ref
                                                 (getf readme-entry :name))))
           (cache-key (when readme-info
                        (cons (getf readme-info :hash) raw-base-url)))
           (cached-html (when cache-key (readme-cache-get cache-key)))
           (readme-html
            (or cached-html
                (let ((content (when readme-entry
                                 (chamber-get-blob owner repo-name ref
                                                   (getf readme-entry :name)))))
                  (when content
                    (let ((html (if (search ".md" (string-downcase
                                                   (getf readme-entry :name)))
                                    (render-markdown content :raw-base-url raw-base-url)
                                    (format nil "<pre>~A</pre>"
                                            (spinneret::escape-string content)))))
                      (when cache-key (readme-cache-put cache-key html))
                      html))))))
      (html-response
       (view-repo :owner-name owner :repo repo :empty empty
                  :default-branch default-branch :current-ref ref
                  :branches branches :tags tags
                  :readme-html readme-html
                  :readme-filename (when readme-entry (getf readme-entry :name)))))))

;; Code (file browser)
(easy-routes:defroute code-page ("/:owner/:repo-name/code" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((empty (chamber-is-empty owner repo-name))
           (default-branch (unless empty (or (chamber-get-default-branch owner repo-name) "main")))
           (branches (unless empty (chamber-get-branches owner repo-name)))
           (tags (unless empty (chamber-get-tags owner repo-name)))
           ;; Honor ?ref= so the file browser follows the switcher selection.
           (ref-param (hunchentoot:get-parameter "ref"))
           (ref (if (and ref-param (member ref-param (append branches tags) :test #'equal))
                    ref-param
                    default-branch))
           (commit-count (unless empty (chamber-get-commit-count owner repo-name :branch ref)))
           (file-tree (unless empty (chamber-get-tree owner repo-name :ref ref)))
           (last-commits (unless empty
                           (chamber-tree-last-commits
                            owner repo-name ref ""
                            (mapcar (lambda (e) (getf e :name)) file-tree))))
           (language-stats (unless empty (chamber-language-stats owner repo-name ref)))
           (recent-commits (unless empty (chamber-get-log owner repo-name :limit 10 :branch ref))))
      (if empty
          (hunchentoot:redirect (format nil "/~A/~A" owner repo-name))
          (html-response
           (view-code :owner-name owner :repo repo
                      :branches branches :tags tags
                      :default-branch default-branch :current-ref ref
                      :commit-count commit-count
                      :recent-commits recent-commits
                      :last-commits last-commits
                      :language-stats language-stats
                      :signatures (commit-signatures-by-sha
                                   (getf repo :id)
                                   (mapcar (lambda (c) (getf c :hash)) recent-commits))
                      :file-tree file-tree))))))

;; Fork
(easy-routes:defroute repo-watch-submit
    ("/:owner/:repo-name/watch" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (if (watching-repo-p (getf repo :id) *current-user-id*)
          (unwatch-repo (getf repo :id) *current-user-id*)
          (watch-repo (getf repo :id) *current-user-id*))
      (hunchentoot:redirect (format nil "/~A/~A" owner repo-name)))))

(easy-routes:defroute fork-repo-submit
    ("/:owner/:repo-name/fork" :method :post) ()
  (when (require-login)
    (let ((source-repo (find-repo owner repo-name)))
      (unless source-repo (return-from fork-repo-submit (not-found)))
      (let* ((username (getf *current-user* :username))
             (existing (find-repo username repo-name)))
        (when existing
          ;; Already forked
          (hunchentoot:redirect (format nil "/~A/~A" username repo-name))
          (return-from fork-repo-submit nil))
        ;; Create the repo record
        (let ((repo (create-repo :owner-id *current-user-id*
                                 :name repo-name
                                 :description (format nil "Fork of ~A/~A" owner repo-name))))
          ;; Clone the bare repo on disk
          (let ((source-path (repo-disk-path owner repo-name))
                (dest-path (repo-disk-path username repo-name)))
            (ensure-directories-exist dest-path)
            (uiop:run-program (list "git" "clone" "--bare"
                                    (namestring source-path)
                                    (namestring dest-path))
                               :output :string :error-output :string)
            ;; Install hooks
            (let ((pre-hook (merge-pathnames "hooks/pre-receive" dest-path)))
              (with-open-file (out pre-hook :direction :output :if-exists :supersede)
                (format out "#!/bin/bash~%exec cave-server run-checks --config /etc/cave.conf --repo ~A/~A~%"
                        username repo-name))
              (uiop:run-program (list "chmod" "+x" (namestring pre-hook))
                                 :ignore-error-status t))
            (let ((post-hook (merge-pathnames "hooks/post-receive" dest-path)))
              (with-open-file (out post-hook :direction :output :if-exists :supersede)
                (format out "#!/bin/bash~%cave-server sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
                        username repo-name)
                (when (string= repo-name "cave-themes")
                  (format out "cave-server sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
                          username)))
              (uiop:run-program (list "chmod" "+x" (namestring post-hook))
                                 :ignore-error-status t))
            ;; Fix ownership
            (uiop:run-program (list "chown" "-R" "cave:cave" (namestring dest-path))
                               :output :string :error-output :string :ignore-error-status t))
          (log-event "repo.forked" :user-id *current-user-id*
                                   :repo-id (getf repo :id)
                                   :metadata (format nil "{\"source\": \"~A/~A\"}" owner repo-name))
          (hunchentoot:redirect (format nil "/~A/~A" username repo-name)))))))

;; Tree (directory) browsing
(defun %valid-git-ref-name-p (name)
  "Permissive git branch/tag name check: 1–255 chars, allowed chars only, no
   leading dash, no '..'. The git command runs argv-style (no shell), so this
   just guards against flag-injection and obviously bad names."
  (and (stringp name) (plusp (length name)) (<= (length name) 255)
       (not (char= (char name 0) #\-))
       (not (search ".." name))
       (every (lambda (c) (or (alphanumericp c) (member c '(#\_ #\- #\. #\/)))) name)))

(easy-routes:defroute tree-page ("/:owner/:repo-name/tree/:ref" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (%valid-git-ref-name-p ref) (return-from tree-page (not-found)))
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (file-tree (chamber-get-tree owner repo-name :ref ref :path path))
           (default-branch (or (chamber-get-default-branch owner repo-name) "main"))
           (branches (chamber-get-branches owner repo-name))
           (tags (chamber-get-tags owner repo-name))
           (last-commits (chamber-tree-last-commits
                          owner repo-name ref path
                          (mapcar (lambda (e) (getf e :name)) file-tree))))
      (html-response
       (view-tree :owner-name owner :repo repo :ref ref
                  :path path :file-tree file-tree
                  :branches branches :tags tags
                  :default-branch default-branch
                  :last-commits last-commits)))))

(easy-routes:defroute create-branch-route ("/:owner/:repo-name/branches" :method :post) ()
  (when (require-login)
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from create-branch-route (not-found)))
      (unless (repo-member-role (getf repo :id) *current-user-id*)
        (setf (hunchentoot:return-code*) 403)
        (return-from create-branch-route "Forbidden"))
      (let ((name (hunchentoot:post-parameter "name"))
            (from (or (hunchentoot:post-parameter "from")
                      (chamber-get-default-branch owner repo-name) "main")))
        (cond
          ((not (%valid-git-ref-name-p name))
           (setf (hunchentoot:return-code*) 400) "Invalid branch name")
          (t
           (multiple-value-bind (ok err)
               (git-create-branch (repo-disk-path owner repo-name) name from)
             (declare (ignore err))
             (cond
               (ok
                (chamber-invalidate-repo owner repo-name)
                (hunchentoot:redirect (format nil "/~A/~A/tree/~A" owner repo-name name))
                nil)
               (t (setf (hunchentoot:return-code*) 400)
                  "Could not create branch (it may already exist)")))))))))

;; Blob (file) viewing
(easy-routes:defroute blob-page ("/:owner/:repo-name/blob/:ref" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (%valid-git-ref-name-p ref) (return-from blob-page (not-found)))
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (info (chamber-get-blob-info owner repo-name ref path)))
      ;; If not found, might be a directory — redirect to tree view
      (unless info
        (let ((tree (chamber-get-tree owner repo-name :ref ref :path path)))
          (if tree
              (progn
                (hunchentoot:redirect
                 (format nil "/~A/~A/tree/~A?path=~A" owner repo-name ref path))
                (return-from blob-page nil))
              (return-from blob-page (not-found)))))
      (let* ((file-size (getf info :size))
             (is-binary (getf info :is-binary))
             (content (when (and (not is-binary) (<= file-size (* 2 1024 1024)))
                        (chamber-get-blob owner repo-name ref path)))
             (language (file-language path))
             (is-markdown (and language (string= language "markdown")))
             ;; Markdown renders to HTML by default; ?view=source shows the
             ;; Monaco source view. Non-markdown files are always source.
             (view-mode (if (and is-markdown
                                  content
                                  (not (member (hunchentoot:get-parameter "view")
                                               '("source" "code" "raw") :test #'equal)))
                            :rendered
                            :source))
             ;; Relative image src in the markdown resolves against the file's
             ;; own directory, so the raw base points there (not the repo root).
             (dir (let ((slash (position #\/ path :from-end t)))
                    (if slash (subseq path 0 (1+ slash)) "")))
             (raw-base-url (format nil "~A/~A/~A/raw/~A?path=~A"
                                   (config-value :base-url "http://localhost:8080")
                                   owner repo-name (or ref "HEAD") dir))
             ;; Reuse the rendered-markdown cache, content-addressed by
             ;; (blob-sha . raw-base-url) exactly as the README path does.
             (rendered-html
               (when (eq view-mode :rendered)
                 (let* ((cache-key (cons (getf info :hash) raw-base-url))
                        (cached (readme-cache-get cache-key)))
                   (or cached
                       (readme-cache-put
                        cache-key
                        (render-markdown content :raw-base-url raw-base-url))))))
             (default-branch (or (chamber-get-default-branch owner repo-name) "main"))
             (branches (chamber-get-branches owner repo-name))
             (tags (chamber-get-tags owner repo-name)))
        (html-response
         (view-blob :owner-name owner :repo repo :ref ref :path path
                    :content content
                    :is-binary is-binary
                    :file-size file-size
                    :language language
                    :is-markdown is-markdown
                    :view-mode view-mode
                    :rendered-html rendered-html
                    :branches branches :tags tags
                    :default-branch default-branch))))))

(defun raw-mime-type (path)
  "Guess MIME type from file extension."
  (let ((ext (string-downcase (or (pathname-type (pathname path)) ""))))
    (cond
      ((member ext '("png") :test #'string=) "image/png")
      ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
      ((member ext '("gif") :test #'string=) "image/gif")
      ((member ext '("svg") :test #'string=) "image/svg+xml")
      ((member ext '("webp") :test #'string=) "image/webp")
      ((member ext '("ico") :test #'string=) "image/x-icon")
      ((member ext '("pdf") :test #'string=) "application/pdf")
      ((member ext '("zip" "gz" "tar" "bz2" "xz") :test #'string=) "application/octet-stream")
      (t "text/plain; charset=utf-8"))))

;; Raw file blob cache — keyed by git object SHA, content-addressable
(defvar *blob-cache* (make-hash-table :test 'equal)
  "In-memory LRU cache for raw file blobs. Key: git SHA, Value: (content . access-time).")
(defvar *blob-cache-lock* (bt2:make-lock :name "blob-cache"))
(defvar *blob-cache-max-bytes* (* 64 1024 1024) "Max cache size in bytes (64MB).")
(defvar *blob-cache-bytes* 0 "Current cache size in bytes.")

;; Rendered-README cache — content-addressable by (sha + raw-base-url). The base-url
;; participates because render-markdown rewrites relative <img src> using it, so the
;; same README on two different deploys must render to two different strings.
(defvar *readme-cache* (make-hash-table :test 'equal))
(defvar *readme-cache-lock* (bt2:make-lock :name "readme-cache"))
(defparameter *readme-cache-max* 256
  "Max entries in *readme-cache*. Beyond this, we evict at random.")

(defun readme-cache-get (key)
  (bt2:with-lock-held (*readme-cache-lock*)
    (gethash key *readme-cache*)))

(defun readme-cache-put (key html)
  (bt2:with-lock-held (*readme-cache-lock*)
    (when (>= (hash-table-count *readme-cache*) *readme-cache-max*)
      ;; Random eviction — no LRU bookkeeping. Cheap enough at this size.
      (let ((victim (loop for k being the hash-keys of *readme-cache* return k)))
        (when victim (remhash victim *readme-cache*))))
    (setf (gethash key *readme-cache*) html))
  html)

(defun blob-cache-get (sha)
  "Get cached blob by SHA. Returns content or NIL."
  (bt2:with-lock-held (*blob-cache-lock*)
    (let ((entry (gethash sha *blob-cache*)))
      (when entry
        (setf (cdr entry) (get-universal-time))
        (car entry)))))

(defun blob-cache-put (sha content)
  "Cache blob content by SHA. Evicts oldest entries if over size limit."
  (let ((size (if (stringp content) (length content)
                  (length content))))
    (when (> size (* 4 1024 1024)) ; don't cache blobs > 4MB
      (return-from blob-cache-put content))
    (bt2:with-lock-held (*blob-cache-lock*)
      ;; Evict oldest entries if needed
      (loop while (> (+ *blob-cache-bytes* size) *blob-cache-max-bytes*)
            do (let ((oldest-key nil) (oldest-time (get-universal-time)))
                 (maphash (lambda (k v)
                            (when (< (cdr v) oldest-time)
                              (setf oldest-key k oldest-time (cdr v))))
                          *blob-cache*)
                 (if oldest-key
                     (let ((old (gethash oldest-key *blob-cache*)))
                       (decf *blob-cache-bytes*
                             (if (stringp (car old)) (length (car old)) (length (car old))))
                       (remhash oldest-key *blob-cache*))
                     (return))))
      (setf (gethash sha *blob-cache*) (cons content (get-universal-time)))
      (incf *blob-cache-bytes* size)))
  content)

;; Raw file content
(easy-routes:defroute raw-page ("/:owner/:repo-name/raw/:ref" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (unless (%valid-git-ref-name-p ref) (return-from raw-page (not-found)))
    (let* ((path (or (hunchentoot:get-parameter "path") ""))
           (mime (raw-mime-type path))
           (info (chamber-get-blob-info owner repo-name ref path)))
      (unless info (return-from raw-page (not-found)))
      (let ((sha (getf info :hash)))
        (setf (hunchentoot:content-type*) mime)
        (setf (hunchentoot:header-out :etag) (format nil "\"~A\"" sha))
        (setf (hunchentoot:header-out :cache-control) "public, max-age=300")
        ;; 304 Not Modified if browser has current version
        (let ((if-none-match (hunchentoot:header-in* :if-none-match)))
          (when (and if-none-match (string= if-none-match (format nil "\"~A\"" sha)))
            (setf (hunchentoot:return-code*) 304)
            (return-from raw-page "")))
        ;; Check server-side blob cache
        (let ((cached (blob-cache-get sha)))
          (when cached
            (if (stringp cached)
                (return-from raw-page cached)
                ;; Binary cached data: write directly to output stream
                (progn
                  (setf (hunchentoot:content-length*) (length cached))
                  (let ((out (hunchentoot:send-headers)))
                    (write-sequence cached out)
                    (finish-output out))
                  (return-from raw-page nil)))))
        ;; Cache miss — read via Chamber
        (if (uiop:string-prefix-p "text/" mime)
            ;; Text: return as string (Hunchentoot handles natively)
            (let ((content (chamber-get-blob owner repo-name ref path)))
              (unless content (return-from raw-page (not-found)))
              (blob-cache-put sha content))
            ;; Binary: read directly via git (bypass gRPC for large binary blobs)
            ;; and write directly to output stream (Hunchentoot pattern)
            (let ((content (git-blob-bytes (repo-disk-path owner repo-name) ref path)))
              (unless content (return-from raw-page (not-found)))
              (blob-cache-put sha content)
              (setf (hunchentoot:content-length*) (length content))
              (let ((out (hunchentoot:send-headers)))
                (write-sequence content out)
                (finish-output out))))))))

;; Commit detail page (also handles .patch and .diff suffixes)
(easy-routes:defroute commit-page ("/:owner/:repo-name/commit/:hash" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((disk-path (repo-disk-path owner repo-name))
           ;; Strip .patch or .diff suffix
           (is-patch (and (> (length hash) 6)
                          (string= ".patch" (subseq hash (- (length hash) 6)))))
           (is-diff (and (> (length hash) 5)
                         (string= ".diff" (subseq hash (- (length hash) 5)))))
           (clean-hash (cond (is-patch (subseq hash 0 (- (length hash) 6)))
                             (is-diff (subseq hash 0 (- (length hash) 5)))
                             (t hash))))
      (unless (%valid-git-ref-name-p clean-hash)
        (return-from commit-page (not-found)))
      (cond
        ;; .patch — git format-patch output
        (is-patch
         (let ((patch (git-format-patch disk-path clean-hash)))
           (unless patch (return-from commit-page (not-found)))
           (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
           patch))
        ;; .diff — raw unified diff
        (is-diff
         (let ((diff (git-commit-diff disk-path clean-hash)))
           (unless diff (return-from commit-page (not-found)))
           (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
           diff))
        ;; HTML commit page
        (t
         (let ((result (chamber-get-commit owner repo-name clean-hash)))
           (unless result (return-from commit-page (not-found)))
           (html-response
            (view-commit :owner-name owner :repo repo
                         :commit (getf result :commit)
                         :signature (find-commit-signature (getf repo :id) clean-hash)
                         :trailers (git-commit-trailers disk-path clean-hash)
                         :diff-raw (getf result :diff)))))))))

(easy-routes:defroute new-org-repo-page ("/o/:org-name/-/new-repo" :method :get) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from new-org-repo-page (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from new-org-repo-page "Forbidden"))
      (html-response (view-new-repo :org org)))))

(easy-routes:defroute create-org-repo-submit ("/o/:org-name/-/new-repo" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from create-org-repo-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from create-org-repo-submit "Forbidden"))
      (let* ((mode (or (hunchentoot:post-parameter "mode") "empty"))
             (name (hunchentoot:post-parameter "name"))
             (description (hunchentoot:post-parameter "description"))
             (is-private (hunchentoot:post-parameter "is_private"))
             (url (hunchentoot:post-parameter "url"))
             (auth-token (hunchentoot:post-parameter "auth_token"))
             (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                      :junk-allowed t))
             (name (if (and (or (string= mode "import") (string= mode "mirror"))
                            (or (null name) (uiop:emptyp name))
                            url (not (uiop:emptyp url)))
                       (repo-name-from-url url)
                       name)))
        (handler-case
            (let ((repo (create-repo :org-id (getf org :id)
                                     :name name
                                     :description description
                                     :is-private (when is-private t))))
              (cond
                ((string= mode "import")
                 (import-repo-from-url org-name name url
                                       :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                     auth-token)))
                ((string= mode "mirror")
                 (import-repo-from-url org-name name url
                                       :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                     auth-token))
                 (create-mirror :repo-id (getf repo :id)
                                :direction "pull"
                                :remote-url url
                                :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                              auth-token)
                                :interval-minutes (or interval 60)))
                (t (init-bare-repo org-name name)))
              (log-event "repo.created" :user-id *current-user-id*
                                        :repo-id (getf repo :id)
                                        :metadata (format nil "{\"mode\": \"~A\"}" mode))
              (hunchentoot:redirect (format nil "/~A/~A" org-name name)))
          (error (e)
            (html-response (view-new-repo :org org :error (format nil "~A" e)))))))))

;; ----------------------------------------------------------------------------
;; Routes: Repo settings

(easy-routes:defroute repo-settings-page
    ("/:owner/:repo-name/settings" :method :get) ()
  (%with-repo-admin (repo owner repo-name repo-settings-page)
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
                         :deploy-keys (list-deploy-keys (getf repo :id))))))

(easy-routes:defroute repo-secret-add-submit
    ("/:owner/:repo-name/settings/secrets" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-secret-add-submit)
    (let ((name (string-trim " " (or (hunchentoot:post-parameter "name") "")))
          (value (or (hunchentoot:post-parameter "value") "")))
      (when (and (plusp (length name)) (plusp (length value)))
        (set-secret "repo" (getf repo :id) name value)))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-secret-delete-submit
    ("/:owner/:repo-name/settings/secrets/:name/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-secret-delete-submit)
    (delete-secret "repo" (getf repo :id) name)
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-protect-add-submit
    ("/:owner/:repo-name/settings/protect" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-protect-add-submit)
    (let ((pattern (string-trim " " (or (hunchentoot:post-parameter "pattern") ""))))
      (when (plusp (length pattern))
        (add-protected-branch
         (getf repo :id) pattern
         :block-direct-push (equal (hunchentoot:post-parameter "block_direct_push") "1")
         :require-signed-commits (equal (hunchentoot:post-parameter "require_signed_commits") "1"))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-protect-delete-submit
    ("/:owner/:repo-name/settings/protect/:id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-protect-delete-submit)
    (let ((pid (parse-integer id :junk-allowed t)))
      (when pid (delete-protected-branch pid (getf repo :id))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-deploy-key-add-submit
    ("/:owner/:repo-name/settings/deploy-keys" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-deploy-key-add-submit)
    (let ((name (string-trim " " (or (hunchentoot:post-parameter "name") "")))
          (key (string-trim '(#\Newline #\Return #\Space)
                            (or (hunchentoot:post-parameter "public_key") ""))))
      (when (and (plusp (length name)) (plusp (length key)))
        (handler-case
            (progn
              (add-deploy-key (getf repo :id) name key
                              :read-write (equal (hunchentoot:post-parameter "read_write") "1"))
              (sync-authorized-keys))
          (error () nil))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-deploy-key-delete-submit
    ("/:owner/:repo-name/settings/deploy-keys/:id/delete" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-deploy-key-delete-submit)
    (let ((kid (parse-integer id :junk-allowed t)))
      (when kid (delete-deploy-key kid (getf repo :id)) (sync-authorized-keys)))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-settings-submit
    ("/:owner/:repo-name/settings" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-settings-submit)
    (let ((section (hunchentoot:post-parameter "section")))
      (when (equal section "merge")
        (update-repo-settings (getf repo :id)
          :required-approvals (or (parse-integer
                                   (or (hunchentoot:post-parameter "required_approvals") "1")
                                   :junk-allowed t) 1)
          :allow-self-approval (when (hunchentoot:post-parameter "allow_self_approval") t)
          :allow-stale-approvals (when (hunchentoot:post-parameter "allow_stale_approvals") t)
          :concerns-count-as-approval (when (hunchentoot:post-parameter "concerns_count") t)
          :block-on-request-changes (when (hunchentoot:post-parameter "block_on_request_changes") t)
          :auto-delete-branch (when (hunchentoot:post-parameter "auto_delete_branch") t))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-add-member-submit
    ("/:owner/:repo-name/settings/members" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-add-member-submit)
    (let* ((username (hunchentoot:post-parameter "username"))
           (role (or (hunchentoot:post-parameter "role") "writer"))
           (user (find-user-by-username username)))
      (when user
        (handler-case
            (add-repo-member (getf repo :id) (getf user :id) :role role)
          (error () nil))))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-remove-member-submit
    ("/:owner/:repo-name/settings/members/:user-id/remove" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-remove-member-submit)
    (let ((uid (parse-integer user-id :junk-allowed t)))
      (when uid (remove-repo-member (getf repo :id) uid)))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-archive-submit
    ("/:owner/:repo-name/settings/archive" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-archive-submit)
    (archive-repo (getf repo :id) :archived t)
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-unarchive-submit
    ("/:owner/:repo-name/settings/unarchive" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-unarchive-submit)
    (archive-repo (getf repo :id) :archived nil)
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-visibility-submit
    ("/:owner/:repo-name/settings/visibility" :method :post) ()
  (%with-repo-admin (repo owner repo-name repo-visibility-submit)
    (let* ((make-private (when (hunchentoot:post-parameter "private") t))
           (was-private (getf repo :is-private)))
      (set-repo-visibility (getf repo :id) :private make-private)
      ;; Going private->public: re-index so the repo becomes searchable
      ;; immediately (search visibility is enforced at query time, so no
      ;; de-index is needed when going public->private).
      (when (and was-private (not make-private))
        (zoekt-index-repo owner repo-name)))
    (hunchentoot:redirect (format nil "/~A/~A/settings" owner repo-name))))

(easy-routes:defroute repo-delete-submit
    ("/:owner/:repo-name/settings/delete" :method :post) ()
  (when (require-sudo (format nil "/~A/~A/settings" owner repo-name))
    (let ((repo (find-repo owner repo-name)))
      (unless repo (return-from repo-delete-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from repo-delete-submit "Forbidden"))
      ;; Delete disk files
      (let ((disk-path (repo-disk-path owner repo-name)))
        (when (probe-file disk-path)
          (uiop:delete-directory-tree disk-path :validate t :if-does-not-exist :ignore)))
      ;; Delete from DB (cascades to issues, PRs, etc.)
      (delete-repo (getf repo :id))
      (hunchentoot:redirect (format nil "/~A" owner)))))

;; Automation runs page
(easy-routes:defroute runs-page ("/:owner/:repo-name/runs" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (html-response
     (view-runs :owner-name owner :repo repo
                :runs (list-automation-runs (getf repo :id))
                :workflow-runs (list-workflow-runs (getf repo :id))))))

(easy-routes:defroute pulse-page ("/:owner/:repo-name/pulse" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    ;; Pulse is owner/member-only — it exposes referrers, visitor counts,
    ;; and per-contributor activity that the public doesn't need to see.
    (unless (and *current-user-id*
                 (repo-member-role (getf repo :id) *current-user-id*))
      (return-from pulse-page (not-found)))
    (let ((repo-id (getf repo :id))
          (days 14))
      (html-response
       (view-pulse :owner-name owner :repo repo
                   :days days
                   :event-counts (repo-event-counts-by-day repo-id :days days)
                   :contributors (repo-top-contributors repo-id :days days :limit 5)
                   :views (repo-page-views-by-day repo-id :days days)
                   :unique-visitors (repo-unique-visitors-by-day repo-id :days days)
                   :referrers (repo-top-referrers repo-id :days days :limit 10))))))

