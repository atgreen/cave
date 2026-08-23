(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Repos

(easy-routes:defroute api-create-personal-repo
    ("/api/v1/user/repos" :method :post) ()
  "Create a personal repo for the authenticated user. Mirrors the HTML
   /-/new-repo flow: mode is one of 'empty' (default), 'import' (one-shot
   clone of remote URL), or 'mirror' (import + ongoing pull mirror)."
  (unless *current-user-id*
    (return-from api-create-personal-repo (json-error "unauthorized" :status 401)))
  (let* ((body-text (hunchentoot:raw-post-data :force-text t))
         (json (com.inuoe.jzon:parse (or body-text "{}")))
         (raw-name (gethash "name" json))
         (description (gethash "description" json))
         (is-private (gethash "private" json))
         (mode (or (gethash "mode" json) "empty"))
         (url (gethash "url" json))
         (auth-token (gethash "auth_token" json))
         (interval (or (gethash "mirror_interval_minutes" json) 60))
         (username (getf *current-user* :username))
         ;; Auto-derive name from URL when importing/mirroring without one.
         (name (cond ((and raw-name (not (eq raw-name 'null)) (not (uiop:emptyp raw-name)))
                      raw-name)
                     ((and (member mode '("import" "mirror") :test #'equal)
                           url (not (eq url 'null)) (not (uiop:emptyp url)))
                      (repo-name-from-url url))
                     (t nil))))
    (unless (and name (not (uiop:emptyp name)))
      (return-from api-create-personal-repo (json-error "name required")))
    (unless (member mode '("empty" "import" "mirror") :test #'equal)
      (return-from api-create-personal-repo
        (json-error "mode must be empty, import, or mirror")))
    (when (and (member mode '("import" "mirror") :test #'equal)
               (or (null url) (eq url 'null) (uiop:emptyp url)))
      (return-from api-create-personal-repo
        (json-error "url required for import/mirror mode")))
    (handler-case
        (let ((repo (create-repo :owner-id *current-user-id*
                                 :name name
                                 :description (unless (eq description 'null) description)
                                 :is-private (and is-private (not (eq is-private 'null))))))
          (cond
            ((equal mode "import")
             (import-repo-from-url username name url
                                   :auth-token (unless (or (null auth-token)
                                                            (eq auth-token 'null)
                                                            (uiop:emptyp auth-token))
                                                 auth-token)))
            ((equal mode "mirror")
             (import-repo-from-url username name url
                                   :auth-token (unless (or (null auth-token)
                                                            (eq auth-token 'null)
                                                            (uiop:emptyp auth-token))
                                                 auth-token))
             (create-mirror :repo-id (getf repo :id)
                            :direction "pull"
                            :remote-url url
                            :auth-token (unless (or (null auth-token)
                                                     (eq auth-token 'null)
                                                     (uiop:emptyp auth-token))
                                          auth-token)
                            :interval-minutes interval))
            (t (init-bare-repo username name)))
          (log-event "repo.created" :user-id *current-user-id*
                                    :repo-id (getf repo :id)
                                    :metadata (format nil "{\"mode\": \"~A\"}" mode))
          ;; Include owner_name so API clients (e.g. the cave CLI) can print
          ;; OWNER/REPO without a second lookup.
          (let ((created (find-repo username name)))
            (json-response (append created (list :owner-name username))
                           :status 201)))
      (error (e)
        (json-error (format nil "~A" e) :status 400)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Issues

(easy-routes:defroute api-list-issues
    ("/api/v1/repos/:owner/:repo-name/issues" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (issues (list-issues (getf repo :id) :status status)))
      (json-response issues))))

(easy-routes:defroute api-create-issue
    ("/api/v1/repos/:owner/:repo-name/issues" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-issue (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (title (gethash "title" json))
           (body (gethash "body" json)))
      (unless title (return-from api-create-issue (json-error "title required")))
      (let ((issue (create-issue :repo-id (getf repo :id)
                                 :author-id *current-user-id*
                                 :title title
                                 :body (if (eq body 'null) nil body))))
        ;; Parity with the web route: notify owner/members/watchers.
        (ignore-errors (notify-issue-created repo owner repo-name issue))
        (json-response issue :status 201)))))

(easy-routes:defroute api-get-issue
    ("/api/v1/repos/:owner/:repo-name/issues/:id" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-get-issue (json-error "not found" :status 404)))
      ;; gh-style detail: embed the author login and the full comment thread so
      ;; `cave issue view` can render everything in one request. Existing issue
      ;; fields are preserved; `author` and `comments` are additive.
      (let ((obj (plist-to-hash-table issue))
            (author (find-user-by-id (getf issue :author-id))))
        (setf (gethash "author" obj) (or (getf author :username) :null))
        (setf (gethash "comments" obj)
              (map 'vector
                   (lambda (c)
                     (let ((h (make-hash-table :test 'equal)))
                       (setf (gethash "id" h) (getf c :id))
                       (setf (gethash "author" h) (or (getf c :username) :null))
                       (setf (gethash "body" h) (getf c :body))
                       (setf (gethash "created_at" h)
                             (princ-to-string (getf c :created-at)))
                       h))
                   (list-issue-comments (getf issue :id))))
        (json-response obj)))))

(easy-routes:defroute api-update-issue
    ("/api/v1/repos/:owner/:repo-name/issues/:id" :method :patch) ()
  (unless *current-user-id*
    (return-from api-update-issue (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-update-issue (json-error "not found" :status 404)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             (status (gethash "status" json)))
        (when status
          (unless (member status '("open" "closed") :test #'equal)
            (return-from api-update-issue (json-error "status must be open or closed")))
          (update-issue (getf issue :id) :status status))
        (json-response (find-issue (getf repo :id) num))))))

(easy-routes:defroute api-create-issue-comment
    ("/api/v1/repos/:owner/:repo-name/issues/:id/comments" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-issue-comment (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer id :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from api-create-issue-comment (json-error "not found" :status 404)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             (body (gethash "body" json)))
        (when (or (not body) (eq body 'null) (uiop:emptyp body))
          (return-from api-create-issue-comment (json-error "body required")))
        (let ((comment (create-issue-comment :issue-id (getf issue :id)
                                             :author-id *current-user-id*
                                             :body body)))
          (notify-issue-comment repo owner repo-name issue body)
          (fire-webhooks (getf repo :id) "issue"
                         (make-webhook-payload "issue.comment"
                                               :owner owner
                                               :repo repo-name
                                               :number (getf issue :number)))
          (json-response comment :status 201))))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Pull Requests

(defparameter +api-docs-html+
  "<!doctype html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <title>Cave API reference</title>
  </head>
  <body>
    <script id=\"api-reference\" data-url=\"/api/v1/openapi.json\"></script>
    <script src=\"https://cdn.jsdelivr.net/npm/@scalar/api-reference\"></script>
  </body>
</html>"
  "Standalone Scalar API-reference page rendering the OpenAPI spec.")

(easy-routes:defroute api-openapi-json ("/api/v1/openapi.json" :method :get) ()
  "Serve the OpenAPI 3.1 description of the v1 API (for SDK generators / docs)."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (uiop:read-file-string (merge-pathnames "static/openapi.json" (app-root)))
    (error ()
      (setf (hunchentoot:return-code*) 404)
      "{\"error\":\"openapi spec not found\"}")))

(easy-routes:defroute api-docs ("/api/v1/docs" :method :get) ()
  "Interactive API reference (Scalar) for the v1 API."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  +api-docs-html+)

(easy-routes:defroute api-list-pulls
    ("/api/v1/repos/:owner/:repo-name/pulls" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (pulls (list-pull-requests (getf repo :id) :status status)))
      (json-response pulls))))

(easy-routes:defroute api-get-pull
    ("/api/v1/repos/:owner/:repo-name/pulls/:number" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-get-pull (json-error "not found" :status 404)))
      (json-response pr))))

(easy-routes:defroute api-update-pull
    ("/api/v1/repos/:owner/:repo-name/pulls/:number" :method :patch) ()
  (unless *current-user-id*
    (return-from api-update-pull (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-update-pull (json-error "not found" :status 404)))
      (unless (or (eql (getf pr :author-id) *current-user-id*)
                  (member-of-repo-p repo))
        (return-from api-update-pull (json-error "forbidden" :status 403)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             ;; GitHub uses "state" for PRs; accept "status" as an alias.
             (state (or (gethash "state" json) (gethash "status" json))))
        (when state
          (unless (member state '("open" "closed") :test #'equal)
            (return-from api-update-pull (json-error "state must be open or closed")))
          (cond
            ((and (equal state "closed") (not (getf pr :is-merged)))
             (close-pull-request (getf pr :id)))
            ((and (equal state "open") (not (getf pr :is-merged)) (getf pr :is-closed))
             (reopen-pull-request (getf pr :id)))))
        (json-response (find-pull-request (getf repo :id) num))))))

(easy-routes:defroute api-create-pull
    ("/api/v1/repos/:owner/:repo-name/pulls" :method :post) ()
  (unless *current-user-id*
    (return-from api-create-pull (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (source (gethash "source_branch" json))
           (target (gethash "target_branch" json)))
      (unless (and source target)
        (return-from api-create-pull (json-error "source_branch and target_branch required")))
      (let* ((disk-path (repo-disk-path owner repo-name))
             (head-commit (handler-case
                              (string-trim '(#\Newline #\Space)
                                           (uiop:run-program
                                            (list "git" "-C" (namestring disk-path)
                                                  "rev-parse" source)
                                            :output :string))
                            (error () nil)))
             (pr (create-pull-request :repo-id (getf repo :id)
                                      :author-id *current-user-id*
                                      :source-branch source
                                      :target-branch target
                                      :head-commit head-commit)))
        ;; Parity with the web route: notify owner/members/watchers.
        (ignore-errors (notify-pr-opened repo owner repo-name pr))
        (json-response pr :status 201)))))

(easy-routes:defroute api-list-reviews
    ("/api/v1/repos/:owner/:repo-name/pulls/:number/reviews" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-list-reviews (json-error "not found" :status 404)))
      (json-response (list-reviews (getf pr :id))))))

(easy-routes:defroute api-submit-review
    ("/api/v1/repos/:owner/:repo-name/pulls/:number/reviews" :method :post) ()
  (unless *current-user-id*
    (return-from api-submit-review (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-submit-review (json-error "not found" :status 404)))
      (unless (repo-reviewer-p (getf repo :id) *current-user-id*)
        (return-from api-submit-review (json-error "forbidden" :status 403)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (com.inuoe.jzon:parse body-text))
             (state (gethash "state" json))
             (body (gethash "body" json)))
        (unless (member state '("approve" "approve_with_concerns" "request_changes" "comment")
                        :test #'equal)
          (return-from api-submit-review (json-error "invalid state")))
        (let ((review (create-review :changeset-id (getf pr :id)
                                     :reviewer-id *current-user-id*
                                     :state state
                                     :body (if (eq body 'null) nil body)
                                     :changeset-version (getf pr :version))))
          (try-auto-merge owner repo-name (getf pr :id))
          (json-response review :status 201))))))

(easy-routes:defroute api-merge-pull
    ("/api/v1/repos/:owner/:repo-name/pulls/:number/merge" :method :post) ()
  "Merge an open, mergeable pull request. Admin-only (mirrors the web merge
route). Optional JSON body: {\"strategy\": \"merge|squash|fast-forward-only\",
\"override\": true} — OVERRIDE bypasses the eligibility gate and is audit-logged.
Returns the merged PR on success."
  (unless *current-user-id*
    (return-from api-merge-pull (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from api-merge-pull (json-error "not found" :status 404)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (return-from api-merge-pull (json-error "forbidden" :status 403)))
      (when (or (getf pr :is-merged) (getf pr :is-closed))
        (return-from api-merge-pull
          (json-error "pull request is already merged or closed" :status 409)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (when (and body-text (plusp (length body-text)))
                     (ignore-errors (com.inuoe.jzon:parse body-text))))
             (strategy (and (hash-table-p json) (gethash "strategy" json)))
             (override (and (hash-table-p json)
                            (let ((o (gethash "override" json)))
                              (and o (not (eq o 'null))))))
             (eligibility (compute-merge-eligibility pr repo)))
        (unless (or (pull-request-mergeable-p eligibility) override)
          (return-from api-merge-pull (json-error "pull request is not mergeable" :status 422)))
        (when override
          (log-event "pr.merge_override" :user-id *current-user-id* :repo-id (getf repo :id)
                     :entity-type "pull_request" :entity-id (getf pr :id)
                     :metadata "API admin override"))
        (multiple-value-bind (ok msg)
            (perform-pr-merge owner repo-name pr repo
                              (and (stringp strategy) strategy)
                              *current-user-id*)
          (unless ok (return-from api-merge-pull (json-error msg :status 409)))
          (json-response (find-pull-request (getf repo :id) num)))))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Commit Statuses

(easy-routes:defroute api-list-commit-statuses
    ("/api/v1/repos/:owner/:repo-name/statuses/:sha" :method :get) ()
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (json-response (list-commit-statuses (getf repo :id) sha))))

(easy-routes:defroute api-set-commit-status
    ("/api/v1/repos/:owner/:repo-name/statuses/:sha" :method :post) ()
  (unless *current-user-id*
    (return-from api-set-commit-status (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (unless (repo-member-role (getf repo :id) *current-user-id*)
      (return-from api-set-commit-status (json-error "forbidden" :status 403)))
    (let* ((body-text (hunchentoot:raw-post-data :force-text t))
           (json (com.inuoe.jzon:parse body-text))
           (state (gethash "state" json))
           (context (gethash "context" json))
           (description (gethash "description" json))
           (target-url (gethash "target_url" json)))
      (unless (member state '("pending" "success" "failure" "error") :test #'equal)
        (return-from api-set-commit-status (json-error "invalid state")))
      (let ((status (set-commit-status
                     :repo-id (getf repo :id)
                     :commit-sha sha
                     :state state
                     :context (or context "default")
                     :description (when (and description (not (eq description 'null))) description)
                     :target-url (when (and target-url (not (eq target-url 'null))) target-url))))
        ;; A passing check may complete an auto-merge-armed PR at this head.
        (dolist (p (pull-requests-armed-for-head (getf repo :id) sha))
          (try-auto-merge owner repo-name (getf p :id)))
        (json-response status :status 201)))))

;; ----------------------------------------------------------------------------
;; Routes: API v1 — Dependencies & security alerts

(easy-routes:defroute api-list-deps
    ("/api/v1/repos/:owner/:repo-name/deps" :method :get) (&get ref)
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (json-response (list-repo-deps (getf repo :id) :ref ref))))

(easy-routes:defroute api-list-alerts
    ("/api/v1/repos/:owner/:repo-name/alerts" :method :get) (&get state)
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (json-response (list-dep-alerts-detailed (getf repo :id)
                                             :state (or state "open")))))

(easy-routes:defroute api-dismiss-alert
    ("/api/v1/repos/:owner/:repo-name/alerts/:id/dismiss" :method :post) ()
  (unless *current-user-id*
    (return-from api-dismiss-alert (json-error "unauthorized" :status 401)))
  (with-visible-repo (repo owner repo-name (lambda () (json-error "not found" :status 404)))
    (unless (repo-member-role (getf repo :id) *current-user-id*)
      (return-from api-dismiss-alert (json-error "forbidden" :status 403)))
    (let* ((alert-id (parse-integer id :junk-allowed t))
           (alert (and alert-id (find-dep-alert-detailed alert-id))))
      (unless (and alert (eql (getf alert :repo-id) (getf repo :id)))
        (return-from api-dismiss-alert (json-error "not found" :status 404)))
      (let* ((body-text (hunchentoot:raw-post-data :force-text t))
             (json (when (and body-text (plusp (length body-text)))
                     (com.inuoe.jzon:parse body-text)))
             (reason (or (and json (let ((r (gethash "reason" json)))
                                     (when (stringp r) r)))
                         "risk_accepted"))
             (note (and json (let ((n (gethash "note" json)))
                               (when (stringp n) n)))))
        (unless (member reason '("not_used" "no_fix" "risk_accepted") :test #'equal)
          (return-from api-dismiss-alert (json-error "invalid reason")))
        (create-dep-suppression :repo-id (getf repo :id)
                                :ecosystem (getf alert :ecosystem)
                                :package-name (getf alert :package-name)
                                :advisory-id (getf alert :advisory-id)
                                :reason reason :note note
                                :created-by *current-user-id*)
        (set-dep-alert-state alert-id "dismissed")
        (handler-case (update-dependency-dashboard (getf repo :id)) (error () nil))
        (json-response (find-dep-alert-detailed alert-id))))))

(easy-routes:defroute api-deps-usage
    ("/api/v1/deps/usage" :method :get) (&get ecosystem package)
  "Org-wide: repos using a package, filtered to repos visible to the caller."
  (unless (and ecosystem package)
    (return-from api-deps-usage (json-error "ecosystem and package required")))
  (let ((rows (remove-if-not
               (lambda (row)
                 (let ((repo (find-repo-by-id (getf row :repo-id))))
                   (and repo (repo-visible-p repo))))
               (find-repos-using-package ecosystem package))))
    (json-response rows)))

;; ----------------------------------------------------------------------------
;; Git smart HTTP transport (read-only, for runner clones)

(defun handle-git-http (owner repo-name)
  "Handle git smart HTTP for a repo. Dispatches based on URL suffix."
  (let* ((repo (find-repo owner repo-name))
         (disk-path (when repo (repo-disk-path owner repo-name))))
    (unless (and repo (probe-file disk-path))
      (setf (hunchentoot:return-code*) 404)
      (return-from handle-git-http "Not found"))
    (unless (repo-visible-p repo)
      (setf (hunchentoot:return-code*) 404)
      (return-from handle-git-http "Not found"))
    (let ((uri (hunchentoot:script-name*)))
      (cond
        ;; GET /owner/repo.git/info/refs?service=git-upload-pack
        ((and (search "/info/refs" uri)
              (equal (hunchentoot:request-method*) :get))
         (unless (equal (hunchentoot:get-parameter "service") "git-upload-pack")
           (setf (hunchentoot:return-code*) 403)
           (return-from handle-git-http "Forbidden"))
         (multiple-value-bind (output _err exit-code)
             (uiop:run-program (sandbox-wrap disk-path
                                (list "git" "upload-pack" "--stateless-rpc"
                                     "--advertise-refs" (namestring disk-path)))
                               :output :string
                               :error-output :string
                               :ignore-error-status t)
           (declare (ignore _err))
           (unless (zerop exit-code)
             (setf (hunchentoot:return-code*) 500)
             (return-from handle-git-http "Internal error"))
           (setf (hunchentoot:content-type*)
                 "application/x-git-upload-pack-advertisement")
           (concatenate 'string
                     "001e# service=git-upload-pack" (string #\Newline)
                     "0000" output)))
        ;; POST /owner/repo.git/git-upload-pack
        ((and (search "/git-upload-pack" uri)
              (equal (hunchentoot:request-method*) :post))
         ;; Counted as a clone for Pulse stats. Anon HTTP clones have no user-id.
         (log-event "git.clone" :user-id *current-user-id* :repo-id (getf repo :id))
         (let* ((request-body (hunchentoot:raw-post-data :force-binary t))
                (in-path (format nil "/tmp/cave-git-in-~A"
                                 (ironclad:byte-array-to-hex-string (ironclad:random-data 8))))
                (out-path (format nil "/tmp/cave-git-out-~A"
                                  (ironclad:byte-array-to-hex-string (ironclad:random-data 8)))))
           (unwind-protect
            (progn
              (with-open-file (s in-path :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :supersede)
                (write-sequence request-body s))
              (let ((exit-code (nth-value 2
                                (uiop:run-program
                                 (sandbox-wrap disk-path
                                  (list "git" "upload-pack" "--stateless-rpc"
                                       (namestring disk-path)))
                                 :input in-path
                                 :output out-path
                                 :error-output :string
                                 :ignore-error-status t))))
                (unless (zerop exit-code)
                  (setf (hunchentoot:return-code*) 500)
                  (return-from handle-git-http "Internal error"))
                (setf (hunchentoot:content-type*)
                      "application/x-git-upload-pack-result")
                (with-open-file (s out-path :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence buf s)
                    buf))))
            (ignore-errors (delete-file in-path))
            (ignore-errors (delete-file out-path)))))
        (t
         (setf (hunchentoot:return-code*) 404)
         "Not found")))))

;; ----------------------------------------------------------------------------
;; Git helpers

(defun repo-disk-path (owner repo-name)
  "Return the on-disk path for a bare git repo.
   OWNER and REPO-NAME are validated as single path components so a crafted
   name (e.g. containing '..' or '/') can never escape the repo storage root."
  (ensure-valid-resource-name owner)
  (ensure-valid-resource-name repo-name)
  (merge-pathnames (format nil "~A/~A.git/" owner repo-name) (repos-dir)))

(defun install-repo-hooks (path owner repo-name)
  "Install pre-receive and post-receive hooks on a bare repo."
  ;; Pre-receive hook (checks)
  (let ((hook-path (merge-pathnames "hooks/pre-receive" path)))
    (ensure-directories-exist hook-path)
    (with-open-file (out hook-path :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%exec cave-server run-checks --config /etc/cave.conf --repo ~A/~A~%"
              owner repo-name))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Post-receive hook (calls back into running Cave server)
  (let ((hook-path (merge-pathnames "hooks/post-receive" path)))
    (with-open-file (out hook-path :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%# Forward ref updates to Cave; relay push-time hints (and -o push options) to the pusher.~%opts=\"\"~%if [ -n \"$GIT_PUSH_OPTION_COUNT\" ]; then i=0; while [ \"$i\" -lt \"$GIT_PUSH_OPTION_COUNT\" ]; do eval \"v=\\$GIT_PUSH_OPTION_$i\"; opts=\"$opts${opts:+,}$v\"; i=$((i+1)); done; fi~%hint=$(curl -sf -X POST --data-binary @- -H \"X-Cave-Push-Options: $opts\" \"http://localhost:~A/-/internal/hook/post-receive/~A/~A?actor=${CAVE_PUSH_USER_ID:-}\" 2>/dev/null)~%[ -n \"$hint\" ] && echo \"$hint\" >&2~%"
              (config-value :http-port 8080) owner repo-name)
      (format out "cave-server sync-mirrors --config /etc/cave.conf --repo ~A/~A &~%"
              owner repo-name)
      (when (string= repo-name "cave-themes")
        (format out "cave-server sync-themes --config /etc/cave.conf --repo ~A/cave-themes &~%"
                owner)))
    (uiop:run-program (list "chmod" "+x" (namestring hook-path))
                       :ignore-error-status t))
  ;; Ensure the cave user owns the repo
  (uiop:run-program (list "chown" "-R" "cave:cave" (namestring path))
                     :output :string :error-output :string :ignore-error-status t))

(defun reinstall-all-hooks ()
  "Rewrite pre-receive/post-receive on every existing repo. Idempotent.
   Called from server startup so the hook script always matches the current binary."
  (let ((count 0)
        (failed 0))
    (dolist (repo (list-all-repos))
      (let* ((owner (getf repo :owner-name))
             (name (getf repo :name))
             (path (repo-disk-path owner name)))
        (cond
          ((not (probe-file path))
           (llog:warn "Skipping hook reinstall: repo missing on disk"
                      :owner owner :repo name))
          (t
           (handler-case
               (progn (install-repo-hooks path owner name) (incf count))
             (error (e)
               (incf failed)
               (llog:warn "Hook reinstall failed"
                          :owner owner :repo name :error (princ-to-string e))))))))
    (llog:info "Reinstalled repo hooks" :ok count :failed failed)))

(defun init-bare-repo (owner repo-name)
  "Initialize a bare git repository on disk with HEAD pointing to main.
   Sets ownership to cave:cave so SSH pushes work."
  (let ((path (repo-disk-path owner repo-name)))
    (ensure-directories-exist path)
    (uiop:run-program (list "git" "init" "--bare" "-b" "main" (namestring path))
                       :output :string :error-output :string)
    (install-repo-hooks path owner repo-name)
    (llog:info "Initialized bare repo" :path path)
    path))

(defun import-repo-from-url (owner repo-name url &key auth-token)
  "Clone a repo from an external URL as a bare repo, install hooks."
  (multiple-value-bind (success-p err)
      (chamber-clone-from-url owner repo-name url :auth-token auth-token)
    (unless success-p
      (error "Clone failed: ~A" err))
    (let ((path (repo-disk-path owner repo-name)))
      (install-repo-hooks path owner repo-name))
    (zoekt-index-repo owner repo-name)
    (llog:info "Imported repo from URL" :owner owner :repo repo-name :url url)))

;; ----------------------------------------------------------------------------
;; Server start

(defun start-server (port)
  "Start the web application."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* nil)
  (setf hunchentoot:*show-lisp-backtraces-p* nil)
  ;; Disable hunchentoot's built-in session handling — we manage our own sessions
  (setf hunchentoot:*session-max-time* 0)
  (setf hunchentoot:*use-user-agent-for-sessions* nil)
  (setf hunchentoot:*rewrite-for-session-urls* nil)
  ;; Suppress the "No session" log noise
  (setf hunchentoot:*log-lisp-warnings-p* nil)
  (init-static-dispatch)
  (setf hunchentoot:*dispatch-table* *static-dispatch-table*)
  (setf *acceptor* (make-instance 'cave-acceptor :port port))
  (handler-case (hunchentoot:start *acceptor*)
    (usocket:address-in-use-error ()
      (format *error-output* "~&Port ~A is already in use.~%" port)
      (uiop:quit 1)))
  (llog:info "HTTP server started" :port port)
  *acceptor*)
