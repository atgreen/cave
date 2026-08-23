(in-package #:cave)

;; Routes: Issues

(easy-routes:defroute issues-page ("/:owner/:repo-name/issues" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (label-filter (hunchentoot:get-parameter "label"))
           (issues (list-issues (getf repo :id) :status status))
           (labels-by-issue (let ((h (make-hash-table)))
                              (dolist (i issues)
                                (setf (gethash (getf i :id) h) (issue-labels (getf i :id))))
                              h))
           (issues (if (and label-filter (plusp (length label-filter)))
                       (remove-if-not
                        (lambda (i) (member label-filter (gethash (getf i :id) labels-by-issue)
                                            :test #'equal))
                        issues)
                       issues)))
      (let ((comment-counts (issue-comment-counts
                             (mapcar (lambda (i) (getf i :id)) issues)))
            (authors (let ((h (make-hash-table)))
                       (dolist (uid (remove-duplicates
                                     (mapcar (lambda (i) (getf i :author-id)) issues)))
                         (when (and uid (not (eq uid :null)))
                           (let ((u (find-user-by-id uid)))
                             (when u (setf (gethash uid h) (getf u :username))))))
                       h)))
        (html-response
         (view-issues :owner-name owner :repo repo :issues issues :current-status status
                      :labels-by-issue labels-by-issue
                      :current-label label-filter
                      :comment-counts comment-counts
                      :authors authors
                      :all-labels (labels-in-repo (getf repo :id))))))))

(easy-routes:defroute deps-page ("/:owner/:repo-name/deps" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (html-response
     (view-dependencies :owner-name owner :repo repo
                        :alerts (list-dep-alerts-detailed (getf repo :id) :state "open")
                        :deps (list-repo-deps (getf repo :id))))))

(easy-routes:defroute milestones-page ("/:owner/:repo-name/milestones" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((milestones (list-milestones (getf repo :id) :state nil))
           (counts (let ((h (make-hash-table)))
                     (dolist (m milestones)
                       (multiple-value-bind (open closed)
                           (milestone-issue-counts (getf m :id))
                         (setf (gethash (getf m :id) h) (cons open closed))))
                     h)))
      (html-response
       (view-milestones :owner-name owner :repo repo :milestones milestones
                        :counts counts :can-edit (and (member-of-repo-p repo) t))))))

(easy-routes:defroute create-milestone-submit
    ("/:owner/:repo-name/milestones" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (unless (member-of-repo-p repo)
        (setf (hunchentoot:return-code*) 403)
        (return-from create-milestone-submit "Forbidden"))
      (let ((title (string-trim " " (or (hunchentoot:post-parameter "title") ""))))
        (when (plusp (length title))
          (create-milestone :repo-id (getf repo :id) :title title
                            :description (hunchentoot:post-parameter "description"))))
      (hunchentoot:redirect (format nil "/~A/~A/milestones" owner repo-name)))))

(easy-routes:defroute milestone-close-submit
    ("/:owner/:repo-name/milestones/:id/close" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (unless (member-of-repo-p repo)
        (setf (hunchentoot:return-code*) 403)
        (return-from milestone-close-submit "Forbidden"))
      (let ((mid (parse-integer id :junk-allowed t)))
        (when mid (update-milestone mid :state "closed")))
      (hunchentoot:redirect (format nil "/~A/~A/milestones" owner repo-name)))))

(easy-routes:defroute milestone-delete-submit
    ("/:owner/:repo-name/milestones/:id/delete" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (unless (member-of-repo-p repo)
        (setf (hunchentoot:return-code*) 403)
        (return-from milestone-delete-submit "Forbidden"))
      (let ((mid (parse-integer id :junk-allowed t)))
        (when mid (delete-milestone mid)))
      (hunchentoot:redirect (format nil "/~A/~A/milestones" owner repo-name)))))

(easy-routes:defroute new-issue-page
    ("/:owner/:repo-name/issues/new" :method :get) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      ;; Allow ?body=… so the blob-view line menu can pre-fill a permalink
      ;; reference; otherwise fall back to the repo's issue template if present.
      (html-response (view-new-issue :owner-name owner :repo repo
                                     :body (or (hunchentoot:get-parameter "body")
                                               (issue-template owner repo-name)))))))

(easy-routes:defroute create-issue-submit
    ("/:owner/:repo-name/issues/new" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (let ((issue (create-issue :repo-id (getf repo :id)
                                 :author-id *current-user-id*
                                 :title (hunchentoot:post-parameter "title")
                                 :body (hunchentoot:post-parameter "body"))))
        (log-event "issue.created" :user-id *current-user-id*
                                   :repo-id (getf repo :id)
                                   :entity-type "issue"
                                   :entity-id (getf issue :id))
        (notify-issue-created repo owner repo-name issue)
        (fire-webhooks (getf repo :id) "issue" (make-webhook-payload "issue.created" :owner owner :repo repo-name :number (getf issue :number) :title (getf issue :title)))
        (hunchentoot:redirect
         (format nil "/~A/~A/issues/~A" owner repo-name (getf issue :number)))))))

(easy-routes:defroute issue-page
    ("/:owner/:repo-name/issues/:number" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((num (parse-integer number :junk-allowed t))
           (issue (when num (find-issue (getf repo :id) num))))
      (unless issue (return-from issue-page (not-found)))
      (let* ((ms-id (getf issue :milestone-id))
             (comments (list-issue-comments (getf issue :id)))
             (comment-reactions (let ((h (make-hash-table)))
                                  (dolist (c comments)
                                    (setf (gethash (getf c :id) h)
                                          (list-reactions "issue_comment" (getf c :id)
                                                          *current-user-id*)))
                                  h)))
        (html-response
         (view-issue :owner-name owner :repo repo :issue issue
                     :author (find-user-by-id (getf issue :author-id))
                     :comments comments
                     :labels (issue-labels (getf issue :id))
                     :assignees (issue-assignees (getf issue :id))
                     :milestone (when (and ms-id (not (eq ms-id :null)))
                                  (find-milestone ms-id))
                     :milestones (list-milestones (getf repo :id) :state "open")
                     :reactions (list-reactions "issue" (getf issue :id) *current-user-id*)
                     :comment-reactions comment-reactions
                     :pinned (let ((p (getf issue :pin-order))) (and p (not (eq p :null))))
                     :can-edit (and (member-of-repo-p repo) t)))))))

(easy-routes:defroute issue-react-submit
    ("/:owner/:repo-name/issues/:number/react" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (let* ((num (parse-integer number :junk-allowed t))
             (issue (when num (find-issue (getf repo :id) num)))
             (emoji (hunchentoot:post-parameter "emoji"))
             (comment-id (parse-integer (or (hunchentoot:post-parameter "comment_id") "")
                                        :junk-allowed t)))
        (unless issue (return-from issue-react-submit (not-found)))
        ;; Only allow a small fixed set of reaction emoji.
        (when (member emoji '("👍" "👎" "😄" "🎉" "😕" "❤️" "🚀" "👀") :test #'equal)
          (if comment-id
              (toggle-reaction "issue_comment" comment-id *current-user-id* emoji)
              (toggle-reaction "issue" (getf issue :id) *current-user-id* emoji)))
        (hunchentoot:redirect
         (format nil "/~A/~A/issues/~A" owner repo-name num))))))

(easy-routes:defroute issue-pin-submit
    ("/:owner/:repo-name/issues/:number/pin" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (unless (member-of-repo-p repo)
        (setf (hunchentoot:return-code*) 403)
        (return-from issue-pin-submit "Forbidden"))
      (let* ((num (parse-integer number :junk-allowed t))
             (issue (when num (find-issue (getf repo :id) num))))
        (unless issue (return-from issue-pin-submit (not-found)))
        (if (let ((p (getf issue :pin-order))) (and p (not (eq p :null))))
            (unpin-issue (getf issue :id))
            (pin-issue (getf issue :id) (getf repo :id)))
        (hunchentoot:redirect
         (format nil "/~A/~A/issues/~A" owner repo-name num))))))

(easy-routes:defroute issue-meta-submit
    ("/:owner/:repo-name/issues/:number/meta" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (unless (member-of-repo-p repo)
        (setf (hunchentoot:return-code*) 403)
        (return-from issue-meta-submit "Forbidden"))
      (let* ((num (parse-integer number :junk-allowed t))
             (issue (when num (find-issue (getf repo :id) num))))
        (unless issue (return-from issue-meta-submit (not-found)))
        ;; Labels: comma-separated strings.
        (set-issue-labels (getf issue :id)
                          (uiop:split-string (or (hunchentoot:post-parameter "labels") "")
                                             :separator '(#\,)))
        ;; Assignees: comma-separated usernames → ids (unknown names ignored).
        (let ((ids (loop for name in (uiop:split-string
                                      (or (hunchentoot:post-parameter "assignees") "")
                                      :separator '(#\,))
                         for trimmed = (string-trim " " name)
                         for u = (and (plusp (length trimmed))
                                      (find-user-by-username trimmed))
                         when u collect (getf u :id))))
          (set-issue-assignees (getf issue :id) ids))
        ;; Milestone: id or empty → clear.
        (let ((mid (parse-integer (or (hunchentoot:post-parameter "milestone_id") "")
                                  :junk-allowed t)))
          (set-issue-milestone (getf issue :id) mid))
        (hunchentoot:redirect
         (format nil "/~A/~A/issues/~A" owner repo-name num))))))

(easy-routes:defroute issue-comment-submit
    ("/:owner/:repo-name/issues/:number/comment" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (issue (when (and repo num) (find-issue (getf repo :id) num))))
      (unless (and repo issue) (return-from issue-comment-submit (not-found)))
      (let ((body (hunchentoot:post-parameter "body"))
            (action (hunchentoot:post-parameter "action")))
        ;; Add comment if body is non-empty
        (when (and body (not (uiop:emptyp body)))
          (create-issue-comment :issue-id (getf issue :id)
                                :author-id *current-user-id*
                                :body body))
        ;; Close or reopen
        (when (equal action "close")
          (update-issue (getf issue :id) :status "closed")
          (when (or (not body) (uiop:emptyp body))
            (create-issue-comment :issue-id (getf issue :id)
                                  :author-id *current-user-id*
                                  :body "Closed this issue.")))
        (when (equal action "reopen")
          (update-issue (getf issue :id) :status "open")
          (when (or (not body) (uiop:emptyp body))
            (create-issue-comment :issue-id (getf issue :id)
                                  :author-id *current-user-id*
                                  :body "Reopened this issue.")))
        ;; Notify
        (let ((comment-text (or body
                                (cond ((equal action "close") "Closed this issue.")
                                      ((equal action "reopen") "Reopened this issue.")))))
          (when comment-text
            (notify-issue-comment repo owner repo-name issue comment-text)
            (fire-webhooks (getf repo :id) "issue" (make-webhook-payload "issue.comment" :owner owner :repo repo-name :number (getf issue :number))))))
      (hunchentoot:redirect
       (format nil "/~A/~A/issues/~A" owner repo-name number)))))

;; ----------------------------------------------------------------------------
;; Routes: Pull requests

(easy-routes:defroute pulls-page
    ("/:owner/:repo-name/pulls" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((status (or (hunchentoot:get-parameter "status") "open"))
           (pulls (list-pull-requests (getf repo :id) :status status)))
      (html-response
       (view-pull-requests :owner-name owner :repo repo :pulls pulls
                        :current-status status)))))

(easy-routes:defroute new-pull-request-page
    ("/:owner/:repo-name/pulls/new" :method :get) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (let* ((branches (chamber-get-branches owner repo-name))
             (default-branch (or (chamber-get-default-branch owner repo-name) "main")))
        (html-response
         (view-new-pull-request :owner-name owner :repo repo
                                :branches branches
                                :default-branch default-branch))))))

(easy-routes:defroute create-pull-request-submit
    ("/:owner/:repo-name/pulls/new" :method :post) ()
  (when (require-login)
    (with-visible-repo (repo owner repo-name #'not-found)
      (let* ((source (hunchentoot:post-parameter "source_branch"))
             (target (hunchentoot:post-parameter "target_branch"))
             (disk-path (repo-disk-path owner repo-name))
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
        ;; Snapshot round 1 for interdiff.
        (when head-commit
          (record-changeset-version (getf pr :id) 1 head-commit
                                    (git-merge-base disk-path target head-commit)))
        ;; Schedule automations
        (schedule-automations (getf repo :id) "changeset_opened"
                              :commit-sha head-commit
                              :ref source
                              :triggered-by-id *current-user-id*)
        ;; Notify the repo owner/members/watchers, plus CODEOWNERS of the files.
        (ignore-errors (notify-pr-opened repo owner repo-name pr))
        (ignore-errors
         (notify-code-owners owner repo-name repo pr
                             (pr-code-owners owner repo-name pr)))
        (hunchentoot:redirect
         (format nil "/~A/~A/pulls/~A" owner repo-name (getf pr :number)))))))

(easy-routes:defroute pull-request-page
    ("/:owner/:repo-name/pulls/:number" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from pull-request-page (not-found)))
      (let* ((author (find-user-by-id (getf pr :author-id)))
             (reviews-raw (list-reviews (getf pr :id)))
             (concerns-all (list-concerns (getf pr :id)))
             (reviews (mapcar
                       (lambda (r)
                         (append r
                          (list :is-stale (review-is-stale-p r pr)
                                :concerns (remove-if-not
                                           (lambda (c) (= (getf c :review-id) (getf r :id)))
                                           concerns-all))))
                       reviews-raw))
             (eligibility (compute-merge-eligibility pr repo))
             (mergeable (pull-request-mergeable-p eligibility))
             (is-admin (and *current-user-id*
                            (equal (repo-member-role (getf repo :id) *current-user-id*)
                                   "admin")))
             (can-merge (and mergeable is-admin))
             ;; Admin escape hatch: PR fails a requirement but an admin may
             ;; still override (as long as it is open).
             (can-override (and is-admin (not mergeable)
                                (not (getf pr :is-merged))
                                (not (getf pr :is-closed))))
             ;; Conflicting files (if any) ride on the :conflicts eligibility rule.
             (conflict-files (getf (find :conflicts eligibility
                                         :key (lambda (r) (getf r :kind)))
                                   :conflict-files))
             (stack (find-stack-by-id (getf pr :stack-id)))
             (stack-items (when stack (list-stack-pull-requests (getf stack :id))))
             ;; Diff
             (source (getf pr :source-branch))
             (target (getf pr :target-branch))
             (source-missing (and (not (getf pr :is-merged))
                                  (not (member source (chamber-get-branches owner repo-name)
                                               :test #'equal))))
             (diff-raw (chamber-get-diff-merge-base owner repo-name target source))
             ;; Inline diff comments
             (raw-comments (list-diff-comments (getf pr :id)))
             (comment-hts (mapcar (lambda (c)
                                    (let ((ht (make-hash-table :test 'equal)))
                                      (setf (gethash "file_path" ht) (getf c :file-path))
                                      (setf (gethash "line_number" ht) (getf c :line-number))
                                      (setf (gethash "side" ht) (getf c :side))
                                      (setf (gethash "body" ht) (getf c :body))
                                      (setf (gethash "username" ht) (getf c :username))
                                      (setf (gethash "created_at" ht)
                                            (princ-to-string (getf c :created-at)))
                                      ht))
                                  raw-comments))
             (checks-mv (if (getf pr :head-commit)
                            (multiple-value-list
                             (pull-request-checks (getf repo :id) (getf pr :head-commit)
                                                  owner repo-name))
                            (list nil (list :total 0 :success 0 :failure 0
                                            :pending 0 :overall "none"))))
             (checks (first checks-mv))
             (checks-rollup (second checks-mv))
             (can-close (and *current-user-id*
                             (or (eql (getf pr :author-id) *current-user-id*)
                                 (member-of-repo-p repo))))
             (code-owners (ignore-errors (pr-code-owners owner repo-name pr)))
             (versions (list-changeset-versions (getf pr :id)))
             (comments-json (if comment-hts
                                (com.inuoe.jzon:stringify comment-hts)
                                "[]")))
        (html-response
         (view-pull-request :owner-name owner :repo repo :pr pr
                         :author author :reviews reviews
                         :eligibility eligibility :can-merge can-merge
                         :can-override can-override :conflict-files conflict-files
                         :stack stack :stack-items stack-items
                         :diff-raw diff-raw :source-missing source-missing
                         :diff-comments-json comments-json
                         :comment-action (format nil "/~A/~A/pulls/~A/diff-comment"
                                                 owner repo-name num)
                         :checks checks :checks-rollup checks-rollup
                         :can-close can-close :code-owners code-owners
                         :versions versions))))))

(easy-routes:defroute pull-request-interdiff
    ("/:owner/:repo-name/pulls/:number/interdiff" :method :get) (&get from to)
  "Show the interdiff (git range-diff) between two rounds of a pull request."
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num)))
           (vf (and pr from (find-changeset-version (getf pr :id)
                                                    (parse-integer from :junk-allowed t))))
           (vt (and pr to (find-changeset-version (getf pr :id)
                                                  (parse-integer to :junk-allowed t)))))
      (unless (and pr vf vt) (return-from pull-request-interdiff (not-found)))
      (let* ((disk (repo-disk-path owner repo-name))
             (text (git-range-diff disk
                                   (or (let ((b (getf vf :base-commit)))
                                         (unless (eq b :null) b))
                                       (getf vf :head-commit))
                                   (getf vf :head-commit)
                                   (or (let ((b (getf vt :base-commit)))
                                         (unless (eq b :null) b))
                                       (getf vt :head-commit))
                                   (getf vt :head-commit))))
        (html-response
         (view-interdiff :owner-name owner :repo repo :pr pr
                         :from-version (getf vf :version) :to-version (getf vt :version)
                         :text text))))))

(easy-routes:defroute pull-request-checks-json
    ("/:owner/:repo-name/pulls/:number/checks.json" :method :get) ()
  (with-visible-repo (repo owner repo-name #'not-found)
    (let* ((num (parse-integer number :junk-allowed t))
           (pr (when num (find-pull-request (getf repo :id) num))))
      (unless pr (return-from pull-request-checks-json (not-found)))
      (setf (hunchentoot:content-type*) "application/json")
      (multiple-value-bind (checks rollup)
          (if (getf pr :head-commit)
              (pull-request-checks (getf repo :id) (getf pr :head-commit) owner repo-name)
              (values nil (list :total 0 :success 0 :failure 0 :pending 0 :overall "none")))
        (let ((root (make-hash-table :test 'equal))
              (rh (make-hash-table :test 'equal))
              (carr (make-array (length checks))))
          (setf (gethash "total" rh) (getf rollup :total)
                (gethash "success" rh) (getf rollup :success)
                (gethash "failure" rh) (getf rollup :failure)
                (gethash "pending" rh) (getf rollup :pending)
                (gethash "overall" rh) (getf rollup :overall))
          (loop for c in checks for i from 0
                do (let ((ch (make-hash-table :test 'equal)))
                     (setf (gethash "name" ch) (getf c :name)
                           (gethash "state" ch) (getf c :state)
                           (gethash "description" ch) (or (getf c :description) "")
                           (gethash "url" ch) (or (getf c :url) ""))
                     (setf (aref carr i) ch)))
          (setf (gethash "rollup" root) rh
                (gethash "checks" root) carr)
          (com.inuoe.jzon:stringify root))))))

;; Inline diff comment
(easy-routes:defroute diff-comment-submit
    ("/:owner/:repo-name/pulls/:number/diff-comment" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless (and repo pr) (return-from diff-comment-submit (not-found)))
      (let ((file-path (hunchentoot:post-parameter "file_path"))
            (line-number (parse-integer (or (hunchentoot:post-parameter "line_number") "0")
                                        :junk-allowed t))
            (side (or (hunchentoot:post-parameter "side") "new"))
            (body (hunchentoot:post-parameter "body")))
        (when (and file-path line-number body (not (uiop:emptyp body)))
          (create-diff-comment :changeset-id (getf pr :id)
                               :author-id *current-user-id*
                               :file-path file-path
                               :line-number line-number
                               :side side
                               :body body)))
      (hunchentoot:redirect
       (format nil "/~A/~A/pulls/~A" owner repo-name number)))))

(easy-routes:defroute submit-review
    ("/:owner/:repo-name/pulls/:number/review" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless repo (return-from submit-review repo))
      (unless pr (return-from submit-review (not-found)))
      (unless (repo-reviewer-p (getf repo :id) *current-user-id*)
        (setf (hunchentoot:return-code*) 403)
        (return-from submit-review "Forbidden"))
      (let* ((state (hunchentoot:post-parameter "state"))
             (body (hunchentoot:post-parameter "body"))
             (concern-text (hunchentoot:post-parameter "concern_text"))
             (review (create-review
                      :changeset-id (getf pr :id)
                      :reviewer-id *current-user-id*
                      :state state
                      :body (unless (uiop:emptyp body) body)
                      :changeset-version (getf pr :version))))
        (when (and (equal state "approve_with_concerns")
                   (not (uiop:emptyp concern-text)))
          (create-concern :review-id (getf review :id)
                          :changeset-id (getf pr :id)
                          :author-id *current-user-id*
                          :body concern-text))
        (log-event "review.submitted"
                   :user-id *current-user-id*
                   :repo-id (getf repo :id)
                   :entity-type "review"
                   :entity-id (getf review :id))
        (notify-pr-review repo owner repo-name pr state)
        (fire-webhooks (getf repo :id) "pull_request" (make-webhook-payload "pull_request.reviewed" :owner owner :repo repo-name :number (getf pr :number) :state state))
        ;; An approval may make an auto-merge-armed PR eligible.
        (try-auto-merge owner repo-name (getf pr :id))
        (hunchentoot:redirect
         (format nil "/~A/~A/pulls/~A" owner repo-name number))))))

(easy-routes:defroute resolve-concern-submit
    ("/:owner/:repo-name/concerns/:concern-id/resolve" :method :post) ()
  (when (require-login)
    (let* ((cid (parse-integer concern-id :junk-allowed t))
           (concern (when cid (find-concern-by-id cid))))
      (when concern
        (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
               (role (when repo (repo-member-role (getf repo :id) *current-user-id*))))
          (when (or (= (getf concern :author-id) *current-user-id*)
                    (equal role "admin"))
            (resolve-concern cid *current-user-id*))))
      (let ((pr (when concern (find-pull-request-by-id (getf concern :changeset-id)))))
        (hunchentoot:redirect
         (if pr
             (format nil "/~A/~A/pulls/~A" owner repo-name (getf pr :number))
             (format nil "/~A/~A" owner repo-name)))))))

(easy-routes:defroute pull-request-state-submit
    ("/:owner/:repo-name/pulls/:number/state" :method :post) ()
  "Change a pull request's state: close/reopen, draft/ready, or arm/disarm
auto-merge. Allowed for the PR author or any repo member."
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless (and repo pr) (return-from pull-request-state-submit (not-found)))
      (unless (or (eql (getf pr :author-id) *current-user-id*)
                  (member-of-repo-p repo))
        (setf (hunchentoot:return-code*) 403)
        (return-from pull-request-state-submit "Forbidden"))
      (let ((action (hunchentoot:post-parameter "action"))
            (open-p (and (not (getf pr :is-merged)) (not (getf pr :is-closed)))))
        (cond
          ((and (equal action "close") (not (getf pr :is-merged)))
           (close-pull-request (getf pr :id)))
          ((and (equal action "reopen")
                (not (getf pr :is-merged)) (getf pr :is-closed))
           (reopen-pull-request (getf pr :id)))
          ((and (equal action "draft") open-p)
           (set-pull-request-draft (getf pr :id) t))
          ((and (equal action "ready") open-p)
           (set-pull-request-draft (getf pr :id) nil))
          ((and (equal action "auto-merge") open-p (not (getf pr :is-draft)))
           (let ((s (hunchentoot:post-parameter "strategy")))
             (set-pull-request-auto-merge
              (getf pr :id)
              (cond ((equal s "squash") "squash")
                    ((equal s "fast-forward-only") "fast-forward-only")
                    (t "merge"))
              *current-user-id*)
             ;; Maybe it's already eligible — merge right away.
             (try-auto-merge owner repo-name (getf pr :id))))
          ((equal action "disable-auto-merge")
           (set-pull-request-auto-merge (getf pr :id) nil *current-user-id*))))
      (hunchentoot:redirect (format nil "/~A/~A/pulls/~A" owner repo-name num)))))

(defun sync-repo-push-mirrors (owner repo-name repo-id)
  "Push REPO to all its enabled push mirrors, in a background thread.

Normal git pushes sync mirrors via the post-receive hook (which shells out to
`cave-server sync-mirrors`). A web/API merge updates the target ref out of band
of that hook, so without this the mirror silently falls behind (issue #18).
Best-effort: records per-mirror status, logs failures, never signals into the
caller."
  (bt2:make-thread
   (lambda ()
     (handler-case
         (postmodern:with-connection *db-spec*
           (dolist (m (list-mirrors repo-id))
             (when (and (equal (getf m :direction) "push") (getf m :enabled))
               (multiple-value-bind (ok err)
                   (chamber-push-mirror owner repo-name
                                        (getf m :remote-url) (getf m :auth-token))
                 (if ok
                     (progn
                       (update-mirror-sync (getf m :id))
                       (llog:info "Push mirror synced"
                                  :repo (format nil "~A/~A" owner repo-name)
                                  :url (getf m :remote-url)))
                     (progn
                       (update-mirror-sync (getf m :id) :error err)
                       (llog:warn "Push mirror failed"
                                  :repo (format nil "~A/~A" owner repo-name)
                                  :url (getf m :remote-url) :error err)))))))
       (error (e)
         (llog:warn "Push mirror sync error"
                    :repo (format nil "~A/~A" owner repo-name)
                    :error (princ-to-string e)))))
   :name (format nil "push-mirror-sync-~A/~A" owner repo-name)))

(defun perform-pr-merge (owner repo-name pr repo strategy actor-id)
  "Run the git merge for PR with STRATEGY, verify the target advanced, mark the
PR merged, auto-delete the source branch if configured, and fire post-merge side
effects. Returns (values OK MESSAGE). Caller is responsible for permission and
eligibility checks."
  (let* ((source (getf pr :source-branch))
         (target (getf pr :target-branch))
         (strategy (cond ((equal strategy "squash") "squash")
                         ((equal strategy "fast-forward-only") "fast-forward-only")
                         (t "merge")))
         (message (format nil "Merge pull request #~A from ~A into ~A"
                          (getf pr :number) source target))
         (disk (repo-disk-path owner repo-name))
         (already-merged (zerop (nth-value 2 (git-run disk "merge-base" "--is-ancestor"
                                                      source target))))
         (before (nth-value 0 (git-run disk "rev-parse" target)))
         (merged (or already-merged
                     (chamber-merge-branch owner repo-name source target
                                           :strategy strategy :message message)))
         (after (nth-value 0 (git-run disk "rev-parse" target))))
    (cond
      ((not merged)
       (values nil (if (equal strategy "fast-forward-only")
                       "Merge failed — not a fast-forward (the target branch has commits the source doesn't)."
                       "Merge failed — conflicts?")))
      ((and (not already-merged) before after (string= before after))
       (values nil "Merge reported success but the target branch did not advance — aborted (storage may be degraded). The source branch was left intact."))
      (t
       (merge-pull-request (getf pr :id))
       (when (getf repo :auto-delete-branch)
         (chamber-delete-branch owner repo-name source))
       (log-event "pr.merged" :user-id actor-id :repo-id (getf repo :id)
                  :entity-type "pull_request" :entity-id (getf pr :id))
       (notify-pr-merged repo owner repo-name pr)
       (schedule-automations (getf repo :id) "changeset_merged"
                             :commit-sha (getf pr :head-commit)
                             :ref source :triggered-by-id actor-id)
       (fire-webhooks (getf repo :id) "pull_request"
                      (make-webhook-payload "pull_request.merged"
                                            :owner owner :repo repo-name
                                            :number (getf pr :number)))
       ;; The merge updated the target ref out of band of the post-receive hook,
       ;; so sync push mirrors ourselves — otherwise they fall behind (#18).
       (sync-repo-push-mirrors owner repo-name (getf repo :id))
       (values t "merged")))))

(defun try-auto-merge (owner repo-name pr-id)
  "If PR-ID has auto-merge armed and is now eligible, merge it. Safe to call from
any trigger (review submitted, status reported); a no-op otherwise."
  (ignore-errors
   (let* ((repo (find-repo owner repo-name))
          (pr (and repo (find-pull-request-by-id pr-id)))
          (strategy (and pr (getf pr :auto-merge-strategy))))
     (when (and pr strategy (not (eq strategy :null))
                (not (getf pr :is-merged)) (not (getf pr :is-closed))
                (not (getf pr :is-draft))
                (pull-request-mergeable-p (compute-merge-eligibility pr repo)))
       (let ((by (getf pr :auto-merge-by)))
         (perform-pr-merge owner repo-name pr repo strategy
                           (unless (eq by :null) by)))))))

(easy-routes:defroute merge-pull-request-submit
    ("/:owner/:repo-name/pulls/:number/merge" :method :post) ()
  (when (require-login)
    (let* ((repo (ensure-repo-visible (find-repo owner repo-name) #'not-found))
           (num (parse-integer number :junk-allowed t))
           (pr (when (and repo num) (find-pull-request (getf repo :id) num))))
      (unless repo (return-from merge-pull-request-submit repo))
      (unless pr (return-from merge-pull-request-submit (not-found)))
      (unless (equal (repo-member-role (getf repo :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from merge-pull-request-submit "Forbidden"))
      ;; A closed/already-merged PR can never be merged, even under override.
      (when (or (getf pr :is-merged) (getf pr :is-closed))
        (setf (hunchentoot:return-code*) 409)
        (return-from merge-pull-request-submit "Pull request is already merged or closed"))
      ;; Mark as manually merged: an admin asserts the PR's changes already
      ;; landed on the target (e.g. they resolved conflicts and pushed out of
      ;; band). Validate the pasted commit is actually an ancestor of the target
      ;; branch, then mark merged — bypassing the eligibility gate and the
      ;; auto-merge entirely (mirrors Gitea's MergeStyleManuallyMerged).
      (let ((manual (hunchentoot:post-parameter "manual_merge_commit")))
        (when (and manual (plusp (length (string-trim " " manual))))
          (let ((commit (string-trim " " manual))
                (target (getf pr :target-branch))
                (disk (repo-disk-path owner repo-name)))
            (unless (git-commit-ancestor-p disk commit target)
              (setf (hunchentoot:return-code*) 409)
              (return-from merge-pull-request-submit
                (format nil "Commit ~A is not on ~A — paste a commit that is already merged into the target branch."
                        commit target)))
            (merge-pull-request (getf pr :id))
            (log-event "pr.merged"
                       :user-id *current-user-id* :repo-id (getf repo :id)
                       :entity-type "pull_request" :entity-id (getf pr :id)
                       :metadata (format nil "Manually merged at ~A" commit))
            (notify-pr-merged repo owner repo-name pr)
            (fire-webhooks (getf repo :id) "pull_request"
                           (make-webhook-payload "pull_request.merged"
                                                 :owner owner :repo repo-name
                                                 :number (getf pr :number)))
            (return-from merge-pull-request-submit
              (hunchentoot:redirect (format nil "/~A/~A/pulls/~A" owner repo-name number))))))
      ;; Admins (the only role that reaches here) may bypass the eligibility
      ;; gate by posting override=t — used by the "merge anyway" UI. Audit-log
      ;; any override so a bypassed check is traceable.
      (let* ((override (let ((o (hunchentoot:post-parameter "override")))
                         (and o (member o '("t" "true" "1" "on" "yes")
                                        :test #'string-equal))))
             (eligibility (compute-merge-eligibility pr repo)))
        (unless (or (pull-request-mergeable-p eligibility) override)
          (setf (hunchentoot:return-code*) 422)
          (return-from merge-pull-request-submit "Pull request is not mergeable"))
        (when override
          (log-event "pr.merge_override"
                     :user-id *current-user-id*
                     :repo-id (getf repo :id)
                     :entity-type "pull_request"
                     :entity-id (getf pr :id)
                     :metadata (format nil "Admin override; failing rules: ~{~A~^; ~}"
                                       (loop for r in eligibility
                                             unless (getf r :pass)
                                             collect (getf r :description))))))
      ;; Perform the merge via the shared helper (also used by auto-merge).
      (let ((strategy (hunchentoot:post-parameter "strategy")))
        (multiple-value-bind (ok msg)
            (perform-pr-merge owner repo-name pr repo strategy *current-user-id*)
          (unless ok
            (setf (hunchentoot:return-code*) 409)
            (return-from merge-pull-request-submit msg))))
      (hunchentoot:redirect
       (format nil "/~A/~A/pulls/~A" owner repo-name number)))))

