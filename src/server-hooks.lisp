(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Routes: Internal hooks (called by git hooks inside the container)

(defun visitor-ip-hash (request)
  "SHA-256 hash of remote IP salted with :secret-key. Raw IP never stored."
  (let* ((fwd (hunchentoot:header-in :x-forwarded-for request))
         (ip (or (and fwd (let ((c (position #\, fwd)))
                            (string-trim " " (if c (subseq fwd 0 c) fwd))))
                 (hunchentoot:remote-addr request)))
         (salt (or (config-value :secret-key) "cave"))
         (bytes (flexi-streams:string-to-octets
                 (concatenate 'string ip ":" salt)
                 :external-format :utf-8)))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha256 bytes))))

(defun referer-host (request)
  "Just the hostname from the Referer header — drop scheme/path/query.
   NIL when no referer, or when the referer is this same host."
  (let ((r (hunchentoot:header-in :referer request)))
    (when (and r (plusp (length r)))
      (let ((stripped (cond
                        ((uiop:string-prefix-p "https://" r) (subseq r 8))
                        ((uiop:string-prefix-p "http://" r)  (subseq r 7))
                        (t r))))
        (let* ((slash (position #\/ stripped))
               (host (if slash (subseq stripped 0 slash) stripped))
               (colon (position #\: host))
               (host (if colon (subseq host 0 colon) host))
               (self (and (config-value :base-url)
                          (let ((bu (string-right-trim "/" (config-value :base-url))))
                            (cond
                              ((uiop:string-prefix-p "https://" bu) (subseq bu 8))
                              ((uiop:string-prefix-p "http://" bu)  (subseq bu 7))
                              (t bu))))))
          (unless (and self (string-equal host self)) host))))))

(defun maybe-log-page-view (uri request)
  "Log a page view if URI looks like a repo subpath (owner/repo[/...])."
  ;; Skip uninteresting / internal URIs early
  (when (or (uiop:string-prefix-p "/-/" uri)
            (uiop:string-prefix-p "/static/" uri)
            (uiop:string-prefix-p "/u/" uri)
            (uiop:string-prefix-p "/o/" uri)
            (search ".git/" uri)
            (search "/-/internal/" uri))
    (return-from maybe-log-page-view nil))
  (let* ((trimmed (string-trim "/" uri))
         (parts (uiop:split-string trimmed :separator '(#\/))))
    ;; Need at least owner/repo
    (when (and (>= (length parts) 2)
               (plusp (length (first parts)))
               (plusp (length (second parts))))
      (let ((repo (find-repo (first parts) (second parts))))
        (when (and repo (repo-visible-p repo))
          (log-page-view (getf repo :id)
                         :ip-hash (visitor-ip-hash request)
                         :user-id *current-user-id*
                         :referer-host (referer-host request)))))))

(defun zero-sha-p (sha)
  (and sha (every (lambda (c) (char= c #\0)) sha)))

(defun push-commit-count (disk-path old new)
  "How many commits this push adds. NIL on failure."
  (multiple-value-bind (out _err exit)
      (if (zero-sha-p old)
          (git-run disk-path "rev-list" "--count" new)
          (git-run disk-path "rev-list" "--count" (format nil "~A..~A" old new)))
    (declare (ignore _err))
    (when (zerop exit) (parse-integer out :junk-allowed t))))

(defun push-tip-subject (disk-path new)
  "First line of NEW commit's message. NIL if NEW is a deletion or unreadable."
  (unless (zero-sha-p new)
    (multiple-value-bind (out _err exit)
        (git-run disk-path "log" "-1" "--format=%s" new)
      (declare (ignore _err))
      (when (zerop exit) (string-trim '(#\Space #\Newline #\Tab) out)))))

(defun build-push-metadata (disk-path ref old new)
  "Hash-table jzon will serialize as a JSON object for the event's metadata column."
  (let ((md (make-hash-table :test 'equal))
        (created (zero-sha-p old))
        (deleted (zero-sha-p new)))
    (setf (gethash "ref" md) ref
          (gethash "old" md) old
          (gethash "new" md) new)
    (when created (setf (gethash "created" md) t))
    (when deleted (setf (gethash "deleted" md) t))
    (unless deleted
      (let ((count (push-commit-count disk-path old new)))
        (when count (setf (gethash "count" md) count))))
    (let ((tip (push-tip-subject disk-path new)))
      (when tip (setf (gethash "tip" md) tip)))
    md))

(defun ensure-allowed-signers-file ()
  "Materialize an allowed_signers file from every SSH key in cave's DB.
Cached in /tmp, rewritten when the registered-key fingerprint set changes.
Returns the path."
  (let* ((path (merge-pathnames "cave-allowed-signers" (uiop:temporary-directory)))
         (keys (all-ssh-keys-with-user))
         (entries (loop for k in keys
                        collect (list (or (getf k :email)
                                          (getf k :username)
                                          (format nil "user-~A" (getf k :user-id)))
                                      (getf k :public-key)))))
    (write-allowed-signers entries path)
    path))

(defun make-gpg-keyring (tag)
  "Materialize a throwaway GnuPG home holding every registered GPG public key,
so the sandboxed `git verify-commit` (with GNUPGHOME pointed here) can validate
GPG-signed commits. Returns the homedir path, or NIL when no GPG keys are
registered. The caller must delete the directory when done.

Lives under /tmp because the git sandbox grants /tmp --rw but not the data dir;
TAG (the repo id) keys it per-push so concurrent pushes never share a homedir
mid-rebuild. The keyring content is identical for every push (all registered
keys), so isolation only avoids transient read-during-rebuild flakiness."
  (let ((keys (all-gpg-keys-with-user)))
    (when keys
      (let ((dir (uiop:ensure-directory-pathname
                  (merge-pathnames (format nil "cave-gpg-~A/" tag)
                                   (uiop:temporary-directory)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
        (ensure-directories-exist dir)
        ;; gpg refuses to use a homedir more permissive than 0700.
        (uiop:run-program (list "chmod" "700" (namestring dir)) :ignore-error-status t)
        (dolist (k keys)
          (with-input-from-string (in (getf k :public-key))
            (uiop:run-program
             (list "gpg" "--homedir" (namestring dir) "--batch" "--import")
             :input in :output nil :error-output nil :ignore-error-status t)))
        dir))))

(defun verify-commits (repo disk-path shas)
  "Verify the signatures of SHAS in REPO (DISK-PATH is its bare repo) and upsert
the results into cave_commit_signatures. The SSH allowed-signers file and the
GPG keyring are built once for the whole batch. Shared by the push hook and the
`reverify` command."
  (when shas
    (let ((signers (ensure-allowed-signers-file))
            (key->user (let ((h (make-hash-table :test 'equal)))
                         (dolist (k (all-ssh-keys-with-user))
                           (setf (gethash (getf k :fingerprint) h) (getf k :user-id)))
                         h))
            ;; GPG keyring + fingerprint→user map, built once for this push.
            (gpg-home (make-gpg-keyring (getf repo :id)))
            (gpgkey->user (let ((h (make-hash-table :test 'equal)))
                            (dolist (k (all-gpg-keys-with-user))
                              (setf (gethash (getf k :key-id) h) (getf k :user-id)))
                            h)))
        (unwind-protect
             (dolist (sha shas)
               (multiple-value-bind (signed scheme)
                   (git-commit-signature-info disk-path sha)
                 (cond
                   ((not signed)
                    (record-commit-signature :repo-id (getf repo :id)
                                             :commit-sha sha :verified nil :scheme nil))
                   ((eq scheme :ssh)
                    (let* ((verified (git-verify-commit disk-path sha signers))
                           (fp (git-commit-signature-key disk-path sha)))
                      (record-commit-signature :repo-id (getf repo :id)
                                               :commit-sha sha :verified verified
                                               :scheme "ssh" :fingerprint fp
                                               :signer-user-id (gethash fp key->user))))
                   (t
                    ;; GPG: verify against the registered-key keyring (NIL when none).
                    (let* ((verified (and gpg-home
                                          (git-verify-commit-gpg disk-path sha gpg-home)))
                           (fp (and verified
                                    (git-commit-gpg-fingerprint disk-path sha gpg-home))))
                      (record-commit-signature :repo-id (getf repo :id)
                                               :commit-sha sha :verified verified
                                               :scheme "gpg" :fingerprint fp
                                               :signer-user-id (and fp (gethash fp gpgkey->user))))))))
          (when gpg-home
            (uiop:delete-directory-tree gpg-home :validate t :if-does-not-exist :ignore))))))

(defun verify-pushed-commits (owner-name repo disk-path refs)
  "For each ref update, verify newly-introduced commits' signatures and cache
the results. Skips deletes and zero-sha boundaries."
  (declare (ignore owner-name))
  (let* ((shas (loop for r in refs
                     when (and (not (zero-sha-p (getf r :new))))
                       append
                       (let ((range (if (zero-sha-p (getf r :old))
                                        (list (getf r :new))
                                        (multiple-value-bind (out _err exit)
                                            (git-run disk-path "rev-list"
                                                     (format nil "~A..~A"
                                                             (getf r :old) (getf r :new)))
                                          (declare (ignore _err))
                                          (if (zerop exit)
                                              (remove-if #'uiop:emptyp
                                                         (uiop:split-string
                                                          out :separator '(#\Newline)))
                                              nil)))))
                         range)))
         (shas (remove-duplicates shas :test #'equal)))
    (verify-commits repo disk-path shas)))

(defun reverify-all-signatures ()
  "Re-run verification for every commit that already has a signature row,
across all repos, using the currently registered SSH and GPG keys. Returns the
number of commits re-verified."
  (let ((n 0))
    (dolist (r (repos-with-signatures))
      (let ((disk-path (ignore-errors
                        (repo-disk-path (getf r :owner) (getf r :name))))
            (shas (repo-recorded-shas (getf r :id))))
        (when (and disk-path (probe-file disk-path) shas)
          (verify-commits (list :id (getf r :id)) disk-path shas)
          (incf n (length shas)))))
    n))

(defun parse-push-options (header)
  "Split the X-Cave-Push-Options header into a list of option strings."
  (when (and header (plusp (length header)))
    (remove "" (mapcar (lambda (s) (string-trim " " s))
                       (uiop:split-string header :separator '(#\,)))
            :test #'equal)))

(defun workflow-files-at (disk-path ref)
  "List .cave/workflows/* file paths present at REF (for `push -o verbose-ci`)."
  (multiple-value-bind (out err code)
      (git-run disk-path "ls-tree" "--name-only" ref ".cave/workflows/")
    (declare (ignore err))
    (when (zerop code)
      (remove-if #'uiop:emptyp (uiop:split-string out :separator '(#\Newline))))))

(easy-routes:defroute internal-post-receive
    ("/-/internal/hook/post-receive/:owner/:repo-name" :method :post) ()
  ;; Only accept from localhost
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-post-receive "Forbidden"))
  (let ((repo (find-repo owner repo-name)))
    (unless repo
      (setf (hunchentoot:return-code*) 404)
      (return-from internal-post-receive "Not found"))
    (let* ((actor (let ((a (hunchentoot:get-parameter "actor")))
                    (when (and a (plusp (length a)))
                      (parse-integer a :junk-allowed t))))
           (push-options (parse-push-options
                          (hunchentoot:header-in* :x-cave-push-options)))
           (skip-ci (and (intersection push-options '("skip-ci" "ci-skip")
                                       :test #'equal) t))
           (verbose-ci (and (intersection push-options '("verbose-ci" "ci-verbose")
                                          :test #'equal) t))
           ;; Parse refs from POST body (one per line: oldsha newsha refname).
           ;; Bound here in the outer let* so BOTH the processing block and the
           ;; push-time-hint block below can see it (previously refs was scoped
           ;; to only the first block, leaving the hint block referencing an
           ;; unbound variable — a 500 on every push).
           (refs (let ((body (hunchentoot:raw-post-data :force-text t))
                       (rs nil))
                   (dolist (line (uiop:split-string body :separator '(#\Newline))
                                 (nreverse rs))
                     (let ((parts (uiop:split-string line :separator '(#\Space))))
                       (when (>= (length parts) 3)
                         (push (list :old (first parts) :new (second parts)
                                     :ref (third parts))
                               rs)))))))
      (progn
        (when refs
          (touch-repo-pushed-at (getf repo :id)))
        ;; Log a rich git.push event per ref + schedule automations
        (let ((disk-path (repo-disk-path owner repo-name)))
          (dolist (r refs)
            (log-event "git.push"
                       :user-id actor
                       :repo-id (getf repo :id)
                       :metadata (build-push-metadata disk-path
                                                      (getf r :ref)
                                                      (getf r :old)
                                                      (getf r :new)))
            (schedule-automations (getf repo :id) "post_receive"
                                  :commit-sha (getf r :new)
                                  :ref (getf r :ref))
            ;; `git push -o skip-ci` suppresses workflow scheduling.
            (unless skip-ci
              (handler-case
                  (parse-and-schedule-workflows (getf repo :id) "post_receive"
                                                :commit-sha (getf r :new)
                                                :ref (getf r :ref))
                (error (e)
                  (llog:error "Workflow scheduling failed" :error (princ-to-string e)))))
            ;; Keep any open PR's head_commit in sync with its source branch tip,
            ;; so merge checks (and approval staleness) evaluate the actual head.
            ;; The version bump in update-pull-request-head re-stales prior
            ;; approvals — correct, since new commits changed the PR.
            (let* ((ref (getf r :ref))
                   (new (getf r :new))
                   (branch (when (and (>= (length ref) 11)
                                      (string= ref "refs/heads/" :end1 11))
                             (subseq ref 11))))
              (when (and branch new
                         (not (every (lambda (c) (char= c #\0)) new)))
                (let ((open-pr (find-pull-request-by-branch (getf repo :id) branch)))
                  (when (and open-pr (not (equal (getf open-pr :head-commit) new)))
                    (update-pull-request-head (getf open-pr :id) new)
                    ;; Snapshot the new round for interdiff.
                    (let ((fresh (find-pull-request-by-id (getf open-pr :id))))
                      (when fresh
                        (record-changeset-version
                         (getf fresh :id) (getf fresh :version) new
                         (git-merge-base disk-path (getf fresh :target-branch) new))))))))))
        ;; Verify any signed commits in the pushed range, cache results
        (handler-case (verify-pushed-commits owner repo (repo-disk-path owner repo-name) refs)
          (error (e)
            (llog:warn "Signature verification failed" :error (princ-to-string e))))
        ;; Invalidate Chamber cache for this repo
        (chamber-invalidate-repo owner repo-name)
        (when (multi-chamber-p)
          (broadcast-invalidate-cache owner repo-name))
        ;; Trigger Zoekt reindexing
        (zoekt-index-repo owner repo-name)
        ;; Scan dependencies on default-branch pushes (runner-based; no-op unless
        ;; :deps-scan-enabled). Enqueues a workflow job a syft runner picks up.
        (let ((default-branch (or (chamber-get-default-branch owner repo-name) "main")))
          (when (find-if (lambda (r)
                           (member (getf r :ref)
                                   (list (format nil "refs/heads/~A" default-branch)
                                         default-branch)
                                   :test #'equal))
                         refs)
            (handler-case (enqueue-deps-scan (getf repo :id) :ref default-branch)
              (error (e)
                (llog:warn "Dep scan enqueue failed"
                           :repo (format nil "~A/~A" owner repo-name)
                           :error (princ-to-string e))))
            ;; Refresh the stored primary language (powers Explore's filter).
            (handler-case
                (update-repo-primary-language owner repo-name (getf repo :id) default-branch)
              (error (e)
                (llog:warn "Primary-language update failed"
                           :repo (format nil "~A/~A" owner repo-name)
                           :error (princ-to-string e))))))
        ;; Fire webhooks
        (dolist (r refs)
          (fire-webhooks (getf repo :id) "push"
                         `(("ref" . ,(getf r :ref))
                           ("after" . ,(getf r :new))
                           ("before" . ,(getf r :old))
                           ("repository" . (("owner" . ,owner)
                                            ("name" . ,repo-name)))))))
    ;; Push-time hint (the post-receive hook echoes this back to the pusher):
    ;; suggest opening a PR for newly pushed feature branches that have none,
    ;; plus `git push -o verbose-ci` CI feedback.
    (let ((default-branch (or (chamber-get-default-branch owner repo-name) "main"))
          (base (config-value :base-url ""))
          (disk (repo-disk-path owner repo-name))
          (lines nil))
      (dolist (r refs)
        (let* ((ref (getf r :ref))
               (new (getf r :new))
               (branch (when (and (>= (length ref) 11)
                                  (string= ref "refs/heads/" :end1 11))
                         (subseq ref 11))))
          (when (and branch new
                     (not (every (lambda (c) (char= c #\0)) new))
                     (not (equal branch default-branch))
                     (not (find-pull-request-by-branch (getf repo :id) branch)))
            (push (format nil "Open a pull request for '~A': ~A/~A/~A/pulls/new"
                          branch base owner repo-name)
                  lines))
          ;; verbose-ci: report which workflows would run for this ref.
          (when (and verbose-ci new (not (every (lambda (c) (char= c #\0)) new)))
            (let ((wf (ignore-errors (workflow-files-at disk new))))
              (cond
                (skip-ci (push "CI: skipped (push -o skip-ci)" lines))
                ((null wf) (push "CI: no .cave/workflows found for this commit" lines))
                (t (push (format nil "CI: ~D workflow file~:P (~{~A~^, ~})"
                                 (length wf)
                                 (mapcar #'file-namestring wf))
                         lines)))))))
      (if lines (format nil "~{~A~^~%~}" (nreverse lines)) "")))))

(defun valid-runner-request-p ()
  "True when the request carries a valid runner bearer token."
  (let ((auth (hunchentoot:header-in* :authorization)))
    (and auth (>= (length auth) 7)
         (string-equal "Bearer " (subseq auth 0 7))
         (authenticate-runner (subseq auth 7))
         t)))

(easy-routes:defroute internal-repo-deps
    ("/-/internal/repos/:owner/:repo-name/deps" :method :post) (&get ref)
  "Ingest a CycloneDX SBOM into the dependency graph. Accepts localhost (the
   host-side scan) or a valid runner bearer token."
  (unless (or (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
              (valid-runner-request-p))
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-repo-deps "Forbidden"))
  (let ((repo (find-repo owner repo-name)))
    (unless repo
      (setf (hunchentoot:return-code*) 404)
      (return-from internal-repo-deps "Not found"))
    (let ((body (hunchentoot:raw-post-data :force-text t))
          (the-ref (or ref (chamber-get-default-branch owner repo-name) "main")))
      (handler-case
          (let ((n (ingest-repo-deps (getf repo :id) the-ref
                                     (scan-deps owner repo-name the-ref body))))
            (format nil "ingested ~A deps~%" n))
        (error (e)
          (setf (hunchentoot:return-code*) 400)
          (format nil "Bad SBOM: ~A~%" e))))))

;; ----------------------------------------------------------------------------
;; Push lock-bracket: SSH pushes acquire/release Chamber write locks via HTTP

(defvar *active-push-tokens* (make-hash-table :test 'equal))
(defvar *active-push-tokens-lock* (bt2:make-lock :name "push-tokens"))

(defun start-push-lock-reaper ()
  "Background thread that reaps orphaned push locks (SSH disconnect, timeout)."
  (bt2:make-thread
   (lambda ()
     (loop
       (sleep 60)
       (let ((now (get-universal-time))
             (max-age 600)
             (expired nil))
         (bt2:with-lock-held (*active-push-tokens-lock*)
           (maphash (lambda (token entry)
                      (when (> (- now (getf entry :time)) max-age)
                        (push (cons token entry) expired)))
                    *active-push-tokens*)
           (dolist (pair expired)
             (let ((token (car pair))
                   (entry (cdr pair)))
               (llog:warn "Reaping orphaned push lock"
                          :repo (format nil "~A/~A" (getf entry :owner) (getf entry :repo))
                          :age-seconds (- now (getf entry :time)))
               (chamber-invalidate-repo (getf entry :owner) (getf entry :repo))
               (when (multi-chamber-p)
                 (broadcast-invalidate-cache (getf entry :owner) (getf entry :repo)))
               (handler-case (bt2:signal-semaphore (getf entry :sema)) (error () nil))
               (handler-case (bt2:signal-semaphore *chamber-semaphore*) (error () nil))
               (remhash token *active-push-tokens*)))))))
   :name "push-lock-reaper"))

(easy-routes:defroute internal-push-acquire
    ("/-/internal/push/acquire/:owner/:repo-name" :method :post) ()
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-push-acquire "Forbidden"))
  (ensure-chamber-semaphore)
  (unless (bt2:wait-on-semaphore *chamber-semaphore* :timeout 30)
    (setf (hunchentoot:return-code*) 503)
    (return-from internal-push-acquire "Busy"))
  (let ((sema (get-repo-write-sema (format nil "~A/~A" owner repo-name))))
    (unless (bt2:wait-on-semaphore sema :timeout 30)
      (bt2:signal-semaphore *chamber-semaphore*)
      (setf (hunchentoot:return-code*) 503)
      (return-from internal-push-acquire "Busy"))
    (let ((token (fuuid:make-v4-string)))
      (bt2:with-lock-held (*active-push-tokens-lock*)
        (setf (gethash token *active-push-tokens*)
              (list :owner owner :repo repo-name
                    :time (get-universal-time) :sema sema)))
      (llog:info "Push lock acquired" :repo (format nil "~A/~A" owner repo-name))
      token)))

(easy-routes:defroute internal-push-release
    ("/-/internal/push/release/:owner/:repo-name" :method :post) ()
  (unless (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'equal)
    (setf (hunchentoot:return-code*) 403)
    (return-from internal-push-release "Forbidden"))
  (let* ((token (string-trim '(#\Space #\Newline #\Return)
                              (hunchentoot:raw-post-data :force-text t)))
         (entry (bt2:with-lock-held (*active-push-tokens-lock*)
                  (gethash token *active-push-tokens*))))
    (unless entry
      (setf (hunchentoot:return-code*) 404)
      (return-from internal-push-release "Unknown token"))
    (chamber-invalidate-repo owner repo-name)
    (when (multi-chamber-p)
      (broadcast-invalidate-cache owner repo-name))
    (handler-case (bt2:signal-semaphore (getf entry :sema)) (error () nil))
    (handler-case (bt2:signal-semaphore *chamber-semaphore*) (error () nil))
    (bt2:with-lock-held (*active-push-tokens-lock*)
      (remhash token *active-push-tokens*))
    (llog:info "Push lock released" :repo (format nil "~A/~A" owner repo-name))
    "ok"))

;; ----------------------------------------------------------------------------
;; Routes: Search

(easy-routes:defroute search-page ("/-/search" :method :get) ()
  (when (require-login)
    (let* ((query (or (hunchentoot:get-parameter "q") ""))
           (repo-scope (hunchentoot:get-parameter "repo"))
           (results (if (string= query "")
                        (list :files nil)
                        (zoekt-search-visible query :limit 50
                                              :repo-scope repo-scope))))
      (html-response
       (view-search-results :query query
                            :repo-scope repo-scope
                            :results results)))))

;; ----------------------------------------------------------------------------
;; Routes: Metrics

(easy-routes:defroute metrics-endpoint ("/-/metrics" :method :get) ()
  (setf (hunchentoot:content-type*) "text/plain; version=0.0.4; charset=utf-8")
  (collect-metrics))

