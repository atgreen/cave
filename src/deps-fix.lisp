;;; deps-fix.lisp — propose dependency-fix pull requests.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Given a security alert with a fix version, classify how it can be fixed and,
;;; for the unambiguous manifest case, open a real bump PR. Anything that needs
;;; package-manager/lockfile tooling (ranged versions, transitive deps) is
;;; classified and reported :manual rather than guessed at — we never corrupt a
;;; manifest. CI-gated auto-merge is a later layer.
;;; See docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *fix-branch-prefix* "cave/deps"
  "Branch namespace for dependency-fix PRs.")

(defun classify-fix-kind (alert)
  "Classify how ALERT (a find-dep-alert-detailed plist) could be fixed:
   'none' (no fix version), 'lockfile' (ocicl — fixed by re-resolving the
   lockfile, never a manifest string edit), 'manifest' (direct dep), or
   'transitive_parent'."
  (let ((fix (getf alert :fix-version))
        (direct (getf alert :is-direct))
        (eco (getf alert :ecosystem)))
    (cond ((or (null fix) (eq fix :null)) "none")
          ;; ocicl deps are pinned in ocicl.csv and the advisory fix is a commit,
          ;; not a version string — a manifest edit would corrupt the lockfile.
          ;; Re-resolving via ocicl is a separate (runner-based) path.
          ((equal eco "ocicl") "lockfile")
          (direct "manifest")
          (t "transitive_parent"))))

(defun %replace-if-unique (content needle replacement)
  "Replace NEEDLE with REPLACEMENT in CONTENT iff NEEDLE occurs exactly once.
   Returns the new string, or NIL when NEEDLE is absent or ambiguous."
  (when (and (stringp content) (plusp (length needle)))
    (let ((count 0) (pos nil) (start 0))
      (loop for p = (search needle content :start2 start)
            while p
            do (incf count) (setf pos p start (+ p (length needle))))
      (when (= count 1)
        (concatenate 'string
                     (subseq content 0 pos)
                     replacement
                     (subseq content (+ pos (length needle))))))))

(defun %replace-all (content needle replacement)
  "Replace every occurrence of NEEDLE in CONTENT. Returns the new string, or NIL
   when NEEDLE is absent."
  (when (and (stringp content) (plusp (length needle)))
    (let ((start 0) (found nil) (out (make-string-output-stream)))
      (loop for p = (search needle content :start2 start)
            while p
            do (setf found t)
               (write-string content out :start start :end p)
               (write-string replacement out)
               (setf start (+ p (length needle))))
      (when found
        (write-string content out :start start)
        (get-output-stream-string out)))))

(defun safe-bump-manifest (content old-version new-version &key package)
  "Return CONTENT with the dependency version bumped, only when the edit is
   unambiguous (the target occurs exactly once) — never a guess that could
   corrupt a manifest. Prefers the qualified \"PACKAGE@old\" form, so a version
   tag shared across entries (e.g. GitHub Actions, where many actions pin @v4)
   is disambiguated and the package's v-prefix convention is preserved; falls
   back to a bare, unique version string."
  (when (stringp content)
    (or (when (and package (plusp (length package)) (plusp (length old-version)))
          (let* ((v-pref (char= (char old-version 0) #\v))
                 (new-v (if (and v-pref (plusp (length new-version))
                                 (char/= (char new-version 0) #\v))
                            (concatenate 'string "v" new-version)
                            new-version)))
            ;; The qualified PACKAGE@old token is specific to this dependency, so
            ;; every occurrence is the same pin (e.g. an action used in several
            ;; jobs) — bump them all, like Dependabot does.
            (%replace-all content
                          (format nil "~A@~A" package old-version)
                          (format nil "~A@~A" package new-v))))
        ;; Bare version fallback is ambiguous, so only when it occurs once.
        (when (plusp (length old-version))
          (%replace-if-unique content old-version new-version)))))

(defun %sanitize-branch-component (s)
  "Make S safe for a git ref component."
  (substitute-if #\- (lambda (c) (not (or (alphanumericp c)
                                          (member c '(#\. #\_ #\-)))))
                 s))

(defun %prepare-fix-commit (alert)
  "Classify ALERT and, for an unambiguous direct-manifest bump, create the fix
   branch + commit. Caches the fix kind on the alert. Returns a plist:
     (:status :ready :repo-id R :branch B :commit SHA :base BASE)
     (:status :no-fix)
     (:status :manual :fix-kind K :reason R)
     (:status :error :reason R)"
  (let* ((alert-id (getf alert :id))
         (fix-kind (classify-fix-kind alert)))
    (set-alert-fix-kind alert-id fix-kind)
    (cond
      ((string= fix-kind "none") (list :status :no-fix))
      ((not (string= fix-kind "manifest"))
       (list :status :manual :fix-kind fix-kind
             :reason "transitive/lockfile fix needs package-manager tooling"))
      (t
       (let* ((repo-id (getf alert :repo-id))
              (repo (find-repo-by-id repo-id))
              (owner (and repo (repo-owner-name repo)))
              (name (and repo (getf repo :name)))
              (manifest (getf alert :manifest-path)))
         (cond
           ((or (null repo) (null manifest) (string= manifest "sbom"))
            (list :status :manual :fix-kind fix-kind
                  :reason "no manifest path recorded for this dependency"))
           (t
            (let* ((disk (repo-disk-path owner name))
                   (base (or (chamber-get-default-branch owner name) "main"))
                   ;; syft records paths rooted at the scan dir ("/.github/...");
                   ;; git wants them repo-relative.
                   (rel (string-left-trim "/" manifest))
                   (pkg (getf alert :package-name))
                   (from (getf alert :version))
                   (to (getf alert :fix-version))
                   (content (handler-case (git-blob disk base rel) (error () nil)))
                   (new-content (safe-bump-manifest content from to :package pkg)))
              (cond
                ((null new-content)
                 (list :status :manual :fix-kind fix-kind
                       :reason "version not found unambiguously in manifest (ranged/lockfile)"))
                (t
                 (let* ((branch (format nil "~A/~A-~A-~A" *fix-branch-prefix*
                                        (%sanitize-branch-component (getf alert :ecosystem))
                                        (%sanitize-branch-component pkg)
                                        (%sanitize-branch-component to)))
                        (message (format nil "deps: bump ~A from ~A to ~A (~A)~%~%Fixes ~A."
                                         pkg from to (getf alert :ecosystem)
                                         (getf alert :osv-id)))
                        (sha (git-commit-file-on-branch disk base branch rel
                                                        new-content message)))
                   (if sha
                       (list :status :ready :repo-id repo-id :branch branch
                             :commit sha :base base)
                       (list :status :error :reason "failed to create fix commit")))))))))))))

(defun %open-pr-for-fix (alert-id repo-id branch base sha &key actor-id)
  "Open the fix PR and link it to ALERT-ID. Returns the PR plist."
  (let ((pr (create-pull-request
             :repo-id repo-id
             :author-id (or actor-id (ensure-dependency-bot-user))
             :source-branch branch :target-branch base :head-commit sha)))
    (set-alert-fix-pr alert-id (getf pr :id))
    pr))

(defun open-dependency-fix-pr (alert-id &key actor-id)
  "Immediately open a fix PR for ALERT-ID (no speculative build) — the on-demand
   path. Opens a real PR only for an unambiguous manifest version bump; returns
   :manual / :no-fix / :error otherwise. See %prepare-fix-commit."
  (let ((alert (find-dep-alert-detailed alert-id)))
    (unless alert
      (return-from open-dependency-fix-pr (list :status :error :reason "alert not found")))
    (let ((prep (%prepare-fix-commit alert)))
      (if (eq (getf prep :status) :ready)
          (let ((pr (%open-pr-for-fix alert-id (getf prep :repo-id) (getf prep :branch)
                                      (getf prep :base) (getf prep :commit) :actor-id actor-id)))
            (list :status :opened :pr pr :branch (getf prep :branch)
                  :commit (getf prep :commit)))
          prep))))

(defun start-speculative-fix (alert-id &key actor-id)
  "Dependabot-style: create the fix branch+commit, then run the repo's PR CI on
   it speculatively. The PR is opened later (by advance-speculative-fix-for-run)
   only once that build is green. If the repo has no PR CI to gate on, open the
   PR directly. Returns a plist describing the outcome."
  (when (dep-fix-attempt-for-alert alert-id)
    (return-from start-speculative-fix (list :status :exists)))
  (let ((alert (find-dep-alert-detailed alert-id)))
    (unless alert
      (return-from start-speculative-fix (list :status :error :reason "alert not found")))
    (let ((prep (%prepare-fix-commit alert)))
      (unless (eq (getf prep :status) :ready)
        (return-from start-speculative-fix prep))
      (let ((repo-id (getf prep :repo-id))
            (branch (getf prep :branch))
            (base (getf prep :base))
            (sha (getf prep :commit)))
        ;; Speculative build: run the repo's *pull_request* CI on the fix commit
        ;; — the checks that would gate a PR. This deliberately excludes
        ;; push-triggered release/deploy workflows (a speculative build must never
        ;; publish). Repos whose CI only triggers on push get no speculative
        ;; build (-> :none -> open the PR directly).
        (handler-case
            (parse-and-schedule-workflows repo-id "changeset_opened"
                                          :commit-sha sha :ref branch
                                          :triggered-by-id (or actor-id (ensure-dependency-bot-user)))
          (error (e) (llog:warn "speculative build scheduling failed"
                                :error (princ-to-string e))))
        (if (eq (speculative-build-status repo-id sha) :none)
            ;; No CI to gate on — open the PR now.
            (let* ((pr (%open-pr-for-fix alert-id repo-id branch base sha :actor-id actor-id))
                   (att (create-dep-fix-attempt :alert-id alert-id :repo-id repo-id
                                                :branch branch :commit-sha sha :state "no_ci")))
              (set-dep-fix-attempt-state (getf att :id) "no_ci" :pr-id (getf pr :id))
              (list :status :opened-no-ci :pr pr :branch branch))
            ;; Building — PR opens when the build goes green.
            (progn
              (create-dep-fix-attempt :alert-id alert-id :repo-id repo-id
                                      :branch branch :commit-sha sha :state "building")
              (list :status :building :branch branch :commit sha)))))))

(defun advance-speculative-fix-for-run (run-id)
  "Called when a workflow run finalizes. If RUN-ID's commit has building fix
   attempts, open their PRs (build green) or mark them build_failed (build red)."
  (handler-case
      (let ((run (find-workflow-run run-id)))
        (when run
          (let ((repo-id (getf run :repo-id))
                (sha (let ((c (getf run :commit-sha))) (unless (eq c :null) c))))
            (when (and repo-id sha)
              (let ((attempts (building-fix-attempts-for-commit repo-id sha)))
                (when attempts
                  (case (speculative-build-status repo-id sha)
                    (:success
                     (let* ((repo (find-repo-by-id repo-id))
                            (owner (and repo (repo-owner-name repo)))
                            (name (and repo (getf repo :name)))
                            (base (or (chamber-get-default-branch owner name) "main")))
                       (dolist (att attempts)
                         (let ((pr (%open-pr-for-fix (getf att :alert-id) repo-id
                                                     (getf att :branch) base
                                                     (getf att :commit-sha))))
                           (set-dep-fix-attempt-state (getf att :id) "opened"
                                                      :pr-id (getf pr :id))
                           (llog:info "Opened green dependency fix PR"
                                      :alert (getf att :alert-id) :pr (getf pr :id))))))
                    (:failure
                     (dolist (att attempts)
                       (set-dep-fix-attempt-state (getf att :id) "build_failed"
                                                  :detail "speculative build failed")
                       (llog:info "Dependency fix build failed; no PR opened"
                                  :alert (getf att :alert-id))))
                    (t nil))))))))   ; :pending -> wait for the remaining runs
    (error (e) (llog:warn "advance-speculative-fix failed" :error (princ-to-string e)))))

(defun process-pending-fixes ()
  "Start speculative fixes for open, manifest-fixable security alerts whose repo
   has auto-fix enabled and that don't already have a fix attempt. Safe to call
   repeatedly (e.g. after sync-advisories, or on a timer)."
  (dolist (alert (open-fixable-alerts-without-attempt))
    (when (auto-fix-security-enabled-p (getf alert :repo-id))
      (handler-case
          (let ((r (start-speculative-fix (getf alert :id))))
            (llog:info "Dependency fix triggered"
                       :alert (getf alert :id) :result (getf r :status)))
        (error (e) (llog:warn "start-speculative-fix failed"
                              :alert (getf alert :id) :error (princ-to-string e)))))))
