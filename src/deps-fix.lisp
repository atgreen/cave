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
    ;; Never open a fix PR on a pull-mirror: the next mirror sync prunes the
    ;; fix branch (leaving an empty, unmergeable PR) and a fix can't land on a
    ;; mirror anyway. Alerts still surface — we just don't auto-fix.
    (when (pull-mirror-repo-p (getf alert :repo-id))
      (return-from %prepare-fix-commit
        (list :status :manual :fix-kind fix-kind
              :reason "repo is a pull-mirror; auto-fix branches are pruned by sync")))
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
    (ignore-errors
     (let ((repo (find-repo-by-id repo-id)))
       (when repo
         (notify-pr-opened repo (repo-owner-name repo) (getf repo :name) pr))))
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
    (when (and (auto-fix-security-enabled-p (getf alert :repo-id))
               (not (pull-mirror-repo-p (getf alert :repo-id))))
      (handler-case
          (let ((r (start-speculative-fix (getf alert :id))))
            (llog:info "Dependency fix triggered"
                       :alert (getf alert :id) :result (getf r :status)))
        (error (e) (llog:warn "start-speculative-fix failed"
                              :alert (getf alert :id) :error (princ-to-string e))))))
  (process-pending-ocicl-fixes))

;;; --- ocicl lockfile fixes (runner-based) -----------------------------------
;;;
;;; ocicl deps can't be fixed by editing a version string — the lockfile pins a
;;; system to <project>-<date>-<sha> plus an OCI digest. So a cave-fix runner job
;;; re-resolves it with `ocicl latest` (always newest, so it supersedes), returns
;;; the regenerated ocicl.csv, and cave verifies the new commit actually clears
;;; the advisory (ancestry) before committing it to a fix branch — one PR per
;;; project, deduped so a repo never has two fix jobs in flight at once.

(defparameter *deps-fix-workflow-name* "deps-fix")
(defparameter *ocicl-csv-marker* "===OCICL-CSV-BELOW===")

(defun enqueue-ocicl-fix (repo-id ref project systems &key triggered-by-id)
  "Schedule a cave-fix runner job: `ocicl latest SYSTEMS` + `ocicl clean`,
   returning the regenerated ocicl.csv via the step log. Returns the run, or NIL."
  (let ((repo (find-repo-by-id repo-id)))
    (when (and repo systems)
      (let* ((labels (config-value :deps-scan-labels ""))
             (runs-on (when (plusp (length labels))
                        (mapcar (lambda (s) (string-trim " " s))
                                (uiop:split-string labels :separator '(#\,)))))
             (run (create-workflow-run
                   :repo-id repo-id :workflow-name *deps-fix-workflow-name*
                   :workflow-file "" :trigger-event "deps_fix"
                   :ref ref :triggered-by-id triggered-by-id))
             (job (create-workflow-job
                   :workflow-run-id (getf run :id)
                   :name (format nil "ocicl-fix:~A" project)
                   :image (config-value :deps-fix-image "ghcr.io/atgreen/cave-fix:main")
                   :runs-on runs-on)))
        ;; `;` not `&&`: emit ocicl.csv even if `ocicl latest` partly fails — the
        ;; completion hook decides per-system whether the bump cleared anything.
        (create-workflow-step
         :job-id (getf job :id) :step-order 1 :name "ocicl-latest"
         :command (format nil "cd /workspace && ocicl latest ~{~A~^ ~} >/dev/null 2>&1; ocicl clean >/dev/null 2>&1; echo ~A; cat ocicl.csv"
                          systems *ocicl-csv-marker*))
        run))))

(defun process-pending-ocicl-fixes ()
  "Enqueue one cave-fix job per repo (its first project with open ocicl alerts)
   when the repo allows auto-fix and has no fix job already in flight. One job
   per repo at a time -> no duplicate PRs; `ocicl latest` -> newer supersedes."
  (dolist (rr (repos-with-open-ocicl-alerts))
    (let ((repo-id (getf rr :repo-id))
          (ref (getf rr :ref)))
      (when (and (auto-fix-security-enabled-p repo-id)
                 (not (pull-mirror-repo-p repo-id))
                 (not (repo-deps-fix-in-flight-p repo-id)))
        (handler-case
            (let ((by-project (make-hash-table :test 'equal)))
              (dolist (tgt (list-open-ocicl-fix-targets repo-id))
                (when (equal (getf tgt :ref) ref)
                  (pushnew (getf tgt :system)
                           (gethash (getf tgt :project) by-project) :test #'equal)))
              (let ((projects (loop for k being the hash-key of by-project collect k)))
                (when projects
                  (let* ((project (first projects))
                         ;; Bump the project's whole system set together (they
                         ;; share a dir) so the lockfile stays consistent.
                         (systems (or (let* ((p (find-ocicl-project project))
                                             (s (and p (getf p :systems))))
                                        (when (and s (not (eq s :null)))
                                          (coerce s 'list)))
                                      (gethash project by-project))))
                    (enqueue-ocicl-fix repo-id ref project systems)
                    (llog:info "Enqueued ocicl fix" :repo repo-id :project project)))))
          (error (e) (llog:warn "process-pending-ocicl-fixes failed"
                                :repo repo-id :error (princ-to-string e))))))))

(defun %ocicl-deps-by-system (deps)
  "Hash system-name -> dep plist, from ocicl-csv->deps output."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (d deps h) (setf (gethash (getf d :package-name) h) d))))

(defun %deps-fix-new-csv (run-id)
  "The regenerated ocicl.csv from RUN-ID's step log (everything after the marker)."
  (let* ((log (with-output-to-string (out)
                (dolist (job (list-workflow-jobs run-id))
                  (dolist (step (list-workflow-steps (getf job :id)))
                    (let ((l (getf step :log)))
                      (when (and l (not (eq l :null))) (write-string l out)))))))
         (pos (search *ocicl-csv-marker* log)))
    (when pos
      (string-trim '(#\Space #\Newline #\Return #\Tab)
                   (subseq log (+ pos (length *ocicl-csv-marker*)))))))

(defun %ocicl-bump-clears-p (adv-repo new-commit introduced fixed)
  "True if NEW-COMMIT is no longer in the advisory's affected range on ADV-REPO
   (the bump now includes the fix). Conservative: NIL if undeterminable."
  (let ((path (ensure-advisory-repo adv-repo (list new-commit introduced fixed))))
    (and path (not (%commit-affected-p path new-commit introduced fixed)))))

(defun maybe-apply-ocicl-fix-run (run-id)
  "When a deps-fix run finalizes successfully, read the regenerated ocicl.csv,
   keep the alerts the bump actually clears (commit ancestry), and — if any —
   commit the lockfile to a fix branch and open one PR."
  (let ((run (find-workflow-run run-id)))
    (when (and run
               (equal (getf run :workflow-name) *deps-fix-workflow-name*)
               (equal (getf run :status) "success"))
      (handler-case (%apply-ocicl-fix-run run)
        (error (e) (llog:warn "ocicl fix apply failed"
                              :run run-id :error (princ-to-string e)))))))

(defun %apply-ocicl-fix-run (run)
  (let* ((repo-id (getf run :repo-id))
         (repo (find-repo-by-id repo-id))
         (owner (and repo (repo-owner-name repo)))
         (name (and repo (getf repo :name)))
         (ref (let ((r (getf run :ref)))
                (if (eq r :null) (chamber-get-default-branch owner name) r)))
         (new-csv (%deps-fix-new-csv (getf run :id)))
         (new-by-system (and new-csv (%ocicl-deps-by-system (ocicl-csv->deps new-csv)))))
    (when (and repo new-csv new-by-system)
      (let ((cleared '()))
        (dolist (tgt (list-open-ocicl-fix-targets repo-id))
          (when (equal (getf tgt :ref) ref)
            (let* ((newdep (gethash (getf tgt :system) new-by-system))
                   (newver (and newdep (getf newdep :version)))
                   (newcommit (and newver (%ocicl-version-commit newver))))
              (when (and newcommit
                         (%ocicl-bump-clears-p (getf tgt :adv-repo) newcommit
                                               (getf tgt :introduced) (getf tgt :fixed)))
                (push tgt cleared)))))
        (if (null cleared)
            (llog:info "ocicl fix: latest clears no advisory" :repo name :run (getf run :id))
            (%commit-ocicl-fix repo-id owner name ref new-csv (nreverse cleared)))))))

(defun %commit-ocicl-fix (repo-id owner name ref new-csv cleared)
  "Commit NEW-CSV to a fix branch, open one PR, link every CLEARED alert to it,
   and schedule the repo's PR CI on the fix commit. Auto-merge stays manual."
  (let* ((disk (repo-disk-path owner name))
         (systems (remove-duplicates (mapcar (lambda (a) (getf a :system)) cleared) :test #'equal))
         (advs (remove-duplicates (mapcar (lambda (a) (getf a :osv-id)) cleared) :test #'equal))
         (branch (format nil "cave/deps/ocicl-~A"
                         (%sanitize-branch-component
                          (format nil "~{~A~^-~}" (subseq systems 0 (min 3 (length systems)))))))
         (message (format nil "deps: bump ocicl ~{~A~^, ~}~%~%Fixes ~{~A~^, ~}." systems advs))
         (sha (git-commit-file-on-branch disk ref branch "ocicl.csv" new-csv message)))
    (when sha
      (let ((pr (create-pull-request
                 :repo-id repo-id :author-id (ensure-dependency-bot-user)
                 :source-branch branch :target-branch ref :head-commit sha)))
        (dolist (a cleared) (set-alert-fix-pr (getf a :alert-id) (getf pr :id)))
        (handler-case
            (parse-and-schedule-workflows repo-id "changeset_opened"
                                          :commit-sha sha :ref branch
                                          :triggered-by-id (ensure-dependency-bot-user))
          (error () nil))
        (llog:info "Opened ocicl fix PR" :repo name :pr (getf pr :id) :branch branch)
        (ignore-errors
         (let ((repo (find-repo-by-id repo-id)))
           (when repo
             (notify-pr-opened repo (repo-owner-name repo) (getf repo :name) pr))))
        pr))))
