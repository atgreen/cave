;;; workflow.lisp — Workflow orchestration: parse, schedule, dependency resolution
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

(defun %env-map->string (env-raw)
  "Turn a parsed `env:` mapping (alist name -> value) into newline-joined
KEY=VALUE, coercing non-string values. Returns \"\" for anything non-map."
  (if (and (listp env-raw) (consp (car env-raw)))
      (with-output-to-string (s)
        (loop for (k . v) in env-raw
              when (stringp k)
                do (format s "~A=~A~%" k
                           (cond ((stringp v) v)
                                 ((null v) "")
                                 ((eq v t) "true")
                                 (t (princ-to-string v))))))
      ""))

;;; --- Admin runner policy (applies to repo-supplied workflow YAML only;
;;; Cave's internal dep-scan / dep-fix jobs are created directly and bypass
;;; this gate) ---

(defun %nixery-image (deps)
  "Build a Nixery on-the-fly image URL from nixpkgs names, so a workflow can say
`dependencies: [sbcl, git]` instead of maintaining a Dockerfile. nixery.dev
builds (and layer-caches) an image with the requested packages on demand."
  (when deps
    (let ((pkgs (remove-if #'uiop:emptyp
                           (mapcar (lambda (d) (string-trim " " (princ-to-string d)))
                                   (if (listp deps) deps (list deps))))))
      (when pkgs
        (format nil "nixery.dev/shell/~{~A~^/~}" pkgs)))))

(defun %workflow-image-allowed-p (image)
  "True unless :workflows-image-allowlist is configured and IMAGE matches none
   of its prefixes. An unset allowlist (the default) permits any image."
  (let ((allow (config-value :workflows-image-allowlist)))
    (or (null allow)
        (and image
             (some (lambda (prefix) (uiop:string-prefix-p prefix image)) allow)))))

(defun workflow-policy-violation (image privileged)
  "Return a human-readable reason if a job running IMAGE with PRIVILEGED is
   disallowed by admin policy (cave.conf), or NIL when permitted. Privileged is
   denied by default; image pinning is opt-in via :workflows-image-allowlist."
  (cond
    ((and privileged (not (config-value :workflows-allow-privileged)))
     "privileged jobs are disabled by admin policy (:workflows-allow-privileged)")
    ((not (%workflow-image-allowed-p image))
     (format nil "image '~A' is not in the allowed registries (:workflows-image-allowlist)"
             image))))

(defun parse-and-schedule-workflows (repo-id trigger &key commit-sha ref triggered-by-id)
  "Discover and schedule workflow runs from .cave/workflows/*.yml at COMMIT-SHA.
   TRIGGER is the Cave event name (post_receive, changeset_opened, etc.)."
  (let* ((repo (find-repo-by-id repo-id))
         (owner-name (when repo (repo-owner-name repo)))
         (repo-name (when repo (getf repo :name)))
         (disk-path (when (and owner-name repo-name)
                      (repo-disk-path owner-name repo-name)))
         (git-ref (or commit-sha ref "HEAD"))
         (yaml-trigger (trigger-to-yaml-event trigger)))
    (unless (and disk-path yaml-trigger)
      (return-from parse-and-schedule-workflows nil))
    ;; List .cave/workflows directory
    (let ((entries (handler-case
                       (git-tree disk-path :ref git-ref :path ".cave/workflows")
                     (error () nil))))
      (dolist (entry entries)
        (let ((name (getf entry :name)))
          (when (and (equal (getf entry :type) "blob")
                     (or (uiop:string-suffix-p name ".yml")
                         (uiop:string-suffix-p name ".yaml")))
            (handler-case
                (let ((content (git-blob disk-path git-ref
                                         (format nil ".cave/workflows/~A" name))))
                  (when content
                    (schedule-workflow-from-yaml repo-id name content
                                                 yaml-trigger
                                                 :commit-sha commit-sha
                                                 :ref ref
                                                 :triggered-by-id triggered-by-id)))
              (error (e)
                (llog:error "Failed to parse workflow"
                            :file name :error (princ-to-string e))))))))))

(defun trigger-to-yaml-event (trigger)
  "Map Cave trigger name to workflow YAML event name."
  (cond
    ((equal trigger "post_receive") "push")
    ((member trigger '("changeset_opened" "changeset_updated") :test #'equal)
     "pull_request")
    ((equal trigger "changeset_merged") "pull_request")
    ((equal trigger "manual") "manual")
    (t nil)))

(defun schedule-workflow-from-yaml (repo-id filename content trigger
                                    &key commit-sha ref triggered-by-id)
  "Parse a workflow YAML file and create run/jobs/steps if trigger matches."
  (let ((workflow (yaml-parse content)))
    (unless workflow (return-from schedule-workflow-from-yaml nil))
    ;; Check if trigger matches. Triggers are ref-aware: a push to a tag
    ;; (ref refs/tags/*) satisfies both `push` and `tag`, while a branch push
    ;; satisfies only `push`. This lets a release workflow say `on: [tag]` and
    ;; never schedule on ordinary commits.
    (let* ((on-field (cdr (assoc "on" workflow :test #'equal)))
           (triggers (if (listp on-field) on-field (list on-field)))
           (is-tag (and ref (uiop:string-prefix-p "refs/tags/" ref)))
           (effective (cond
                        ((and (equal trigger "push") is-tag) '("push" "tag"))
                        ((equal trigger "push") '("push"))
                        (t (list trigger)))))
      (unless (some (lambda (e) (member e triggers :test #'equal)) effective)
        (return-from schedule-workflow-from-yaml nil))
      ;; Create workflow run
      (let* ((name (or (cdr (assoc "name" workflow :test #'equal))
                       (pathname-name filename)))
             ;; Workflow-level `env:` — prepended to each job's env (job/step
             ;; levels override by appearing later in the merged KEY=VALUE list).
             (wf-env (%env-map->string (cdr (assoc "env" workflow :test #'equal))))
             (run (create-workflow-run
                   :repo-id repo-id
                   :workflow-name name
                   :workflow-file filename
                   :trigger-event trigger
                   :commit-sha commit-sha
                   :ref ref
                   :triggered-by-id triggered-by-id)))
        (llog:info "Created workflow run"
                   :name name :file filename :run-id (getf run :id))
        ;; Create jobs and steps
        (let ((jobs-alist (cdr (assoc "jobs" workflow :test #'equal))))
          ;; Admin policy gate: if any job in this workflow violates policy
          ;; (privileged when disabled, or an image outside the allowlist),
          ;; reject the entire run — don't dispatch any job — and surface the
          ;; reason as a failed run so the author sees why.
          (let ((violation
                  (when (listp jobs-alist)
                    (loop for (job-name . job-spec) in jobs-alist
                          for image = (or (cdr (assoc "image" job-spec :test #'equal))
                                          (%nixery-image
                                           (cdr (assoc "dependencies" job-spec :test #'equal))))
                          for priv = (eq (cdr (assoc "privileged" job-spec :test #'equal)) t)
                          thereis (and job-name image
                                       (workflow-policy-violation image priv))))))
            (when violation
              (let ((j (create-workflow-job :workflow-run-id (getf run :id)
                                            :name (format nil "blocked by policy: ~A" violation)
                                            :image "-")))
                (update-job-status (getf j :id) "failure"))
              (update-workflow-run-status (getf run :id) "failure")
              (llog:warn "Workflow run blocked by admin policy"
                         :run-id (getf run :id) :file filename :reason violation)
              (return-from schedule-workflow-from-yaml run)))
          (when (and jobs-alist (listp jobs-alist))
            (dolist (job-entry jobs-alist)
              (let* ((job-name (car job-entry))
                     (job-spec (cdr job-entry))
                     (image (or (cdr (assoc "image" job-spec :test #'equal))
                                ;; No image? Build one on the fly from nix deps.
                                (%nixery-image
                                 (cdr (assoc "dependencies" job-spec :test #'equal)))))
                     (needs-raw (cdr (assoc "needs" job-spec :test #'equal)))
                     (needs (cond
                              ((null needs-raw) nil)
                              ((listp needs-raw) needs-raw)
                              (t (list needs-raw))))
                     (runs-on-raw (cdr (assoc "runs-on" job-spec :test #'equal)))
                     (runs-on (cond
                                ((null runs-on-raw) nil)
                                ((listp runs-on-raw) runs-on-raw)
                                (t (list runs-on-raw))))
                     (job-timeout (let ((v (cdr (assoc "timeout" job-spec :test #'equal))))
                                    (when (integerp v) v)))
                     (job-continue-on-error (cdr (assoc "continue-on-error" job-spec :test #'equal)))
                     (job-privileged (cdr (assoc "privileged" job-spec :test #'equal)))
                     (cache-raw (cdr (assoc "cache" job-spec :test #'equal)))
                     (cache-paths (cond
                                    ((null cache-raw) nil)
                                    ((listp cache-raw)
                                     (remove-if-not #'stringp cache-raw))
                                    ((stringp cache-raw) (list cache-raw))
                                    (t nil)))
                     ;; Merged workflow+job env (workflow first so job overrides).
                     (job-env (concatenate 'string wf-env
                                           (%env-map->string
                                            (cdr (assoc "env" job-spec :test #'equal)))))
                     (steps-raw (cdr (assoc "steps" job-spec :test #'equal))))
                (when (and job-name image)
                  (let ((job (create-workflow-job
                              :workflow-run-id (getf run :id)
                              :name job-name
                              :image image
                              :needs needs
                              :runs-on runs-on
                              :timeout-seconds job-timeout
                              :continue-on-error (eq job-continue-on-error t)
                              :privileged (eq job-privileged t)
                              :cache-paths cache-paths
                              :env job-env)))
                    (llog:info "Created workflow job"
                               :job job-name :job-id (getf job :id))
                    ;; Create steps
                    (when (and steps-raw (listp steps-raw))
                      (loop for step-spec in steps-raw
                            for order from 1
                            do (let ((step-name (cdr (assoc "name" step-spec :test #'equal)))
                                     (command (cdr (assoc "run" step-spec :test #'equal)))
                                     (step-timeout (let ((v (cdr (assoc "timeout" step-spec :test #'equal))))
                                                     (when (integerp v) v)))
                                     (step-continue-on-error (cdr (assoc "continue-on-error" step-spec :test #'equal)))
                                     (step-env (%env-map->string
                                                (cdr (assoc "env" step-spec :test #'equal)))))
                                 (when command
                                   (create-workflow-step
                                    :job-id (getf job :id)
                                    :step-order order
                                    :name step-name
                                    :command command
                                    :timeout-seconds step-timeout
                                    :continue-on-error (eq step-continue-on-error t)
                                    :env step-env))))))))))
        run)))))

(defun rerun-workflow (run-id)
  "Re-create a fresh run from RUN-ID's workflow file at its commit. Reuses
   schedule-workflow-from-yaml (same trigger the original matched), so the admin
   policy gate still applies. Returns the new run plist, or NIL."
  (let* ((run (find-workflow-run run-id))
         (repo-id (and run (getf run :repo-id)))
         (file (and run (getf run :workflow-file)))
         (commit (and run (getf run :commit-sha)))
         (trigger (and run (getf run :trigger-event)))
         (repo (and repo-id (find-repo-by-id repo-id)))
         (owner (and repo (repo-owner-name repo)))
         (name (and repo (getf repo :name))))
    (when (and file owner name trigger)
      (let ((content (handler-case
                         (git-blob (repo-disk-path owner name)
                                   (if (and commit (not (eq commit :null))) commit "HEAD")
                                   (format nil ".cave/workflows/~A" file))
                       (error () nil))))
        (when content
          (schedule-workflow-from-yaml repo-id file content trigger
                                       :commit-sha (and commit (not (eq commit :null)) commit)
                                       :ref (getf run :ref)))))))

(defun check-workflow-job-completion (job)
  "After a job completes, update dependent jobs and the workflow run.
   If the job failed, skip dependent jobs. If succeeded, unblock dependents.
   When all jobs are terminal, finalize the workflow run."
  (let* ((run-id (getf job :workflow-run-id))
         (job-name (getf job :name))
         (job-status (getf job :status))
         (continue-on-error (eq (getf job :continue-on-error) t))
         (all-jobs (list-workflow-jobs run-id)))
    ;; If failed and NOT continue-on-error: skip jobs that depend on this one
    (when (and (equal job-status "failure") (not continue-on-error))
      (dolist (j all-jobs)
        (when (and (member (getf j :status) '("queued" "blocked") :test #'equal)
                   (job-depends-on-p j job-name))
          (update-job-status (getf j :id) "skipped")
          ;; Transitively skip jobs depending on the skipped job
          (check-workflow-job-completion
           (list :workflow-run-id run-id
                 :name (getf j :name)
                 :status "skipped")))))
    ;; If succeeded (or failed with continue-on-error): unblock waiting jobs
    (when (or (equal job-status "success")
              (and (equal job-status "failure") continue-on-error))
      (let ((refreshed-jobs (list-workflow-jobs run-id)))
        (dolist (j refreshed-jobs)
          (when (equal (getf j :status) "blocked")
            (let ((needs (parse-needs-string (getf j :needs))))
              (when (every (lambda (dep-name)
                            (let ((dep (find dep-name refreshed-jobs
                                             :key (lambda (x) (getf x :name))
                                             :test #'equal)))
                              (and dep (or (equal (getf dep :status) "success")
                                           (and (equal (getf dep :status) "failure")
                                                (eq (getf dep :continue-on-error) t))))))
                          needs)
                (update-job-status (getf j :id) "queued")))))))
    ;; Check if all jobs are terminal → finalize run
    (let ((final-jobs (list-workflow-jobs run-id)))
      (when (every (lambda (j)
                     (member (getf j :status)
                             '("success" "failure" "skipped" "cancelled")
                             :test #'equal))
                   final-jobs)
        (let ((final-status (if (every (lambda (j)
                                         (member (getf j :status)
                                                 '("success" "skipped")
                                                 :test #'equal))
                                       final-jobs)
                                "success" "failure")))
          (update-workflow-run-status run-id final-status)
          ;; Dependency-scan runs (Option B): ingest the SBOM from the step log.
          (maybe-ingest-scan-run run-id)
          ;; ocicl fix runs: validate the bump cleared the advisory, then PR.
          (maybe-apply-ocicl-fix-run run-id)
          ;; Speculative dependency-fix builds: open the PR if green, else hold.
          (advance-speculative-fix-for-run run-id))))))

(defun job-depends-on-p (job dep-name)
  "Check if JOB depends on DEP-NAME."
  (let ((needs (parse-needs-string (getf job :needs))))
    (member dep-name needs :test #'equal)))

(defun parse-needs-string (needs-str)
  "Parse comma-separated needs string into a list."
  (when (and needs-str (not (eq needs-str :null)) (not (uiop:emptyp needs-str)))
    (mapcar (lambda (s) (string-trim '(#\Space) s))
            (uiop:split-string needs-str :separator '(#\,)))))
