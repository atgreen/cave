;;; workflow.lisp — Workflow orchestration: parse, schedule, dependency resolution
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

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
    ;; Check if trigger matches
    (let* ((on-field (cdr (assoc "on" workflow :test #'equal)))
           (triggers (if (listp on-field) on-field (list on-field))))
      (unless (member trigger triggers :test #'equal)
        (return-from schedule-workflow-from-yaml nil))
      ;; Create workflow run
      (let* ((name (or (cdr (assoc "name" workflow :test #'equal))
                       (pathname-name filename)))
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
          (when (and jobs-alist (listp jobs-alist))
            (dolist (job-entry jobs-alist)
              (let* ((job-name (car job-entry))
                     (job-spec (cdr job-entry))
                     (image (cdr (assoc "image" job-spec :test #'equal)))
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
                     (steps-raw (cdr (assoc "steps" job-spec :test #'equal))))
                (when (and job-name image)
                  (let ((job (create-workflow-job
                              :workflow-run-id (getf run :id)
                              :name job-name
                              :image image
                              :needs needs
                              :runs-on runs-on
                              :timeout-seconds job-timeout)))
                    (llog:info "Created workflow job"
                               :job job-name :job-id (getf job :id))
                    ;; Create steps
                    (when (and steps-raw (listp steps-raw))
                      (loop for step-spec in steps-raw
                            for order from 1
                            do (let ((step-name (cdr (assoc "name" step-spec :test #'equal)))
                                     (command (cdr (assoc "run" step-spec :test #'equal)))
                                     (step-timeout (let ((v (cdr (assoc "timeout" step-spec :test #'equal))))
                                                     (when (integerp v) v))))
                                 (when command
                                   (create-workflow-step
                                    :job-id (getf job :id)
                                    :step-order order
                                    :name step-name
                                    :command command
                                    :timeout-seconds step-timeout)))))))))))
        run))))

(defun check-workflow-job-completion (job)
  "After a job completes, update dependent jobs and the workflow run.
   If the job failed, skip dependent jobs. If succeeded, unblock dependents.
   When all jobs are terminal, finalize the workflow run."
  (let* ((run-id (getf job :workflow-run-id))
         (job-name (getf job :name))
         (job-status (getf job :status))
         (all-jobs (list-workflow-jobs run-id)))
    ;; If failed: skip jobs that depend on this one
    (when (equal job-status "failure")
      (dolist (j all-jobs)
        (when (and (member (getf j :status) '("queued" "blocked") :test #'equal)
                   (job-depends-on-p j job-name))
          (update-job-status (getf j :id) "skipped")
          ;; Transitively skip jobs depending on the skipped job
          (check-workflow-job-completion
           (list :workflow-run-id run-id
                 :name (getf j :name)
                 :status "skipped")))))
    ;; If succeeded: unblock waiting jobs whose deps are now all met
    (when (equal job-status "success")
      (let ((refreshed-jobs (list-workflow-jobs run-id)))
        (dolist (j refreshed-jobs)
          (when (equal (getf j :status) "blocked")
            (let ((needs (parse-needs-string (getf j :needs))))
              (when (every (lambda (dep-name)
                            (let ((dep (find dep-name refreshed-jobs
                                             :key (lambda (x) (getf x :name))
                                             :test #'equal)))
                              (and dep (equal (getf dep :status) "success"))))
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
          (update-workflow-run-status run-id final-status))))))

(defun job-depends-on-p (job dep-name)
  "Check if JOB depends on DEP-NAME."
  (let ((needs (parse-needs-string (getf job :needs))))
    (member dep-name needs :test #'equal)))

(defun parse-needs-string (needs-str)
  "Parse comma-separated needs string into a list."
  (when (and needs-str (not (eq needs-str :null)) (not (uiop:emptyp needs-str)))
    (mapcar (lambda (s) (string-trim '(#\Space) s))
            (uiop:split-string needs-str :separator '(#\,)))))
