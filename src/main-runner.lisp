(in-package #:cave)

(defun %runner-arch ()
  "GitHub-Actions-style RUNNER_ARCH for the host CPU."
  (let ((m (string-trim '(#\Newline #\Space)
                        (handler-case (nth-value 0 (uiop:run-program '("uname" "-m")
                                                                     :output '(:string :stripped t)))
                          (error () "x86_64")))))
    (cond ((member m '("x86_64" "amd64") :test #'equal) "X64")
          ((member m '("aarch64" "arm64") :test #'equal) "ARM64")
          (t (string-upcase m)))))

(defun make-runner-command ()
  (clingon:make-command
   :name "runner"
   :description "Start a Cave runner agent"
   :options (list
             (clingon:make-option :string
              :long-name "url" :key :url :required t
              :description "Cave gRPC server URL (e.g. grpc://localhost:9443)")
             (clingon:make-option :string
              :long-name "token" :key :token
              :description "Registration token (or use --token-file)")
             (clingon:make-option :string
              :long-name "token-file" :key :token-file
              :description "Read the registration token from this file. Re-read on
                           each start, so an ExecStartPre that mints a fresh token
                           keeps a restarting runner working despite single-use tokens.")
             (clingon:make-option :string
              :long-name "name" :key :name
              :description "Runner name (default: hostname)")
             (clingon:make-option :string
              :long-name "labels" :key :labels
              :description "Comma-separated labels (e.g. linux,fast)")
             (clingon:make-option :boolean
              :long-name "ephemeral" :key :ephemeral
              :description "Exit after one task"))
   :handler #'handle-runner))

;;; --- RUNNER-TOKEN subcommand ---

(defun make-runner-token-command ()
  (clingon:make-command
   :name "runner-token"
   :description "Mint a single-use runner registration token (instance/org/repo/user scope)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "scope" :key :scope :initial-value "instance"
              :description "Token scope: instance, org, repo, or user")
             (clingon:make-option :string
              :long-name "org" :key :org
              :description "Org name (implies --scope org)")
             (clingon:make-option :string
              :long-name "repo" :key :repo
              :description "Repo as owner/name (implies --scope repo)")
             (clingon:make-option :string
              :long-name "user" :key :user
              :description "Username (implies --scope user)")
             (clingon:make-option :flag
              :long-name "quiet" :key :quiet
              :description "Print only the token, with no trailing detail"))
   :handler #'handle-runner-token
   :examples '(("Mint an instance-scoped token:" . "cave-server runner-token")
               ("Mint a repo-scoped token:" . "cave-server runner-token --repo atgreen/cave")
               ("For a quadlet ExecStartPre:" . "cave-server runner-token --quiet"))))

(defun handle-runner-token (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (scope (clingon:getopt cmd :scope))
        (org-name (clingon:getopt cmd :org))
        (repo-spec (clingon:getopt cmd :repo))
        (user-name (clingon:getopt cmd :user))
        (quiet (clingon:getopt cmd :quiet))
        ;; Keep the real stdout for the token alone. llog's console appender is a
        ;; synonym-stream to *standard-output*, so binding it to *error-output*
        ;; below routes all log/config noise to stderr — leaving stdout clean
        ;; enough to redirect straight into a --token-file.
        (real-out *standard-output*))
    (let ((*standard-output* *error-output*))
    (load-config config-path)
    (connect-db)
    (flet ((fail (fmt &rest args)
             ;; uiop:quit unwinds the stack, so the unwind-protect below still
             ;; closes the DB connection.
             (format *error-output* "~&~?~%" fmt args)
             (uiop:quit 1)))
      (unwind-protect
           (let ((scope-id :null))
             ;; A scope-bearing flag overrides --scope and resolves the id.
             (cond
               (org-name
                (let ((org (find-org-by-name org-name)))
                  (unless org (fail "No such org: ~A" org-name))
                  (setf scope "org" scope-id (getf org :id))))
               (repo-spec
                (let* ((slash (position #\/ repo-spec))
                       (owner (and slash (subseq repo-spec 0 slash)))
                       (rname (and slash (subseq repo-spec (1+ slash))))
                       (repo (and owner rname (find-repo owner rname))))
                  (unless repo (fail "No such repo: ~A (use owner/name)" repo-spec))
                  (setf scope "repo" scope-id (getf repo :id))))
               (user-name
                (let ((user (find-user-by-username user-name)))
                  (unless user (fail "No such user: ~A" user-name))
                  (setf scope "user" scope-id (getf user :id)))))
             (unless (member scope '("instance" "org" "repo" "user") :test #'string=)
               (fail "Invalid scope: ~A (instance, org, repo, or user)" scope))
             (let ((record (create-registration-token
                            :scope scope
                            :scope-id (unless (eq scope-id :null) scope-id))))
               (if quiet
                   (format real-out "~A~%" (getf record :token))
                   (format real-out "~&~A~%  scope: ~A~@[ #~A~]~%  expires: ~A~%"
                           (getf record :token) scope
                           (unless (eq scope-id :null) scope-id)
                           (getf record :expires-at)))))
        (disconnect-db))))))

(defun parse-grpc-url (url)
  "Parse grpc://host:port or grpcs://host[:port] into (host port tls-p).
   grpcs:// defaults to port 443; grpc:// defaults to port 9443."
  (let* ((tls-p (uiop:string-prefix-p "grpcs://" url))
         (stripped (cond
                     ((uiop:string-prefix-p "grpcs://" url) (subseq url 8))
                     ((uiop:string-prefix-p "grpc://" url) (subseq url 7))
                     (t url)))
         (colon (position #\: stripped))
         (host (if colon (subseq stripped 0 colon) stripped))
         (port (cond
                 (colon (parse-integer (subseq stripped (1+ colon)) :junk-allowed t))
                 (tls-p 443)
                 (t 9443))))
    (list host port tls-p)))

(defun handle-runner (cmd)
  (let* ((url (clingon:getopt cmd :url))
         (token-file (clingon:getopt cmd :token-file))
         (token (or (clingon:getopt cmd :token)
                    (and token-file
                         (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (uiop:read-file-string token-file)))))
         (name (or (clingon:getopt cmd :name)
                   (machine-instance)
                   "cave-runner"))
         (runner-labels (or (clingon:getopt cmd :labels) ""))
         (ephemeral (clingon:getopt cmd :ephemeral))
         (parsed (parse-grpc-url url))
         (host (first parsed))
         (port (second parsed))
         (tls-p (third parsed)))

    (when (or (null token) (zerop (length token)))
      (format *error-output*
              "~&No registration token. Pass --token <value> or --token-file <path>.~%")
      (uiop:quit 1))

    (format t "~&Cave Runner~%  Server: ~A:~A~A~%  Name: ~A~%  Labels: ~A~%  Ephemeral: ~A~%"
            host port (if tls-p " (TLS)" "") name runner-labels ephemeral)

    (labels
        ((make-auth-metadata (auth-token)
           (ag-grpc:alist-to-metadata `(("authorization" . ,(format nil "Bearer ~A" auth-token)))))

         (execute-task (channel auth-token task)
           "Execute a task — dispatch between simple automation and workflow job."
           (let ((job-id (handler-case (slot-value task 'cave::job-id) (error () 0))))
             (if (and job-id (plusp job-id))
                 (execute-workflow-job channel auth-token task)
                 (execute-simple-task channel auth-token task))))

         (execute-simple-task (channel auth-token task)
           "Execute a simple automation task (single command)."
           (let ((run-id (slot-value task 'cave::run-id))
                 (repo-owner (slot-value task 'cave::repo-owner))
                 (repo-name (slot-value task 'cave::repo-name))
                 (command (slot-value task 'cave::command)))
             (format t "~&Task #~A: ~A/~A — ~A~%" run-id repo-owner repo-name command)
             (ag-grpc:grpc-call channel
                                "/cave.runner.RunnerService/UpdateTaskStatus"
                                (make-instance 'cave::update-task-status-request
                                               :run-id run-id :status "running")
                                :response-type 'cave::update-task-status-response
                                :metadata (make-auth-metadata auth-token))
             (multiple-value-bind (output error-output exit-code)
                 (uiop:run-program (list "bash" "-c" command)
                                   :output '(:string :stripped t)
                                   :error-output '(:string :stripped t)
                                   :ignore-error-status t)
               (let ((log (format nil "~A~@[~%~A~]" (or output "") error-output)))
                 (ag-grpc:grpc-call channel
                                    "/cave.runner.RunnerService/AppendTaskLog"
                                    (make-instance 'cave::append-task-log-request
                                                   :run-id run-id :chunk log)
                                    :response-type 'cave::append-task-log-response
                                    :metadata (make-auth-metadata auth-token)))
               (let ((status (if (zerop exit-code) "success" "failure")))
                 (format t "  Result: ~A (exit ~A)~%" status exit-code)
                 (ag-grpc:grpc-call channel
                                    "/cave.runner.RunnerService/UpdateTaskStatus"
                                    (make-instance 'cave::update-task-status-request
                                                   :run-id run-id :status status)
                                    :response-type 'cave::update-task-status-response
                                    :metadata (make-auth-metadata auth-token))))))

         (execute-workflow-job (channel auth-token task)
           "Execute a workflow job: pull image, clone repo, run steps in container."
           (let* ((job-id (slot-value task 'cave::job-id))
                  (repo-owner (slot-value task 'cave::repo-owner))
                  (repo-name (slot-value task 'cave::repo-name))
                  (image (slot-value task 'cave::image))
                  (commit-sha (slot-value task 'cave::commit-sha))
                  (clone-url (slot-value task 'cave::clone-url))
                  (steps (slot-value task 'cave::steps))
                  (privileged (handler-case (slot-value task 'cave::privileged)
                                (error () nil)))
                  ;; CI secrets for this job: newline-joined KEY=VALUE, injected
                  ;; as container env and masked in streamed logs.
                  (secrets-env (handler-case (slot-value task 'cave::secrets-env)
                                 (error () "")))
                  (secret-pairs (when (and secrets-env (plusp (length secrets-env)))
                                  (remove-if #'uiop:emptyp
                                             (uiop:split-string secrets-env
                                                                :separator '(#\Newline)))))
                  (secret-values (loop for p in secret-pairs
                                       for eq = (position #\= p)
                                       when (and eq (< (1+ eq) (length p)))
                                       collect (subseq p (1+ eq))))
                  ;; Reusable per-job caches: newline-joined VOLUME<TAB>PATH.
                  ;; Each is a repo-scoped persistent podman named volume mounted
                  ;; at the declared in-container path, so deps/build artifacts
                  ;; (npm, cargo, go-build, ~/.cache/common-lisp, …) survive
                  ;; across runs. Volume auto-creates on first `podman create -v`.
                  (cache-mounts (handler-case (slot-value task 'cave::cache-mounts)
                                  (error () "")))
                  (cache-pairs (when (and cache-mounts (plusp (length cache-mounts)))
                                 (loop for line in (uiop:split-string
                                                    cache-mounts :separator '(#\Newline))
                                       for tab = (position #\Tab line)
                                       when (and tab (plusp tab) (< (1+ tab) (length line)))
                                         collect (cons (subseq line 0 tab)
                                                       (subseq line (1+ tab))))))
                  ;; Base dir for per-job working trees. In production this is a
                  ;; NATIVE (ext4/xfs) host volume mounted into the rootless
                  ;; cave-runner, NOT the container's fuse-overlayfs /tmp: an
                  ;; SBCL/Go build reading source from a fuse-overlayfs /workspace
                  ;; crawls and wedges the job (issue #9). Override the location
                  ;; with CAVE_RUNNER_WORKDIR.
                  (workdir-base (let ((e (uiop:getenv "CAVE_RUNNER_WORKDIR")))
                                  (if (and e (plusp (length e)))
                                      (string-right-trim "/" e)
                                      "/var/lib/cave-runner/work")))
                  (workdir (format nil "~A/cave-job-~A" workdir-base job-id))
                  ;; Runtime dir for the GitHub-Actions file-command protocol
                  ;; ($GITHUB_OUTPUT/$GITHUB_ENV/$GITHUB_PATH/$GITHUB_STEP_SUMMARY).
                  ;; Bind-mounted at /__cave_rt so the runner reads each step's
                  ;; files back from the host side after the step runs.
                  (gh-dir (format nil "~A-rt" workdir))
                  ;; Static job env: GITHUB_*/CAVE_* context + workflow/job env,
                  ;; plus RUNNER_*; injected as container env at create time.
                  (context-env (handler-case (slot-value task 'cave::context-env)
                                 (error () "")))
                  ;; strategy.matrix combo for ${{ matrix.* }} (JSON object).
                  (matrix-map (handler-case
                                  (let ((mj (slot-value task 'cave::matrix-json)))
                                    (when (and (stringp mj) (plusp (length mj)))
                                      (com.inuoe.jzon:parse mj)))
                                (error () nil)))
                  ;; needs.<job>.outputs/result for ${{ needs.* }} (JSON object).
                  (needs-map (handler-case
                                 (let ((nj (slot-value task 'cave::needs-json)))
                                   (when (and (stringp nj) (plusp (length nj)))
                                     (com.inuoe.jzon:parse nj)))
                               (error () nil)))
                  ;; Job-level outputs: NAME=<expr> to resolve after the steps run.
                  (output-defs (handler-case (slot-value task 'cave::output-defs)
                                 (error () "")))
                  ;; Per-job timeout (seconds). 0/absent = no job deadline (the
                  ;; stale-job reaper is the global backstop). When set, the whole
                  ;; step sequence is bounded by JOB-DEADLINE, so a job that wedges
                  ;; with no per-step timeout still fails at its declared budget
                  ;; instead of running to the 120-min reaper cap. (issue #10)
                  (job-timeout (let ((ts (handler-case (slot-value task 'cave::timeout-seconds)
                                           (error () 0))))
                                 (when (and (integerp ts) (plusp ts)) ts)))
                  (job-deadline (when job-timeout (+ (get-universal-time) job-timeout)))
                  (job-env-pairs (%cave-mirror-pairs
                                  (append (%kv-lines->pairs context-env)
                                          (list "RUNNER_OS=Linux"
                                                (format nil "RUNNER_ARCH=~A" (%runner-arch))
                                                "RUNNER_TEMP=/tmp"
                                                "RUNNER_TOOL_CACHE=/opt/hostedtoolcache"))))
                  ;; Carried forward across steps via $GITHUB_ENV / $GITHUB_PATH.
                  (acc-env nil)
                  (acc-path nil)
                  ;; Resolved job-level outputs (JSON), reported with final status.
                  (resolved-outputs-json "")
                  ;; Log masks: CI secrets + anything a step emits via ::add-mask::.
                  (masks (copy-list secret-values))
                  (container-name (format nil "cave-job-~A" job-id))
                  ;; Persisted across jobs so Lisp builds reuse compiled FASLs
                  ;; instead of recompiling the whole ocicl tree every run (the
                  ;; recompile is what drives the memory spike that wedges the
                  ;; build — see issue #9). Harmless for non-Lisp jobs.
                  (fasl-cache "/var/cache/cave-runner/common-lisp")
                  ;; Object store root for actions/cache + artifacts (local dir
                  ;; backend; overridden by CAVE_RUNNER_CACHE_REMOTE -> rclone/S3).
                  ;; Objects: cache/<owner>/<repo>/... and artifacts/<run-id>/...
                  (store-root (format nil "~A/store"
                                      (or (uiop:getenv "CAVE_RUNNER_CACHE")
                                          "/var/cache/cave-runner")))
                  (overall-success t))
             (format t "~&Workflow job #~A: ~A/~A [~A] (~A steps)~%"
                     job-id repo-owner repo-name image (length steps))
             (when cache-pairs
               (format t "  Caches: ~{~A~^, ~}~%" (mapcar #'cdr cache-pairs)))
             ;; Report job running
             (ag-grpc:grpc-call channel
                                "/cave.runner.RunnerService/UpdateTaskStatus"
                                (make-instance 'cave::update-task-status-request
                                               :run-id 0
                                               :status (format nil "job:~A:running" job-id))
                                :response-type 'cave::update-task-status-response
                                :metadata (make-auth-metadata auth-token))
             (unwind-protect
              (progn
                ;; Ensure the (native-volume) workdir base exists before cloning.
                (ignore-errors
                 (ensure-directories-exist (concatenate 'string workdir-base "/")))
                ;; Pull image
                (format t "  Pulling ~A...~%" image)
                (multiple-value-bind (_out _err exit)
                    (uiop:run-program (list "podman" "pull" image)
                                      :output '(:string :stripped t)
                                      :error-output '(:string :stripped t)
                                      :ignore-error-status t)
                  (declare (ignore _out _err))
                  (unless (zerop exit)
                    (format *error-output* "  Failed to pull image ~A~%" image)
                    (setf overall-success nil)))
                ;; GitHub model: the workspace starts EMPTY. A step must run
                ;; `uses: actions/checkout` to populate it — cave no longer
                ;; auto-clones the repo.
                (when overall-success
                  (handler-case
                      (ensure-directories-exist (concatenate 'string workdir "/"))
                    (error (e)
                      (format *error-output* "  Failed to create workspace: ~A~%" e)
                      (setf overall-success nil))))
                ;; Create a long-lived container for all steps
                (when overall-success
                  (format t "  Creating container ~A...~%" container-name)
                    ;; Remove any leftover container from a previous run
                    (uiop:run-program (list "podman" "rm" "-f" container-name)
                                      :output :string :error-output :string
                                      :ignore-error-status t)
                    ;; Make sure the shared FASL cache dir + GH runtime dir exist.
                    (ignore-errors
                     (ensure-directories-exist (concatenate 'string fasl-cache "/")))
                    (ignore-errors
                     (ensure-directories-exist (concatenate 'string store-root "/")))
                    (ignore-errors
                     (ensure-directories-exist (concatenate 'string gh-dir "/")))
                    (multiple-value-bind (_out err exit)
                        (uiop:run-program
                         (append
                          (list "podman" "create" "--name" container-name)
                          ;; A job opts in to privileged mode with `privileged:
                          ;; true` in its workflow YAML.  Privileged jobs can
                          ;; launch nested containers (podman/docker-in-podman)
                          ;; and register QEMU binfmt for foreign-arch testing.
                          ;; Off by default so untrusted repos can't escalate.
                          (when privileged (list "--privileged"))
                          ;; Inject CI secrets as environment variables.
                          (loop for p in secret-pairs append (list "-e" p))
                          ;; Inject the static job env: GITHUB_*/CAVE_* context,
                          ;; RUNNER_*, CI, and the merged workflow/job `env:`.
                          (loop for p in job-env-pairs append (list "-e" p))
                          ;; Mount per-job reusable caches (repo-scoped named
                          ;; volumes). :U chowns to the container user so the
                          ;; build can write; the volume auto-creates if absent.
                          (loop for (vol . path) in cache-pairs
                                append (list "-v" (format nil "~A:~A:U" vol path)))
                          (list
                           ;; :Z relabels the bind mount with a private SELinux
                           ;; label so the container can write to it on
                           ;; enforcing hosts (Fedora/RHEL).
                           "-v" (format nil "~A:/workspace:Z" workdir)
                           ;; Shared (:z) so successive job containers reuse the
                           ;; FASL cache without relabeling the whole (growing)
                           ;; tree each run.
                           "-v" (format nil "~A:/root/.cache/common-lisp:z" fasl-cache)
                           ;; GitHub-Actions file-command runtime dir. Also the
                           ;; staging area for actions/cache tarballs (the runner
                           ;; moves them to the keyed store host-side).
                           "-v" (format nil "~A:/__cave_rt:Z" gh-dir)
                           "-w" "/workspace"
                           image "sleep" "infinity"))
                         :output '(:string :stripped t)
                         :error-output '(:string :stripped t)
                         :ignore-error-status t)
                      (declare (ignore _out))
                      (unless (zerop exit)
                        (format *error-output* "  Failed to create container: ~A~%" err)
                        (setf overall-success nil)))
                    (when overall-success
                      (uiop:run-program (list "podman" "start" container-name)
                                        :output :string :error-output :string
                                        :ignore-error-status t))
                    ;; Run each step. job-failed tracks a non-continue-on-error
                    ;; failure; later steps still run when their if: says so
                    ;; (success() default, or always()/failure()).
                    (when overall-success
                     (let ((steps-table (make-hash-table :test 'equal))
                           (job-failed nil)
                           ;; Set when the job timeout fires (see JOB-DEADLINE).
                           (job-timed-out nil)
                           ;; Action post-thunks (e.g. cache save), run at job end.
                           (post-actions nil))
                      (dolist (step steps)
                        ;; Job-level deadline spent between steps: fail the job now.
                        ;; Remaining steps see job-failed and are skipped by their
                        ;; should-run check (so they're marked skipped, not run).
                        (when (and job-deadline (not job-timed-out)
                                   (>= (get-universal-time) job-deadline))
                          (setf job-timed-out t job-failed t)
                          (format *error-output*
                                  "  Job timed out after ~As — skipping remaining steps~%"
                                  job-timeout))
                        (let* ((step-id (slot-value step 'cave::step-id))
                               (step-name (slot-value step 'cave::name))
                               (step-cmd (or (slot-value step 'cave::command) ""))
                               (step-uses (handler-case (slot-value step 'cave::uses) (error () "")))
                               (with-pairs (%kv-lines->pairs
                                            (handler-case (slot-value step 'cave::with-inputs)
                                              (error () ""))))
                               (uses-step (and step-uses (plusp (length step-uses))))
                               (step-timeout (let ((ts (slot-value step 'cave::timeout-seconds)))
                                               (when (and ts (plusp ts)) ts)))
                               (step-env-pairs (%kv-lines->pairs
                                                (handler-case (slot-value step 'cave::env)
                                                  (error () ""))))
                               (step-continue-on-error (slot-value step 'cave::continue-on-error))
                               (id-name (handler-case (slot-value step 'cave::id-name) (error () "")))
                               (if-cond (handler-case (slot-value step 'cave::if-cond) (error () "")))
                               (step-outputs (make-hash-table :test 'equal))
                               ;; ${{ }} context with the accumulated steps.* + job status.
                               (gha-ctx (%gha-context (append job-env-pairs acc-env step-env-pairs)
                                                      secret-pairs steps-table
                                                      (if job-failed "failure" "success")
                                                      matrix-map needs-map workdir))
                               (should-run (if (and if-cond (plusp (length if-cond)))
                                               (gha-expression-true-p if-cond gha-ctx)
                                               (not job-failed))))
                          (if (not should-run)
                              ;; --- skipped (if: false, or a prior step failed) ---
                              (progn
                                (format t "  Step ~A: skipped~%"
                                        (if (uiop:emptyp step-name) "(unnamed)" step-name))
                                (ag-grpc:grpc-call channel
                                                   "/cave.runner.RunnerService/UpdateStepStatus"
                                                   (make-instance 'cave::update-step-status-request
                                                                  :step-id step-id :status "skipped"
                                                                  :exit-code 0)
                                                   :response-type 'cave::update-step-status-response
                                                   :metadata (make-auth-metadata auth-token))
                                (when (plusp (length id-name))
                                  (setf (gethash id-name steps-table)
                                        (%mk-step-result step-outputs "skipped" "skipped"))))
                              ;; --- run ---
                              (progn
                                (format t "  Step ~A: ~A~%"
                                        (if (uiop:emptyp step-name) "(unnamed)" step-name)
                                        (if uses-step
                                            (format nil "uses ~A" step-uses)
                                            (subseq step-cmd 0 (min 60 (length step-cmd)))))
                                (ag-grpc:grpc-call channel
                                                   "/cave.runner.RunnerService/UpdateStepStatus"
                                                   (make-instance 'cave::update-step-status-request
                                                                  :step-id step-id :status "running"
                                                                  :exit-code 0)
                                                   :response-type 'cave::update-step-status-response
                                                   :metadata (make-auth-metadata auth-token))
                                (let ((exit-code
                                       (if uses-step
                                           ;; --- uses: action — orchestrated here, effected in-container ---
                                           (%run-action step-uses with-pairs gha-ctx
                                                        (list :workdir workdir :gh-dir gh-dir
                                                              :clone-url clone-url
                                                              :commit-sha commit-sha
                                                              :job-token ""
                                                              :store (%object-store-descriptor store-root)
                                                              :repo-owner repo-owner :repo-name repo-name
                                                              :run-id
                                                              (let ((p (find-if (lambda (s)
                                                                                  (uiop:string-prefix-p "GITHUB_RUN_ID=" s))
                                                                                job-env-pairs)))
                                                                (if p (subseq p 14) "0"))
                                                              ;; Register an uploaded artifact with the server.
                                                              :register-artifact
                                                              (lambda (aname object-path size)
                                                                (let ((rid (let ((p (find-if (lambda (s)
                                                                                               (uiop:string-prefix-p "GITHUB_RUN_ID=" s))
                                                                                             job-env-pairs)))
                                                                             (if p (or (parse-integer (subseq p 14) :junk-allowed t) 0) 0))))
                                                                  (ignore-errors
                                                                   (ag-grpc:grpc-call channel
                                                                    "/cave.runner.RunnerService/RegisterArtifact"
                                                                    (make-instance 'cave::register-artifact-request
                                                                                   :run-id rid :job-id job-id
                                                                                   :name aname :object-path object-path
                                                                                   :size-bytes size)
                                                                    :response-type 'cave::register-artifact-response
                                                                    :metadata (make-auth-metadata auth-token)))))
                                                              :ref (handler-case (slot-value task 'cave::ref)
                                                                     (error () "")))
                                                        step-outputs channel auth-token step-id masks
                                                        container-name
                                                        (%action-base-from-clone-url clone-url)
                                                        (lambda (thunk) (push thunk post-actions)))
                                        (block step-run
                                          (handler-case
                                            (let* ((log-file (format nil "/tmp/cave-step-~A.log" step-id))
                                                   (rt-c "/__cave_rt")
                                                   (f-out (format nil "~A/out-~A" rt-c step-id))
                                                   (f-env (format nil "~A/env-~A" rt-c step-id))
                                                   (f-path (format nil "~A/path-~A" rt-c step-id))
                                                   (f-sum (format nil "~A/sum-~A" rt-c step-id))
                                                   (h-out (format nil "~A/out-~A" gh-dir step-id))
                                                   (h-env (format nil "~A/env-~A" gh-dir step-id))
                                                   (h-path (format nil "~A/path-~A" gh-dir step-id))
                                                   (h-sum (format nil "~A/sum-~A" gh-dir step-id))
                                                   (proto-pairs
                                                     (%cave-mirror-pairs
                                                      (list (format nil "GITHUB_OUTPUT=~A" f-out)
                                                            (format nil "GITHUB_ENV=~A" f-env)
                                                            (format nil "GITHUB_PATH=~A" f-path)
                                                            (format nil "GITHUB_STEP_SUMMARY=~A" f-sum))))
                                                   ;; Interpolate ${{ }} in command + step env.
                                                   (icmd (interpolate-gha step-cmd gha-ctx))
                                                   (istep-env
                                                     (mapcar (lambda (p)
                                                               (let ((eq (position #\= p)))
                                                                 (if eq
                                                                     (concatenate 'string (subseq p 0 (1+ eq))
                                                                                  (interpolate-gha (subseq p (1+ eq)) gha-ctx))
                                                                     p)))
                                                             step-env-pairs))
                                                   (exec-pairs (append acc-env istep-env proto-pairs))
                                                   (script (if acc-path
                                                               (format nil "export PATH=~A:\"$PATH\"~%~A"
                                                                       (format nil "~{~A~^:~}" acc-path) icmd)
                                                               icmd))
                                                   (exec-cmd
                                                     (append (list "podman" "exec")
                                                             (loop for p in exec-pairs append (list "-e" p))
                                                             (list container-name "bash" "-c" script)))
                                                   (process
                                                     (progn
                                                       (dolist (hf (list h-out h-env h-path h-sum))
                                                         (ignore-errors
                                                          (with-open-file (s hf :direction :output
                                                                              :if-exists :supersede
                                                                              :if-does-not-exist :create))))
                                                       (uiop:launch-program exec-cmd
                                                                            :output log-file
                                                                            :error-output log-file)))
                                                   (sent 0))
                                              (flet ((send-log ()
                                                       (handler-case
                                                           (when (probe-file log-file)
                                                             (let* ((content (uiop:read-file-string log-file))
                                                                    (len (length content)))
                                                               (when (> len sent)
                                                                 (let* ((new-start sent)
                                                                        (raw (subseq content new-start
                                                                                     (min len (+ new-start 65536)))))
                                                                   (dolist (line (uiop:split-string raw :separator '(#\Newline)))
                                                                     (let ((m (search "::add-mask::" line)))
                                                                       (when m
                                                                         (let ((v (string-trim '(#\Return #\Space)
                                                                                               (subseq line (+ m 12)))))
                                                                           (when (plusp (length v)) (pushnew v masks :test #'equal))))))
                                                                   (let ((chunk (%mask-secrets raw masks)))
                                                                     (setf sent (+ new-start (length raw)))
                                                                     (ag-grpc:grpc-call channel
                                                                      "/cave.runner.RunnerService/AppendStepLog"
                                                                      (make-instance 'cave::append-step-log-request
                                                                                     :step-id step-id :chunk chunk)
                                                                      :response-type 'cave::append-step-log-response
                                                                      :metadata (make-auth-metadata auth-token)))))))
                                                         (error (e)
                                                           (format *error-output* "  Log send error: ~A~%" e)))))
                                                (let* ((step-deadline (when step-timeout
                                                                        (+ (get-universal-time) step-timeout)))
                                                       ;; Kill at whichever of the step or job deadline is
                                                       ;; sooner. A job-deadline kill also fails the job and
                                                       ;; stops later steps, overriding continue-on-error.
                                                       (deadline (cond ((and step-deadline job-deadline)
                                                                        (min step-deadline job-deadline))
                                                                       (t (or step-deadline job-deadline)))))
                                                  (loop while (uiop:process-alive-p process)
                                                        do (when (and deadline (>= (get-universal-time) deadline))
                                                             (if (and job-deadline (>= (get-universal-time) job-deadline))
                                                                 (progn
                                                                   (setf job-timed-out t)
                                                                   (format *error-output* "    Job timed out after ~As~%" job-timeout))
                                                                 (format *error-output* "    Step timed out after ~As~%" step-timeout))
                                                             (uiop:terminate-process process :urgent t)
                                                             (uiop:wait-process process)
                                                             (send-log)
                                                             (ignore-errors (delete-file log-file))
                                                             (return-from step-run 124))
                                                           (send-log) (sleep 2))
                                                  (send-log)
                                                  (ignore-errors (delete-file log-file))
                                                  (let ((code (uiop:wait-process process)))
                                                    (let ((envc (ignore-errors (uiop:read-file-string h-env))))
                                                      (when (and envc (plusp (length envc)))
                                                        (setf acc-env (append acc-env (%kv-lines->pairs envc)))))
                                                    (let ((pathc (ignore-errors (uiop:read-file-string h-path))))
                                                      (when pathc
                                                        (dolist (ln (uiop:split-string pathc :separator '(#\Newline)))
                                                          (let ((p (string-trim '(#\Return #\Space) ln)))
                                                            (when (plusp (length p)) (push p acc-path))))))
                                                    (let ((sumc (ignore-errors (uiop:read-file-string h-sum))))
                                                      (when (and sumc (plusp (length sumc)))
                                                        (ignore-errors
                                                         (ag-grpc:grpc-call channel
                                                          "/cave.runner.RunnerService/AppendStepLog"
                                                          (make-instance 'cave::append-step-log-request
                                                                         :step-id step-id
                                                                         :chunk (format nil "~%::group::Step summary::~%~A~%"
                                                                                        (%mask-secrets sumc masks)))
                                                          :response-type 'cave::append-step-log-response
                                                          :metadata (make-auth-metadata auth-token)))))
                                                    ;; $GITHUB_OUTPUT -> steps.<id>.outputs
                                                    (let ((outc (ignore-errors (uiop:read-file-string h-out))))
                                                      (when (and outc (plusp (length outc)))
                                                        (dolist (kv (%kv-lines->pairs outc))
                                                          (let ((eq (position #\= kv)))
                                                            (when eq
                                                              (setf (gethash (subseq kv 0 eq) step-outputs)
                                                                    (subseq kv (1+ eq))))))))
                                                    code))))
                                            (error () 1))))))
                                  (let* ((ok (zerop exit-code))
                                         (outcome (if ok "success" "failure"))
                                         (conclusion (if (or ok step-continue-on-error) "success" "failure")))
                                    (format t "    ~A (exit ~A)~%" outcome exit-code)
                                    (ag-grpc:grpc-call channel
                                                       "/cave.runner.RunnerService/UpdateStepStatus"
                                                       (make-instance 'cave::update-step-status-request
                                                                      :step-id step-id :status outcome
                                                                      :exit-code exit-code)
                                                       :response-type 'cave::update-step-status-response
                                                       :metadata (make-auth-metadata auth-token))
                                    (when (plusp (length id-name))
                                      (setf (gethash id-name steps-table)
                                            (%mk-step-result step-outputs outcome conclusion)))
                                    (when (and (not ok) step-continue-on-error)
                                      (format t "    continue-on-error: proceeding despite failure~%"))
                                    (when (and (not ok) (not step-continue-on-error))
                                      (setf job-failed t))
                                    ;; A job-timeout kill fails the job even when
                                    ;; the step has continue-on-error.
                                    (when job-timed-out (setf job-failed t))))))))
                      (when job-failed (setf overall-success nil))
                      ;; Run action post-thunks (e.g. cache save) in reverse step
                      ;; order — like GitHub post steps; failures are warnings.
                      (dolist (thunk post-actions)
                        (handler-case (funcall thunk)
                          (error (e)
                            (format *error-output* "  post-action error: ~A~%" e))))
                      ;; Resolve job-level outputs: against the final steps context.
                      (when (plusp (length output-defs))
                        (let ((final-ctx (%gha-context (append job-env-pairs acc-env)
                                                       secret-pairs steps-table
                                                       (if job-failed "failure" "success")
                                                       matrix-map needs-map workdir))
                              (outs nil))
                          (dolist (kv (%kv-lines->pairs output-defs))
                            (let ((eqpos (position #\= kv)))
                              (when eqpos
                                (push (cons (subseq kv 0 eqpos)
                                            (interpolate-gha (subseq kv (1+ eqpos)) final-ctx))
                                      outs))))
                          (setf resolved-outputs-json (%outputs->json (nreverse outs))))))))
                    ;; Stop and remove container
                    (uiop:run-program (list "podman" "rm" "-f" container-name)
                                      :output :string :error-output :string
                                      :ignore-error-status t)) ;; close when(outer)
              ;; Cleanup workdir + GH runtime dir
              (uiop:run-program (list "rm" "-rf" workdir gh-dir)
                                :ignore-error-status t))
             ;; Report overall job status
             (let ((status (if overall-success "success" "failure")))
               (format t "  Job result: ~A~%" status)
               (ag-grpc:grpc-call channel
                                  "/cave.runner.RunnerService/UpdateTaskStatus"
                                  (make-instance 'cave::update-task-status-request
                                                 :run-id 0
                                                 :status (format nil "job:~A:~A" job-id status))
                                  :response-type 'cave::update-task-status-response
                                  :metadata (make-auth-metadata auth-token)))))

         (run-watch-loop (channel auth-token)
           "Open a WatchTasks server stream and process tasks as they arrive."
           (format t "~&Watching for tasks...~%")
           (let ((stream (ag-grpc:call-server-stream
                          channel
                          "/cave.runner.RunnerService/WatchTasks"
                          (make-instance 'cave::watch-tasks-request
                                         :runner-labels runner-labels)
                          :response-type 'cave::task-event
                          :metadata (make-auth-metadata auth-token))))
             (loop
               (let ((task (ag-grpc:stream-receive-message stream)))
                 (unless task (return)) ; stream ended
                 (handler-case
                     (progn
                       (execute-task channel auth-token task)
                       (when ephemeral
                         (format t "~&Ephemeral runner — exiting.~%")
                         (uiop:quit 0)))
                   (error (e)
                     (format *error-output* "~&Task execution error: ~A~%" e))))))))

      ;; Register with the server (no timeout — builds can take minutes)
      (let ((channel (ag-grpc:make-channel host port :timeout nil :tls tls-p)))
        (handler-case
            (progn
              (format t "~&Registering...~%")
              (let* ((req (make-instance 'cave::register-runner-request
                                         :name name
                                         :runner-labels runner-labels
                                         :ephemeral ephemeral
                                         :registration-token token))
                     (resp (ag-grpc:grpc-call channel
                                              "/cave.runner.RunnerService/RegisterRunner"
                                              req :response-type 'cave::register-runner-response)))
                (let ((auth-token (slot-value resp 'cave::auth-token))
                      (runner-id (slot-value resp 'cave::runner-id)))
                  (format t "  Registered as runner #~A~%" runner-id)
                  ;; Main loop: watch for tasks with auto-reconnect
                  (loop
                    (handler-case
                        (let ((ch (ag-grpc:make-channel host port
                                    :timeout nil
                                    :tls tls-p
                                    :keepalive (ag-grpc:make-keepalive-config
                                                :ping-interval 15
                                                :ping-timeout 5
                                                :permit-without-calls t))))
                          (run-watch-loop ch auth-token))
                      (error (e)
                        (format *error-output* "~&Stream disconnected: ~A~%" e)
                        (format *error-output* "  Reconnecting in 5s...~%")
                        (sleep 5)))))))
          (error (e)
            (format *error-output* "~&Runner error: ~A~%" e)
            (uiop:quit 1)))))))

;;; --- POST-RECEIVE subcommand ---

