(in-package #:cave)

(defun handle-git-proxy (cmd)
  "Bridge SSH stdin/stdout to Chamber gRPC bidirectional stream.
   Single-threaded: the main thread reads gRPC responses and forwards to stdout,
   a reader thread drives the HTTP/2 connection, and a sender thread reads stdin."
  (let* ((config-path (clingon:getopt cmd :config))
         (git-command (clingon:getopt cmd :command))
         (repo-path (clingon:getopt cmd :repo))
         (parts (uiop:split-string repo-path :separator '(#\/)))
         (owner (first parts))
         (repo-name (second parts)))
    (unless (and owner repo-name)
      (format *error-output* "cave: invalid repo path~%")
      (uiop:quit 1))
    (load-config config-path)
    ;; Create binary stdin/stdout streams
    (let ((bin-in (sb-sys:make-fd-stream 0 :input t
                                          :element-type '(unsigned-byte 8)
                                          :buffering :full
                                          :name "git-proxy-stdin"))
          (bin-out (sb-sys:make-fd-stream 1 :output t
                                           :element-type '(unsigned-byte 8)
                                           :buffering :full
                                           :name "git-proxy-stdout")))
      ;; Connect to the correct Chamber node
      (let* ((is-write (equal git-command "git-receive-pack"))
             (nodes-config (config-value :chamber-nodes))
             (multi-p (and nodes-config (> (length nodes-config) 1)))
             (channel
               (if multi-p
                   ;; Multi-chamber: look up node assignment from DB
                   (progn
                     (connect-db)
                     (unwind-protect
                         (let* ((repo (find-repo owner repo-name))
                                (repo-id (when repo (getf repo :id)))
                                (node (when repo-id
                                        (if is-write
                                            (repo-primary-node repo-id)
                                            (let ((healthy (repo-healthy-nodes repo-id)))
                                              (if healthy
                                                  (nth (random (length healthy)) healthy)
                                                  (repo-primary-node repo-id))))))
                                (addr (when node (getf node :address)))
                                (colon (when addr (position #\: addr :from-end t)))
                                (host (if colon (subseq addr 0 colon) (or addr "127.0.0.1")))
                                (port (if colon
                                          (parse-integer (subseq addr (1+ colon)) :junk-allowed t)
                                          (config-value :chamber-port 9444))))
                           (ag-grpc:make-channel host (or port 9444) :timeout nil))
                       (disconnect-db)))
                   ;; Single-chamber: use config URL
                   (let* ((url (config-value :chamber-url))
                          (host (if url
                                    (let ((pos (search "://" url)))
                                      (if pos (subseq url (+ pos 3)) url))
                                    "127.0.0.1"))
                          (port-str (let ((colon (position #\: host :from-end t)))
                                      (when colon (subseq host (1+ colon)))))
                          (host-only (let ((colon (position #\: host :from-end t)))
                                       (if colon (subseq host 0 colon) host)))
                          (port (if port-str
                                    (parse-integer port-str :junk-allowed t)
                                    (config-value :chamber-port 9444))))
                     (ag-grpc:make-channel host-only port :timeout nil))))
             (conn (ag-grpc::channel-connection channel))
             (method (if (equal git-command "git-receive-pack")
                         "/cave.chamber.Chamber/ReceivePack"
                         "/cave.chamber.Chamber/UploadPack"))
             (stream (ag-grpc:call-bidirectional-streaming
                      channel method
                      :response-type 'cave::pack-data)))
        ;; Start a dedicated reader thread for the HTTP/2 connection.
        ;; This drives frame processing so stream-read-message and
        ;; stream-send (flow control) work from separate threads.
        (setf (ag-http2:connection-reader-thread-active-p conn) t)
        (let ((reader-done nil))
          (bt2:make-thread
           (lambda ()
             (handler-case
                 (loop while (not (member (ag-http2:connection-state conn)
                                          '(:closing :closed)))
                       do (ag-http2:connection-read-frame conn))
               (error () nil))
             (setf reader-done t)
             ;; Wake any waiters on flow control
             (bt2:condition-notify (ag-http2:connection-flow-control-cv conn)))
           :name "git-proxy-reader")
          (handler-case
              (progn
                ;; Send init message with repo info
                (ag-grpc:stream-send stream
                  (make-instance 'cave::pack-data
                                 :data (make-array 0 :element-type '(unsigned-byte 8))
                                 :owner owner :repo-name repo-name))
                ;; Stdin→gRPC sender thread
                (let ((sender-thread
                        (bt2:make-thread
                         (lambda ()
                           (let ((buf (make-array 32768 :element-type '(unsigned-byte 8))))
                             (handler-case
                                 (loop
                                   (let ((n (read-sequence buf bin-in)))
                                     (when (zerop n) (return))
                                     (ag-grpc:stream-send stream
                                       (make-instance 'cave::pack-data
                                                      :data (subseq buf 0 n)))))
                               (error () nil))
                             (handler-case (ag-grpc:stream-close-send stream)
                               (error () nil))))
                         :name "git-proxy-sender")))
                  ;; gRPC→stdout in main thread
                  (loop
                    (let ((msg (ag-grpc:stream-read-message stream)))
                      (unless msg (return))
                      (let ((data (slot-value msg 'cave::data)))
                        (when (and data (plusp (length data)))
                          (write-sequence data bin-out)
                          (force-output bin-out)))))
                  (bt2:join-thread sender-thread))
                (uiop:quit 0))
            (error (e)
              (format *error-output* "cave: proxy error: ~A~%" e)
              (uiop:quit 1))))))))

;;; --- UPDATE-KEYS subcommand ---

(defun make-update-keys-command ()
  (clingon:make-command
   :name "update-keys"
   :description "Regenerate the authorized_keys file from the database"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "output" :key :output
              :description "Path to write authorized_keys"
              :initial-value (namestring
                              (merge-pathnames ".ssh/authorized_keys"
                                               (user-homedir-pathname))))
             (clingon:make-option :string
              :long-name "cave-shell" :key :cave-shell
              :description "Path to cave-shell.sh"
              :initial-value "/usr/bin/cave-shell.sh"))
   :handler #'handle-update-keys
   :examples '(("Update authorized_keys:" .
                "cave-server update-keys --config /etc/cave.conf"))))

;;; --- RUN-CHECKS subcommand ---

(defun make-run-checks-command ()
  (clingon:make-command
   :name "run-checks"
   :description "Run server-side push checks for a repo (called by pre-receive hook)"
   :options (list
             (make-config-option)
             (clingon:make-option :string
              :long-name "repo" :key :repo :required t
              :description "Repo path as owner/name"))
   :handler #'handle-run-checks))

(defun %pushed-refs-from-stdin ()
  "Read pre-receive ref updates from stdin as a list of (old new ref) triples.
Reads stdin once (it can't be re-read); callers derive shas from the result."
  (let ((refs nil))
    (handler-case
        (loop for line = (read-line *standard-input* nil nil)
              while line
              do (let ((parts (uiop:split-string
                               (string-trim '(#\Space #\Tab #\Return) line)
                               :separator '(#\Space))))
                   (when (>= (length parts) 3)
                     (push (list (first parts) (second parts) (third parts)) refs))))
      (error () nil))
    (nreverse refs)))

(defun %unsigned-commits (bare-path old new)
  "SHAs among the pushed commits (OLD..NEW, or just NEW for a new branch) whose
commit object carries no signature."
  (let* ((zero (every (lambda (c) (char= c #\0)) old))
         (range (if zero new (format nil "~A..~A" old new))))
    (multiple-value-bind (out _e code)
        (uiop:run-program (list "git" "-C" (namestring bare-path) "rev-list"
                                (if zero "--max-count=50" "") range)
                          :output '(:string :stripped t) :error-output nil
                          :ignore-error-status t)
      (declare (ignore _e))
      (when (zerop code)
        (loop for sha in (remove-if #'uiop:emptyp
                                    (uiop:split-string out :separator '(#\Newline)))
              unless (cave::git-commit-signature-info bare-path sha)
                collect sha)))))

(defun %enforce-protected-branches (repo refs bare-path pusher-id)
  "Return a rejection reason string if any REF update violates a branch
protection rule, else NIL. Direct-push protection is bypassed by repo admins."
  (loop for (old new ref) in refs
        for branch = (when (uiop:string-prefix-p "refs/heads/" ref) (subseq ref 11))
        when branch
          do (let ((prot (branch-protection (getf repo :id) branch)))
               (when prot
                 (let ((deleting (every (lambda (c) (char= c #\0)) new)))
                   (cond
                     ;; Block direct pushes/deletes (admins may override).
                     ((and (getf prot :block-direct-push)
                           (not (and pusher-id
                                     (equal (repo-member-role (getf repo :id) pusher-id)
                                            "admin"))))
                      (return (format nil "branch '~A' is protected — open a pull request instead of pushing directly"
                                      branch)))
                     ;; Require signed commits on the protected branch.
                     ((and (getf prot :require-signed-commits) (not deleting))
                      (let ((unsigned (%unsigned-commits bare-path old new)))
                        (when unsigned
                          (return (format nil "branch '~A' requires signed commits — ~A unsigned commit~:P pushed"
                                          branch (length unsigned))))))))))))

(defun %git-head-sha (bare-path)
  "Resolve HEAD of a bare repo to a sha, or NIL (e.g. empty repo)."
  (let ((out (nth-value 0 (uiop:run-program
                           (list "git" "-C" (namestring bare-path) "rev-parse" "HEAD")
                           :output '(:string :stripped t) :ignore-error-status t))))
    (when (and out (plusp (length out))) out)))

(defun %make-check-workdir ()
  "Create a private temp dir for a check worktree. Returns its path (no trailing /)."
  (string-trim '(#\Newline #\Space)
               (nth-value 0 (uiop:run-program
                             (list "mktemp" "-d" "/tmp/cave-check-XXXXXX")
                             :output '(:string :stripped t)))))

(defun %extract-tree (bare-path sha dest-dir)
  "Extract the tree at SHA from the bare repo into DEST-DIR as plain files (no
   .git, so the check can't reach the repo or network through git). Returns T."
  (let ((tarfile (format nil "~A.tar" dest-dir)))
    (unwind-protect
         (and (zerop (nth-value 2 (uiop:run-program
                                   (list "git" "-C" (namestring bare-path)
                                         "archive" "--format=tar" "-o" tarfile sha)
                                   :ignore-error-status t
                                   :output nil :error-output nil)))
              (zerop (nth-value 2 (uiop:run-program
                                   (list "tar" "-xf" tarfile "-C" dest-dir)
                                   :ignore-error-status t
                                   :output nil :error-output nil))))
      (ignore-errors (delete-file tarfile)))))

(defvar *check-net-isolation* :unprobed
  "Cached result of probing whether `unshare -n` works in this environment.")

(defun %check-net-isolation-available-p ()
  "True when `unshare -n` succeeds here (needs root + an unmasked runtime).
   Probed once per process."
  (when (eq *check-net-isolation* :unprobed)
    (setf *check-net-isolation*
          (handler-case
              (zerop (nth-value 2 (uiop:run-program
                                   (list "unshare" "-n" "true")
                                   :ignore-error-status t
                                   :output nil :error-output nil)))
            (error () nil))))
  *check-net-isolation*)

(defun %sandbox-argv (command workdir sha repo-path timeout isolate-net mem-mb)
  "Build the argv to run a check COMMAND against the extracted WORKDIR under a
   sandbox: scrubbed environment, a wall-clock TIMEOUT, an optional per-process
   memory cap, and (when ISOLATE-NET) a network namespace. CWD is set to WORKDIR
   by the caller. We deliberately do NOT use `ulimit -u`: RLIMIT_NPROC is
   per-UID across the whole container, so capping it would starve the long-lived
   cave-server threads (running as the same uid) of forks. `timeout` bounds
   runaway/fork-bomb code instead."
  (let ((ulimit (if mem-mb
                    (format nil "ulimit -v ~A 2>/dev/null; " (* mem-mb 1024))
                    "")))
    ;; Landlock filesystem confinement (landrun) around the whole check. :exec t
    ;; grants the worktree --rwx so a check may run scripts it shipped (./build.sh).
    ;; landrun's network policy mirrors the check's: when isolate-net is on we also
    ;; deny TCP in landrun (belt to `unshare -n`); when the admin allows check
    ;; network (:checks-allow-network), landrun allows it too so the check works.
    (sandbox-wrap workdir
     (append
     (when isolate-net (list "unshare" "-n"))
     (list "env" "-i"
           "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
           (format nil "HOME=~A" workdir)
           "LANG=C.UTF-8"
           (format nil "CAVE_COMMIT=~A" sha)
           (format nil "CAVE_REPO=~A" repo-path))
     (list "timeout" "-k" "10" (format nil "~A" timeout))
     (list "bash" "-c" (format nil "~A~A" ulimit command)))
     :exec t :network (not isolate-net))))

(defun handle-run-checks (cmd)
  (let ((config-path (clingon:getopt cmd :config))
        (repo-path (clingon:getopt cmd :repo)))
    (load-config config-path)
    (handler-case (connect-db)
      (error (e)
        (format *error-output* "~&cave: cannot connect to database: ~A~%" e)
        (uiop:quit 1)))
    (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
           (owner (first parts))
           (name (second parts))
           (repo (find-repo owner name)))
      (unless repo
        (format *error-output* "~&cave: repo not found: ~A~%" repo-path)
        (disconnect-db)
        (uiop:quit 0)) ;; Don't block push for unknown repos
      (let ((checks (remove-if-not (lambda (c) (getf c :enabled))
                                   (list-check-configs (getf repo :id))))
            (disk-path (repo-disk-path owner name))
            ;; Read the ref updates once (stdin can't be re-read).
            (refs (%pushed-refs-from-stdin)))
        ;; Protected-branch enforcement runs first, even when no checks exist.
        (let ((violation
                (handler-case
                    (%enforce-protected-branches
                     repo refs disk-path
                     (let ((p (uiop:getenv "CAVE_PUSH_USER_ID")))
                       (when (and p (plusp (length p)))
                         (parse-integer p :junk-allowed t))))
                  (error () nil))))
          (when violation
            (format *error-output* "~&cave: push rejected — ~A~%" violation)
            (disconnect-db)
            (uiop:quit 1)))
        ;; Nothing else to enforce — accept.
        (when (null checks)
          (disconnect-db)
          (uiop:quit 0))
        (let* ((shas (or (loop for (o n r) in refs
                               unless (every (lambda (c) (char= c #\0)) n)
                                 collect n)
                         (let ((h (%git-head-sha disk-path))) (when h (list h)))))
               (want-isolation (not (config-value :checks-allow-network)))
               (isolation-ok (and want-isolation (%check-net-isolation-available-p)))
               (mem (config-value :checks-memory-mb))  ; nil = no memory cap
               (default-timeout (config-value :checks-timeout-seconds 120))
               (failed nil))
          ;; Network isolation requested but the runtime forbids `unshare -n`
          ;; (the default cave container lacks the capability). Fail closed when
          ;; an admin demands isolation; otherwise warn and run without it.
          (when (and want-isolation (not isolation-ok))
            (if (config-value :checks-require-network-isolation)
                (progn
                  (format *error-output*
                          "~&cave: push rejected — network isolation required but unavailable ~
                           (the cave container lacks unshare; grant the capability or run ~
                           checks on a runner)~%")
                  (disconnect-db)
                  (uiop:quit 1))
                (format *error-output*
                        "~&cave: warning — network isolation unavailable; running checks without it~%")))
          (dolist (sha shas)
            (let ((wt (%make-check-workdir)))
              (unwind-protect
                   (if (not (%extract-tree disk-path sha wt))
                       (progn
                         (format *error-output*
                                 "~&cave: could not extract tree ~A for checks~%" sha)
                         (push "(tree extract)" failed))
                       (dolist (chk checks)
                         (let* ((to (let ((t0 (getf chk :timeout-seconds)))
                                      (if (and t0 (plusp t0)) t0 default-timeout)))
                                (argv (%sandbox-argv (getf chk :command) wt sha repo-path
                                                     to isolation-ok mem)))
                           (format t "~&Running check: ~A (~A)~%"
                                   (getf chk :name) (subseq sha 0 (min 7 (length sha))))
                           (handler-case
                               (multiple-value-bind (output error-output exit-code)
                                   (uiop:run-program
                                    argv
                                    :output '(:string :stripped t)
                                    :error-output '(:string :stripped t)
                                    :ignore-error-status t
                                    :directory (namestring wt))
                                 (cond
                                   ((zerop exit-code)
                                    (format t "  ✓ ~A passed~%" (getf chk :name)))
                                   ((= exit-code 124)
                                    (format t "  ✗ ~A timed out after ~As~%"
                                            (getf chk :name) to)
                                    (push (getf chk :name) failed))
                                   (t
                                    (format t "  ✗ ~A failed (exit ~A)~%"
                                            (getf chk :name) exit-code)
                                    (when (and output (plusp (length output)))
                                      (format t "    ~A~%" output))
                                    (when (and error-output (plusp (length error-output)))
                                      (format t "    ~A~%" error-output))
                                    (push (getf chk :name) failed))))
                             (error (e)
                               (format t "  ✗ ~A error: ~A~%" (getf chk :name) e)
                               (push (getf chk :name) failed))))))
                (ignore-errors
                 (uiop:run-program (list "rm" "-rf" wt) :ignore-error-status t)))))
          (disconnect-db)
          (when failed
            (format *error-output*
                    "~&cave: push rejected — ~A check~:P failed: ~{~A~^, ~}~%"
                    (length failed) (nreverse failed))
            (uiop:quit 1)))))))

;;; --- RUNNER subcommand ---

(defun %string-replace-all (string old new)
  "Replace every occurrence of OLD with NEW in STRING."
  (if (or (null old) (zerop (length old)))
      string
      (with-output-to-string (out)
        (loop with olen = (length old)
              for start = 0 then (+ pos olen)
              for pos = (search old string :start2 start)
              do (write-string string out :start start :end (or pos (length string)))
              while pos do (write-string new out)))))

(defun %mask-secrets (text values)
  "Replace each secret VALUE in TEXT with *** so secrets never reach the log UI."
  (let ((result text))
    (dolist (v values result)
      (when (and v (>= (length v) 4))   ; avoid masking trivially short values
        (setf result (%string-replace-all result v "***"))))))

(defun %kv-lines->pairs (s)
  "Newline KEY=VALUE blob -> list of non-empty \"KEY=VALUE\" strings."
  (when (and s (stringp s) (plusp (length s)))
    (remove-if (lambda (l) (or (uiop:emptyp l) (null (position #\= l))))
               (mapcar (lambda (l) (string-trim '(#\Return #\Newline) l))
                       (uiop:split-string s :separator '(#\Newline))))))

(defun %cave-mirror-pairs (pairs)
  "For each GITHUB_X=VALUE in PAIRS, also emit a CAVE_X=VALUE twin (so every
GitHub-Actions variable has a cave-native alias). Non-GITHUB_ keys pass through."
  (append pairs
          (loop for p in pairs
                when (uiop:string-prefix-p "GITHUB_" p)
                  collect (concatenate 'string "CAVE_" (subseq p 7)))))

(defun %pairs->map (pairs &key strip downcase)
  "Hash-table (string test) from KEY=VALUE PAIRS, optionally stripping a key
prefix and downcasing the key."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p pairs h)
      (let ((eq (position #\= p)))
        (when eq
          (let ((k (subseq p 0 eq)) (v (subseq p (1+ eq))))
            (when (and strip (uiop:string-prefix-p strip k)) (setf k (subseq k (length strip))))
            (when downcase (setf k (string-downcase k)))
            (setf (gethash k h) v)))))))

(defun %gha-context (env-pairs secret-pairs steps-map job-status
                     &optional matrix-map needs-map workspace-root)
  "Assemble the ${{ }} evaluation context for the runner from the data it has:
github.* / runner.* (from the GITHUB_*/RUNNER_* env), env.*, secrets.*, the
accumulated steps.* outputs, job.status, and the strategy matrix.* combo."
  (flet ((prefixed (pre) (remove-if-not (lambda (p) (uiop:string-prefix-p pre p)) env-pairs)))
    (let ((ctx (make-hash-table :test 'equal))
          (job (make-hash-table :test 'equal)))
      (setf (gethash "github" ctx) (%pairs->map (prefixed "GITHUB_") :strip "GITHUB_" :downcase t))
      ;; `cave` mirrors `github` — for every GITHUB_* there's a CAVE_* twin, so
      ;; ${{ cave.sha }} works exactly like ${{ github.sha }}.
      (setf (gethash "cave" ctx) (%pairs->map (prefixed "CAVE_") :strip "CAVE_" :downcase t))
      (setf (gethash "runner" ctx) (%pairs->map (prefixed "RUNNER_") :strip "RUNNER_" :downcase t))
      (setf (gethash "env" ctx) (%pairs->map env-pairs))
      (setf (gethash "secrets" ctx) (%pairs->map secret-pairs))
      (setf (gethash "steps" ctx) (or steps-map (make-hash-table :test 'equal)))
      (setf (gethash "matrix" ctx) (or matrix-map (make-hash-table :test 'equal)))
      (setf (gethash "needs" ctx) (or needs-map (make-hash-table :test 'equal)))
      ;; Host-side workspace root for hashFiles() (keyword key — never a GHA context).
      (when workspace-root (setf (gethash :workspace-root ctx) workspace-root))
      (setf (gethash "status" job) (or job-status "success"))
      (setf (gethash "job" ctx) job)
      ctx)))

(defun %mk-step-result (outputs outcome conclusion)
  "A steps.<id> entry: outputs map + outcome (success/failure/skipped) +
conclusion (outcome after continue-on-error coalesces failures to success)."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "outputs" h) outputs
          (gethash "outcome" h) outcome
          (gethash "conclusion" h) conclusion)
    h))

(defun %outputs->json (pairs)
  "Serialize resolved job outputs (a list of (name . string-value)) to a JSON
object string. Empty list -> \"\"."
  (if (null pairs)
      ""
      (format nil "{~{~A~^,~}}"
              (mapcar (lambda (p)
                        (format nil "\"~A\":\"~A\"" (%json-escape (car p)) (%json-escape (cdr p))))
                      pairs))))

(defun %podman-exec-fn (container-name)
  "A closure (arglist -> (values exit-code combined-output)) that runs a command
   IN CONTAINER-NAME via podman exec. This is how built-in actions effect changes
   in the job container, sharing the run: steps' filesystem and environment."
  (lambda (cmd-args)
    (multiple-value-bind (out err code)
        (uiop:run-program (append (list "podman" "exec" container-name) cmd-args)
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (values code
              (string-trim '(#\Newline #\Space)
                           (concatenate 'string (or out "")
                                        (if (and err (plusp (length err)))
                                            (concatenate 'string (string #\Newline) err)
                                            "")))))))

(defun %run-action (uses with-pairs gha-ctx ctx step-outputs channel auth-token
                    step-id masks container-name action-base register-post)
  "Resolve and run a `uses:` action. Built-in actions/* run as in-runner
   orchestrators that effect changes in the job container via (ctx :exec).
   Non-built-in owner/repo@ref are resolved cave-local from the chamber and run
   sandboxed in the container (see %run-fetched-action). Streams the log, fills
   STEP-OUTPUTS, returns an exit code (0 ok, 1 failure / unsupported)."
  ;; Mask the job-scoped token: actions may embed it in a clone URL / git output.
  (let ((masks (let ((jt (getf ctx :job-token)))
                 (if (and jt (plusp (length jt))) (cons jt masks) masks))))
   (flet ((emit (text)
           (ignore-errors
            (ag-grpc:grpc-call channel
             "/cave.runner.RunnerService/AppendStepLog"
             (make-instance 'cave::append-step-log-request
                            :step-id step-id :chunk (%mask-secrets text masks))
             :response-type 'cave::append-step-log-response
             :metadata (make-auth-metadata auth-token)))))
    (multiple-value-bind (owner repo ref kind) (parse-action-ref uses)
      (cond
        ((eq kind :external)
         (emit (format nil "Unsupported action ref '~A' — cave resolves owner/repo cave-local only.~%" uses))
         1)
        (t
         (let* ((name (format nil "~A/~A" owner repo))
                (entry (builtin-action name))
                ;; ctx + the in-container exec closure for built-ins.
                (full-ctx (list* :exec (%podman-exec-fn container-name)
                                 :workspace "/workspace"
                                 :runtime-dir "/__cave_rt"
                                 ctx)))
           (if entry
               ;; --- trusted built-in: in-runner orchestrator, effects in-container ---
               (let ((inputs (make-hash-table :test 'equal)))
                 (dolist (kv with-pairs)
                   (let ((p (position #\= kv)))
                     (when p
                       (setf (gethash (subseq kv 0 p) inputs)
                             (interpolate-gha (subseq kv (1+ p)) gha-ctx)))))
                 (apply-input-defaults name inputs)
                 (handler-case
                     (multiple-value-bind (ok outputs log post)
                         (funcall (getf entry :fn) full-ctx inputs)
                       (when (and log (plusp (length log))) (emit log))
                       (when (hash-table-p outputs)
                         (maphash (lambda (k v) (setf (gethash k step-outputs) v)) outputs))
                       ;; A built-in may register a post-thunk (e.g. cache save at job end).
                       (when (and post register-post)
                         (funcall register-post
                                  (lambda ()
                                    (multiple-value-bind (pok plog) (funcall post)
                                      (when (and plog (plusp (length plog))) (emit plog))
                                      pok))))
                       (if ok 0 1))
                   (error (e)
                     (emit (format nil "Action error: ~A~%" e))
                     1)))
               ;; --- non-built-in: resolve from the chamber, run sandboxed ---
               (%run-fetched-action owner repo ref with-pairs gha-ctx full-ctx
                                    step-outputs container-name action-base
                                    #'emit)))))))))

(defun %run-lisp-action-sandboxed (action-dir main-file inputs ctx step-outputs emit)
  "Run a fetched `using: lisp` action in a DEDICATED cave-actions container —
   never in the runner process and never in the job container. The action shares
   only the workspace volume + the file-command runtime dir, gets its inputs as
   INPUT_* env and a job-scoped CAVE_TOKEN, and reads back $GITHUB_OUTPUT. This is
   the trust boundary for untrusted user Lisp: a crash/corruption/runaway dies
   with the throwaway container. Returns an exit code."
  (let* ((workdir (getf ctx :workdir))
         (gh-dir (getf ctx :gh-dir))
         (token (or (getf ctx :job-token) ""))
         (image (config-value :actions-runtime-image "ghcr.io/atgreen/cave-actions:main"))
         (tag (ironclad:byte-array-to-hex-string (ironclad:random-data 4)))
         (out-name (format nil "action-out-~A" tag))
         (out-host (format nil "~A/~A" gh-dir out-name)))
    (ignore-errors
     (with-open-file (s out-host :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (let* ((env-args (loop for k being the hash-keys of inputs using (hash-value v)
                           append (list "-e" (format nil "INPUT_~A=~A"
                                                     (substitute #\_ #\Space (string-upcase k)) v))))
           (run-args (append
                      (list "podman" "run" "--rm"
                            "-v" (format nil "~A:/workspace:Z" workdir)
                            "-v" (format nil "~A:/action:ro" action-dir)
                            "-v" (format nil "~A:/__cave_rt:Z" gh-dir)
                            "-w" "/workspace"
                            "-e" "GITHUB_WORKSPACE=/workspace"
                            "-e" (format nil "GITHUB_OUTPUT=/__cave_rt/~A" out-name)
                            "-e" (format nil "CAVE_TOKEN=~A" token)
                            "-e" (format nil "CAVE_API_URL=~A" (config-value :base-url "")))
                      env-args
                      (list image "/action" main-file))))
      (multiple-value-bind (out err code)
          (uiop:run-program run-args :output '(:string :stripped t)
                            :error-output '(:string :stripped t) :ignore-error-status t)
        (let ((log (concatenate 'string (or out "")
                                (if (and err (plusp (length err)))
                                    (concatenate 'string (string #\Newline) err) ""))))
          (when (plusp (length log)) (funcall emit (concatenate 'string log (string #\Newline)))))
        ;; $GITHUB_OUTPUT (shared via /__cave_rt) -> steps.<id>.outputs
        (let ((outc (ignore-errors (uiop:read-file-string out-host))))
          (when (and outc (plusp (length outc)))
            (dolist (kv (%kv-lines->pairs outc))
              (let ((p (position #\= kv)))
                (when p (setf (gethash (subseq kv 0 p) step-outputs) (subseq kv (1+ p))))))))
        (if (zerop code) 0 1)))))

(defun %run-fetched-action (owner repo ref with-pairs gha-ctx ctx step-outputs
                            container-name action-base emit)
  "Resolve a non-built-in `owner/repo@ref` cave-local: fetch its repo from the
   chamber, read action.yml, and run it. `using: lisp` runs sandboxed (see
   %run-lisp-action-sandboxed); Docker/composite are not supported yet. Reading
   the repo here is host-side (it's data); only execution crosses into a
   container. Returns an exit code."
  (declare (ignore container-name))
  (if (or (null action-base) (zerop (length action-base)))
      (progn
        (funcall emit (format nil "Action '~A/~A' not found and no actions base URL to resolve it.~%"
                              owner repo))
        1)
      (let* ((url (format nil "~A/~A/~A.git" action-base owner repo))
             (tag (ironclad:byte-array-to-hex-string (ironclad:random-data 4)))
             (tmpdir (format nil "/tmp/cave-action-~A" tag)))
        (unwind-protect
            (let ((clone-args (append (list "git" "clone" "--depth" "1")
                                      (when (plusp (length ref)) (list "--branch" ref))
                                      (list url tmpdir))))
              (multiple-value-bind (o e code)
                  (uiop:run-program clone-args :output '(:string :stripped t)
                                    :error-output '(:string :stripped t) :ignore-error-status t)
                (declare (ignore o))
                (cond
                  ((not (zerop code))
                   (funcall emit (format nil "Could not fetch action ~A/~A@~A cave-local: ~A~%"
                                         owner repo (if (plusp (length ref)) ref "default") e))
                   1)
                  (t
                   (let ((spec (parse-action-yml tmpdir)))
                     (cond
                       ((null spec)
                        (funcall emit (format nil "Action ~A/~A has no readable action.yml.~%" owner repo))
                        1)
                       ((equal (getf spec :using) "lisp")
                        (let ((inputs (make-hash-table :test 'equal)))
                          (dolist (kv with-pairs)
                            (let ((p (position #\= kv)))
                              (when p (setf (gethash (subseq kv 0 p) inputs)
                                            (interpolate-gha (subseq kv (1+ p)) gha-ctx)))))
                          (dolist (d (getf spec :inputs))
                            (unless (nth-value 1 (gethash (car d) inputs))
                              (setf (gethash (car d) inputs) (cdr d))))
                          (%run-lisp-action-sandboxed tmpdir (or (getf spec :main) "main.lisp")
                                                      inputs ctx step-outputs emit)))
                       (t
                        (funcall emit (format nil "Action ~A/~A uses '~A' — only `using: lisp` is ~
                                                   supported (Docker/composite not yet).~%"
                                              owner repo (getf spec :using)))
                        1)))))))
          (uiop:run-program (list "rm" "-rf" tmpdir) :ignore-error-status t)))))

