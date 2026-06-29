;;; actions.lisp — the `uses:` action runtime.
;;;
;;; cave models GitHub Actions: the workspace starts EMPTY and an action such as
;;; `actions/checkout` populates it. Actions are referenced `owner/repo@ref` and
;;; resolved cave-local (no github.com). The built-in `actions/*` set is
;;; implemented natively in Lisp and runs in-process in the runner; this is the
;;; trusted core. (Fetching third-party Lisp actions from the chamber and running
;;; them sandboxed is a later step — for now only built-ins resolve.)
;;;
;;; A built-in action function has the contract
;;;   (fn ctx inputs) -> (values ok-p outputs-hash log-string)
;;; where CTX is a plist of host-side job facts (:workdir :clone-url :commit-sha
;;; :ref :token) and INPUTS is a string->string hash of the resolved `with:`
;;; values (already ${{ }}-interpolated). The function performs its effect
;;; host-side and never touches gRPC — the runner glue streams the log and
;;; records the outputs.

(in-package :cave)

(defvar *builtin-actions* (make-hash-table :test 'equal)
  "Maps \"owner/repo\" -> plist (:fn function :inputs default-alist :using string).")

(defun register-builtin-action (name &key fn (inputs nil) (using "lisp"))
  "Register a built-in action under NAME (\"owner/repo\"). INPUTS is an alist of
   (input-name . default) used to fill omitted `with:` keys."
  (setf (gethash name *builtin-actions*) (list :fn fn :inputs inputs :using using)))

(defun builtin-action (name)
  "The registry entry for NAME (\"owner/repo\"), or NIL."
  (gethash name *builtin-actions*))

(defun parse-action-ref (uses)
  "Parse a `uses:` value. Returns (values owner repo ref kind):
   KIND is :local for a bare owner/repo[@ref] (resolved cave-local) or :external
   for docker://, URLs, and ./local paths (not supported yet)."
  (cond
    ((or (search "://" uses)
         (uiop:string-prefix-p "docker://" uses)
         (uiop:string-prefix-p "./" uses)
         (uiop:string-prefix-p "../" uses)
         (uiop:string-prefix-p "/" uses))
     (values nil nil nil :external))
    (t (let* ((at (position #\@ uses))
              (path (if at (subseq uses 0 at) uses))
              (ref (if at (subseq uses (1+ at)) ""))
              (slash (position #\/ path)))
         (if (and slash (plusp slash) (< slash (1- (length path))))
             (values (subseq path 0 slash) (subseq path (1+ slash)) ref :local)
             (values nil nil nil :external))))))

(defun apply-input-defaults (name inputs)
  "Fill INPUTS (a string->string hash) with the registered defaults for action
   NAME that the caller omitted. Returns INPUTS."
  (let ((entry (builtin-action name)))
    (when entry
      (dolist (d (getf entry :inputs))
        (unless (nth-value 1 (gethash (car d) inputs))
          (setf (gethash (car d) inputs) (cdr d))))))
  inputs)

;;; --------------------------------------------------------------------------
;;; actions/checkout
;;; --------------------------------------------------------------------------

(defun %git-in (dir log &rest args)
  "Run git ARGS (DIR is the -C target, or NIL). Append stderr to the LOG stream.
   Returns the exit code."
  (multiple-value-bind (out err code)
      (uiop:run-program (append (list "git") (when dir (list "-C" dir)) args)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (declare (ignore out))
    (when (and err (plusp (length err))) (format log "~A~%" err))
    code))

(defun action-checkout (ctx inputs)
  "Native `actions/checkout`. Clones the workflow repo into the workspace (or a
   `path:` subdir), checks out `ref:` (else the triggering commit), and honors
   `fetch-depth` (0 = full history) and `submodules`. Sets the `ref`/`commit`
   outputs. Returns (values ok-p outputs log)."
  (let* ((workdir (getf ctx :workdir))
         (clone-url (getf ctx :clone-url))
         (commit (getf ctx :commit-sha))
         (job-ref (getf ctx :ref))
         (path-in (gethash "path" inputs))
         (ref-in (gethash "ref" inputs))
         (repo-in (gethash "repository" inputs))
         (depth (let ((d (gethash "fetch-depth" inputs)))
                  (if (and d (plusp (length d)))
                      (or (parse-integer d :junk-allowed t) 1)
                      1)))
         (submodules (gethash "submodules" inputs))
         (target (if (and path-in (plusp (length path-in)))
                     (format nil "~A/~A" workdir path-in)
                     workdir))
         (log (make-string-output-stream))
         (outputs (make-hash-table :test 'equal))
         (ok t))
    ;; cave-local only: a different `repository:` would need chamber resolution.
    (when (and repo-in (plusp (length repo-in)))
      (format log "checkout: 'repository:' override is not supported yet; ~
                   checking out the workflow's own repo.~%"))
    (when (or (null clone-url) (zerop (length clone-url)))
      (format log "checkout: no clone URL available for this job.~%")
      (return-from action-checkout (values nil outputs (get-output-stream-string log))))
    (ignore-errors (ensure-directories-exist (concatenate 'string target "/")))
    (format log "checkout: cloning into ~A~%" target)
    ;; Clone (shallow unless fetch-depth: 0).
    (let ((args (list "clone")))
      (when (plusp depth)
        (setf args (append args (list "--depth" (princ-to-string depth)))))
      (setf args (append args (list clone-url target)))
      (unless (zerop (apply #'%git-in nil log args))
        (format log "checkout: clone failed~%")
        (setf ok nil)))
    ;; Check out the requested ref (or the triggering commit). The clone only
    ;; fetched the default-branch tip, so explicitly fetch the triggering ref —
    ;; a real ref is reliably fetchable, unlike a bare SHA — then check out the
    ;; commit (now present), falling back to the fetched ref tip.
    (when ok
      (let ((fetch-ref (cond ((and ref-in (plusp (length ref-in))) ref-in)
                             ((and job-ref (plusp (length job-ref))) job-ref)
                             (t nil)))
            (checkout-target (cond ((and commit (plusp (length commit))) commit)
                                   ((and ref-in (plusp (length ref-in))) ref-in)
                                   (t nil))))
        (when fetch-ref
          (apply #'%git-in target log "fetch"
                 (append (when (plusp depth) (list "--depth" (princ-to-string depth)))
                         (list "origin" fetch-ref))))
        (when checkout-target
          (unless (zerop (%git-in target log "checkout" checkout-target))
            (if (and fetch-ref (zerop (%git-in target log "checkout" "FETCH_HEAD")))
                (format log "checkout: ~A not found; checked out ~A tip instead~%"
                        checkout-target fetch-ref)
                (progn
                  (format log "checkout: could not check out ~A~%" checkout-target)
                  (setf ok nil)))))))
    ;; Submodules.
    (when (and ok (member submodules '("true" "recursive") :test #'equal))
      (apply #'%git-in target log "submodule" "update" "--init"
             (when (equal submodules "recursive") (list "--recursive"))))
    ;; Outputs: resolved commit + ref.
    (when ok
      (let ((sha (string-trim '(#\Newline #\Space)
                              (or (ignore-errors
                                   (nth-value 0 (uiop:run-program
                                                 (list "git" "-C" target "rev-parse" "HEAD")
                                                 :output '(:string :stripped t)
                                                 :ignore-error-status t)))
                                  ""))))
        (setf (gethash "commit" outputs) sha
              (gethash "ref" outputs) (or (and ref-in (plusp (length ref-in)) ref-in)
                                          job-ref ""))))
    (values ok outputs (get-output-stream-string log))))

(register-builtin-action "actions/checkout"
  :fn #'action-checkout
  :inputs '(("repository" . "") ("ref" . "") ("path" . "")
            ("fetch-depth" . "1") ("submodules" . "false") ("clean" . "true")))
