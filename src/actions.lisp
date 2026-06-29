;;; actions.lisp — the `uses:` action runtime.
;;;
;;; cave models GitHub Actions: the workspace starts EMPTY and an action such as
;;; `actions/checkout` populates it. Actions are referenced `owner/repo@ref` and
;;; resolved cave-local (no github.com). Two tiers:
;;;   - Built-in `actions/*` (this file): cave-authored, compiled into the runner,
;;;     run as in-runner orchestrators that effect changes in the job container
;;;     via (ctx :exec). Trusted because it's our code, shipped in the image.
;;;   - Non-built-in owner/repo@ref: fetched from the chamber and run sandboxed in
;;;     a dedicated cave-actions container — never in the runner (see main.lisp
;;;     %run-fetched-action / %run-lisp-action-sandboxed and src/actions-sdk.lisp).
;;;
;;; A built-in action function has the contract
;;;   (fn ctx inputs) -> (values ok-p outputs-hash log-string)
;;; where INPUTS is a string->string hash of the resolved `with:` values (already
;;; ${{ }}-interpolated) and CTX is a plist of job facts:
;;;   :exec        - (lambda (arglist) -> (values exit-code output-string)) that
;;;                  runs a command IN the job container (podman exec). Built-ins
;;;                  effect their changes in the container so they share the same
;;;                  filesystem/env as the run: steps — exactly like GitHub drives
;;;                  a container job. (The action's own control logic runs in the
;;;                  runner; only its effects cross into the container.)
;;;   :workspace   - the workspace path INSIDE the container (e.g. /workspace)
;;;   :runtime-dir - the file-command runtime dir inside the container (/__cave_rt)
;;;   :clone-url :commit-sha :ref - the triggering repo/commit/ref.
;;; The function never touches gRPC — the runner glue streams the log and records
;;; the outputs.

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

(defun parse-action-yml (action-dir)
  "Read action.yml / action.yaml from ACTION-DIR. Returns a plist
   (:using string :main string :inputs alist-of-(name . default)) or NIL when the
   file is absent or unparseable."
  (let* ((dir (uiop:ensure-directory-pathname action-dir))
         (file (or (probe-file (merge-pathnames "action.yml" dir))
                   (probe-file (merge-pathnames "action.yaml" dir)))))
    (when file
      (handler-case
          (let* ((parsed (yaml-parse (uiop:read-file-string file)))
                 (runs (cdr (assoc "runs" parsed :test #'equal)))
                 (inputs (cdr (assoc "inputs" parsed :test #'equal)))
                 (defaults (loop for entry in inputs
                                 for k = (car entry)
                                 for spec = (cdr entry)
                                 collect (cons k (or (and (listp spec)
                                                          (cdr (assoc "default" spec :test #'equal)))
                                                     "")))))
            (list :using (cdr (assoc "using" runs :test #'equal))
                  :main (cdr (assoc "main" runs :test #'equal))
                  :inputs defaults))
        (error () nil)))))

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

(defun action-checkout (ctx inputs)
  "Native `actions/checkout`, run IN the job container via (getf ctx :exec).
   Clones the workflow repo into the workspace (or a `path:` subdir), checks out
   `ref:` (else the triggering commit), and honors `fetch-depth` (0 = full
   history) and `submodules`. Sets the `ref`/`commit` outputs. The clone lands in
   the container's filesystem (where the run: steps see it), and `git` runs from
   the job image. Returns (values ok-p outputs log)."
  (let* ((exec (getf ctx :exec))
         (workspace (or (getf ctx :workspace) "/workspace"))
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
                     (format nil "~A/~A" workspace path-in)
                     workspace))
         (log (make-string-output-stream))
         (outputs (make-hash-table :test 'equal))
         (ok t))
    (flet ((git (&rest args)
             (multiple-value-bind (code out) (funcall exec (cons "git" args))
               (when (and out (plusp (length out))) (format log "~A~%" out))
               code))
           (git-out (&rest args)
             (multiple-value-bind (code out) (funcall exec (cons "git" args))
               (declare (ignore code))
               (string-trim '(#\Newline #\Space #\Return) (or out "")))))
      (when (null exec)
        (format log "checkout: no container exec available.~%")
        (return-from action-checkout (values nil outputs (get-output-stream-string log))))
      ;; cave-local only: a different `repository:` would need chamber resolution.
      (when (and repo-in (plusp (length repo-in)))
        (format log "checkout: 'repository:' override is not supported yet; ~
                     checking out the workflow's own repo.~%"))
      (when (or (null clone-url) (zerop (length clone-url)))
        (format log "checkout: no clone URL available for this job.~%")
        (return-from action-checkout (values nil outputs (get-output-stream-string log))))
      (funcall exec (list "mkdir" "-p" target))
      (format log "checkout: cloning into ~A~%" target)
      ;; Clone (shallow unless fetch-depth: 0).
      (let ((args (list "clone")))
        (when (plusp depth)
          (setf args (append args (list "--depth" (princ-to-string depth)))))
        (setf args (append args (list clone-url target)))
        (unless (zerop (apply #'git args))
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
            (apply #'git "-C" target "fetch"
                   (append (when (plusp depth) (list "--depth" (princ-to-string depth)))
                           (list "origin" fetch-ref))))
          (when checkout-target
            (unless (zerop (git "-C" target "checkout" checkout-target))
              (if (and fetch-ref (zerop (git "-C" target "checkout" "FETCH_HEAD")))
                  (format log "checkout: ~A not found; checked out ~A tip instead~%"
                          checkout-target fetch-ref)
                  (progn
                    (format log "checkout: could not check out ~A~%" checkout-target)
                    (setf ok nil)))))))
      ;; Submodules.
      (when (and ok (member submodules '("true" "recursive") :test #'equal))
        (apply #'git "-C" target "submodule" "update" "--init"
               (when (equal submodules "recursive") (list "--recursive"))))
      ;; Outputs: resolved commit + ref.
      (when ok
        (setf (gethash "commit" outputs) (git-out "-C" target "rev-parse" "HEAD")
              (gethash "ref" outputs) (or (and ref-in (plusp (length ref-in)) ref-in)
                                          job-ref "")))
      (values ok outputs (get-output-stream-string log)))))

(register-builtin-action "actions/checkout"
  :fn #'action-checkout
  :inputs '(("repository" . "") ("ref" . "") ("path" . "")
            ("fetch-depth" . "1") ("submodules" . "false") ("clean" . "true")))
