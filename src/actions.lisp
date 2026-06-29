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

(defun %action-base-from-clone-url (clone-url)
  "Derive the base URL hosting repos from a clone URL:
   <base>/<owner>/<repo>.git -> <base>. NIL if it can't be derived."
  (when (and clone-url (plusp (length clone-url)))
    (let* ((u (string-right-trim "/" clone-url))
           (u (if (uiop:string-suffix-p ".git" u) (subseq u 0 (- (length u) 4)) u))
           (s1 (position #\/ u :from-end t))
           (u2 (and s1 (subseq u 0 s1)))
           (s2 (and u2 (position #\/ u2 :from-end t))))
      (and s2 (subseq u2 0 s2)))))

(defun %authed-url (url token)
  "Embed TOKEN in URL as cave's git http scheme (https://TOKEN@host/...).
   URL unchanged if TOKEN is empty."
  (if (and token (plusp (length token)))
      (let ((pos (search "://" url)))
        (if pos
            (format nil "~A://~A@~A" (subseq url 0 pos) token (subseq url (+ pos 3)))
            url))
      url))

(defun %nonblank-lines (s)
  "Split S on newlines into trimmed, non-empty lines."
  (when s
    (remove-if (lambda (x) (zerop (length x)))
               (mapcar (lambda (x) (string-trim '(#\Space #\Return #\Tab) x))
                       (uiop:split-string s :separator '(#\Newline))))))

(defun %cache-key-file (key)
  "Prefix-preserving sanitization of a cache KEY into a filename component."
  (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\. #\_ #\-))) c #\-))
       key))

(defun action-cache (ctx inputs)
  "Native `actions/cache` — GitHub cache@v4-compatible. Restores on the main step
   and returns a post-thunk that saves at job end (unless an exact key hit). The
   keyed store is a repo-scoped dir bind-mounted at /__cave_cache (shared across
   the runner host's job containers). Honors path, key, restore-keys, lookup-only,
   fail-on-cache-miss; sets the `cache-hit` output. Tar/restore run in the job
   container via (ctx :exec). Returns (values ok outputs log post-thunk)."
  (let* ((exec (getf ctx :exec))
         (workspace (or (getf ctx :workspace) "/workspace"))
         (cache-dir "/__cave_cache")
         (key (gethash "key" inputs))
         (paths (%nonblank-lines (gethash "path" inputs)))
         (restore-keys (%nonblank-lines (gethash "restore-keys" inputs)))
         (lookup-only (equal (gethash "lookup-only" inputs) "true"))
         (fail-on-miss (equal (gethash "fail-on-cache-miss" inputs) "true"))
         (log (make-string-output-stream))
         (outputs (make-hash-table :test 'equal))
         (ok t)
         (exact-hit nil))
    (flet ((sh (&rest args)
             (multiple-value-bind (code out) (funcall exec args)
               (when (and out (plusp (length out))) (format log "~A~%" out))
               code))
           (sh-out (&rest args)
             (multiple-value-bind (code out) (funcall exec args)
               (declare (ignore code))
               (string-trim '(#\Newline #\Space) (or out ""))))
           (exists (path)
             (zerop (nth-value 0 (funcall exec (list "sh" "-c"
                                                     (format nil "test -e ~A" path)))))))
      (when (null exec)
        (format log "cache: no container exec available.~%")
        (return-from action-cache (values nil outputs (get-output-stream-string log))))
      (when (or (null key) (zerop (length key)))
        (format log "cache: 'key' is required.~%")
        (return-from action-cache (values nil outputs (get-output-stream-string log))))
      (when (null paths)
        (format log "cache: 'path' is required.~%")
        (return-from action-cache (values nil outputs (get-output-stream-string log))))
      (funcall exec (list "mkdir" "-p" cache-dir))
      (let* ((kf (%cache-key-file key))
             (entry (format nil "~A/~A.tar.gz" cache-dir kf)))
        (setf exact-hit (exists entry))
        (cond
          (exact-hit
           (format log "cache: exact hit for key '~A'~%" key)
           (unless lookup-only (sh "tar" "xzf" entry "-P" "-C" workspace))
           (setf (gethash "cache-hit" outputs) "true"))
          (t
           (setf (gethash "cache-hit" outputs) "false")
           (let ((restored nil))
             (dolist (rk restore-keys)
               (unless restored
                 (let ((match (sh-out "sh" "-c"
                                      (format nil "ls -1t ~A/~A*.tar.gz 2>/dev/null | head -n1"
                                              cache-dir (%cache-key-file rk)))))
                   (when (plusp (length match))
                     (format log "cache: partial restore from restore-key '~A'~%" rk)
                     (unless lookup-only (sh "tar" "xzf" match "-P" "-C" workspace))
                     (setf restored t)))))
             (unless restored
               (format log "cache: no match for key '~A'~%" key)
               (when fail-on-miss
                 (format log "cache: fail-on-cache-miss is set.~%")
                 (setf ok nil))))))
        ;; Post: save the paths under KEY at job end, unless we had an exact hit.
        (let ((post (when (and ok (not exact-hit) (not lookup-only))
                      (lambda ()
                        (let ((plog (make-string-output-stream)))
                          (flet ((psh (&rest args)
                                   (multiple-value-bind (code out) (funcall exec args)
                                     (when (and out (plusp (length out))) (format plog "~A~%" out))
                                     code)))
                            (funcall exec (list "mkdir" "-p" cache-dir))
                            (if (exists entry)
                                (format plog "cache: key '~A' was saved by another job; skipping.~%" key)
                                (let ((tmp (format nil "~A/.tmp-~A.tar.gz" cache-dir kf)))
                                  (format plog "cache: saving ~{~A~^, ~} under key '~A'~%" paths key)
                                  (if (zerop (apply #'psh "tar" "czf" tmp "-P" "-C" workspace paths))
                                      (psh "mv" "-f" tmp entry)
                                      (progn (format plog "cache: save failed~%")
                                             (psh "rm" "-f" tmp)))))
                            (values t (get-output-stream-string plog))))))))
          (values ok outputs (get-output-stream-string log) post))))))

(defun %sparse-patterns (sparse)
  "Split a multi-line `sparse-checkout` value into non-empty patterns."
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar (lambda (s) (string-trim '(#\Space #\Return #\Tab) s))
                     (uiop:split-string sparse :separator '(#\Newline)))))

(defun action-checkout (ctx inputs)
  "Native `actions/checkout` — compatible with GitHub's checkout@v4. Runs IN the
   job container via (getf ctx :exec), so the repo, git state, and `path:` land in
   the container where run: steps see them. Honors: repository, ref, token,
   persist-credentials, path, clean, filter, sparse-checkout(+cone-mode),
   fetch-depth (0 = full), fetch-tags, lfs, submodules, set-safe-directory. Sets
   the `ref`/`commit` outputs. `ssh-key` is unsupported (warns). Returns
   (values ok-p outputs log)."
  (let* ((exec (getf ctx :exec))
         (workspace (or (getf ctx :workspace) "/workspace"))
         (orig-clone-url (getf ctx :clone-url))
         (commit (getf ctx :commit-sha))
         (job-ref (getf ctx :ref))
         (job-token (getf ctx :job-token))
         (repo-in (gethash "repository" inputs))
         (own-repo-p (or (null repo-in) (zerop (length repo-in))))
         (token (let ((tk (gethash "token" inputs)))
                  (if (and tk (plusp (length tk))) tk job-token)))
         (persist (not (equal (gethash "persist-credentials" inputs) "false")))
         (ref-in (gethash "ref" inputs))
         (path-in (gethash "path" inputs))
         (depth (let ((d (gethash "fetch-depth" inputs)))
                  (if (and d (plusp (length d))) (or (parse-integer d :junk-allowed t) 1) 1)))
         (fetch-tags (equal (gethash "fetch-tags" inputs) "true"))
         (filter (gethash "filter" inputs))
         (sparse (gethash "sparse-checkout" inputs))
         (cone (not (equal (gethash "sparse-checkout-cone-mode" inputs) "false")))
         (lfs (equal (gethash "lfs" inputs) "true"))
         (submodules (gethash "submodules" inputs))
         (clean (not (equal (gethash "clean" inputs) "false")))
         (safe-dir (not (equal (gethash "set-safe-directory" inputs) "false")))
         (ssh-key (gethash "ssh-key" inputs))
         (base (%action-base-from-clone-url orig-clone-url))
         (clone-url (if own-repo-p orig-clone-url (and base (format nil "~A/~A.git" base repo-in))))
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
           (git-q (&rest args) (funcall exec (cons "git" args)))
           (git-out (&rest args)
             (multiple-value-bind (code out) (funcall exec (cons "git" args))
               (declare (ignore code))
               (string-trim '(#\Newline #\Space #\Return) (or out "")))))
      (when (null exec)
        (format log "checkout: no container exec available.~%")
        (return-from action-checkout (values nil outputs (get-output-stream-string log))))
      (when (and ssh-key (plusp (length ssh-key)))
        (format log "::warning::checkout: 'ssh-key' is not supported on cave; ignoring (use token auth).~%"))
      (when (null clone-url)
        (format log "checkout: cannot resolve repository '~A' (no base URL).~%" repo-in)
        (return-from action-checkout (values nil outputs (get-output-stream-string log))))
      (funcall exec (list "mkdir" "-p" target))
      (when safe-dir (git-q "config" "--global" "--add" "safe.directory" target))
      (let ((authed (%authed-url clone-url token))
            (has-git (zerop (git-q "-C" target "rev-parse" "--git-dir"))))
        ;; clean: reuse + clean an existing checkout; else clone fresh (= clean).
        (if (and has-git clean)
            (progn
              (format log "checkout: cleaning existing checkout in ~A~%" target)
              (git "-C" target "clean" "-ffdx")
              (git "-C" target "reset" "--hard")
              (git "-C" target "remote" "set-url" "origin" authed))
            (progn
              (format log "checkout: cloning ~A into ~A~%"
                      (if own-repo-p "the repository" repo-in) target)
              (let ((args (list "clone")))
                (when (plusp depth) (setf args (append args (list "--depth" (princ-to-string depth)))))
                (when (and filter (plusp (length filter)))
                  (setf args (append args (list (format nil "--filter=~A" filter)))))
                (when (and sparse (plusp (length sparse))) (setf args (append args (list "--no-checkout"))))
                (setf args (append args (list authed target)))
                (unless (zerop (apply #'git args))
                  (format log "checkout: clone failed~%")
                  (setf ok nil))))))
      ;; sparse-checkout
      (when (and ok sparse (plusp (length sparse)))
        (git "-C" target "sparse-checkout" "init" (if cone "--cone" "--no-cone"))
        (apply #'git "-C" target "sparse-checkout" "set" (%sparse-patterns sparse)))
      ;; Fetch the triggering ref (reliably fetchable, unlike a bare SHA), then
      ;; check out the commit (now present), falling back to the fetched ref tip.
      (when ok
        (let ((fetch-ref (cond ((and ref-in (plusp (length ref-in))) ref-in)
                               ((and own-repo-p job-ref (plusp (length job-ref))) job-ref)
                               (t nil)))
              (checkout-target (cond ((and ref-in (plusp (length ref-in))) ref-in)
                                     ((and own-repo-p commit (plusp (length commit))) commit)
                                     (t nil))))
          (when fetch-ref
            (apply #'git "-C" target "fetch"
                   (append (when (plusp depth) (list "--depth" (princ-to-string depth)))
                           (when fetch-tags (list "--tags"))
                           (list "origin" fetch-ref))))
          (when (and (not fetch-ref) fetch-tags)
            (git "-C" target "fetch" "--tags" "origin"))
          (when checkout-target
            (unless (zerop (git "-C" target "checkout" checkout-target))
              (if (and fetch-ref (zerop (git-q "-C" target "checkout" "FETCH_HEAD")))
                  (format log "checkout: ~A not found; checked out ~A tip instead~%"
                          checkout-target fetch-ref)
                  (progn
                    (format log "checkout: could not check out ~A~%" checkout-target)
                    (setf ok nil)))))))
      ;; Submodules.
      (when (and ok (member submodules '("true" "recursive") :test #'equal))
        (apply #'git "-C" target "submodule" "update" "--init"
               (when (equal submodules "recursive") (list "--recursive"))))
      ;; Git LFS (needs git-lfs in the job image).
      (when (and ok lfs)
        (unless (zerop (git-q "-C" target "lfs" "pull"))
          (format log "::warning::checkout: 'lfs: true' but git-lfs is unavailable in the job image.~%")))
      ;; persist-credentials: strip the token from the remote when not persisting.
      (when (and ok token (plusp (length token)) (not persist))
        (git-q "-C" target "remote" "set-url" "origin" clone-url))
      ;; Outputs.
      (when ok
        (setf (gethash "commit" outputs) (git-out "-C" target "rev-parse" "HEAD")
              (gethash "ref" outputs) (or (and ref-in (plusp (length ref-in)) ref-in)
                                          (and own-repo-p job-ref) "")))
      (values ok outputs (get-output-stream-string log)))))

(register-builtin-action "actions/checkout"
  :fn #'action-checkout
  :inputs '(("repository" . "") ("ref" . "") ("token" . "")
            ("ssh-key" . "") ("ssh-known-hosts" . "") ("ssh-strict" . "true")
            ("ssh-user" . "git") ("persist-credentials" . "true") ("path" . "")
            ("clean" . "true") ("filter" . "") ("sparse-checkout" . "")
            ("sparse-checkout-cone-mode" . "true") ("fetch-depth" . "1")
            ("fetch-tags" . "false") ("show-progress" . "true") ("lfs" . "false")
            ("submodules" . "false") ("set-safe-directory" . "true")
            ("github-server-url" . "")))

(register-builtin-action "actions/cache"
  :fn #'action-cache
  :inputs '(("path" . "") ("key" . "") ("restore-keys" . "")
            ("upload-chunk-size" . "") ("enableCrossOsArchive" . "false")
            ("fail-on-cache-miss" . "false") ("lookup-only" . "false")
            ("save-always" . "false")))
