;;; actions-sdk.lisp — the cave-actions runtime SDK.
;;;
;;; This file is NOT part of the cave system. It is shipped in the dedicated
;;; `cave-actions` container image and loaded by a plain SBCL there to run a
;;; user-provided `using: lisp` action. It runs in a throwaway container that
;;; shares only the workspace volume + the file-command runtime dir with the job
;;; — never in the cave runner process — so untrusted action code cannot touch
;;; the runner's heap, tokens, or other jobs.
;;;
;;; Deliberately dependency-free: only CL + sb-ext (no ASDF/Quicklisp), so the
;;; image needs nothing but `sbcl` (+ `curl` for the API helper). The user action
;;; is a Lisp file that runs on load and calls the `cave-actions` (`ca`) helpers:
;;;
;;;   (ca:info "Hello from ~A" (ca:input "name"))
;;;   (ca:set-output "result" "42")
;;;
;;; Contract: the action's `runs:` declares `using: lisp` and `main: <file>`;
;;; the runtime loads <file> from /action. Errors / (ca:set-failed ...) exit 1.

(defpackage :cave-actions
  (:use :cl)
  (:nicknames :ca)
  (:export :input :get-input :set-output :export-var :add-path :workspace
           :info :notice :action-warn :action-error :set-failed
           :sh :sh! :api :run-action))

(in-package :cave-actions)

(defun env (name &optional default)
  (or (sb-ext:posix-getenv name) default))

(defun input (name &optional default)
  "The `with:` input NAME (GitHub convention: INPUT_<UPCASE, spaces->_>)."
  (or (env (format nil "INPUT_~A" (substitute #\_ #\Space (string-upcase name))))
      default))

(defun get-input (name &optional default) (input name default))

(defun %append-line (envvar line)
  (let ((f (env envvar)))
    (when (and f (plusp (length f)))
      (with-open-file (s f :direction :output :if-exists :append :if-does-not-exist :create)
        (write-line line s)))))

(defun set-output (name value)
  "Set steps.<id>.outputs.NAME for downstream steps (writes $GITHUB_OUTPUT)."
  (%append-line "GITHUB_OUTPUT" (format nil "~A=~A" name value)))

(defun export-var (name value)
  "Export an env var to subsequent steps (writes $GITHUB_ENV)."
  (%append-line "GITHUB_ENV" (format nil "~A=~A" name value)))

(defun add-path (dir)
  "Prepend DIR to PATH for subsequent steps (writes $GITHUB_PATH)."
  (%append-line "GITHUB_PATH" dir))

(defun workspace () (or (env "GITHUB_WORKSPACE") "/workspace"))

(defun info (fmt &rest args) (format t "~&~A~%" (apply #'format nil fmt args)))
(defun notice (fmt &rest args) (format t "~&::notice::~A~%" (apply #'format nil fmt args)))
(defun action-warn (fmt &rest args) (format t "~&::warning::~A~%" (apply #'format nil fmt args)))
(defun action-error (fmt &rest args) (format t "~&::error::~A~%" (apply #'format nil fmt args)))

(defun sh (command)
  "Run COMMAND via /bin/sh in THIS action container. Returns (values output exit-code)."
  (let* ((out (make-string-output-stream))
         (proc (sb-ext:run-program "/bin/sh" (list "-c" command)
                                   :search nil :output out :error out :wait t)))
    (values (get-output-stream-string out) (sb-ext:process-exit-code proc))))

(defun sh! (command)
  "Like SH but signals an error on a non-zero exit."
  (multiple-value-bind (out code) (sh command)
    (unless (zerop code) (error "command failed (~A): ~A~%~A" code command out))
    out))

(defun api (method path &key data)
  "Call cave's API at $CAVE_API_URL/PATH with the job-scoped $CAVE_TOKEN, via
   curl. METHOD is GET/POST/.... Returns (values body exit-code)."
  (let* ((base (string-right-trim "/" (or (env "CAVE_API_URL") "")))
         (token (or (env "CAVE_TOKEN") ""))
         (url (format nil "~A/~A" base (string-left-trim "/" path)))
         (args (append (list "-fsS" "-X" method
                             "-H" (format nil "Authorization: Bearer ~A" token))
                       (when data (list "-H" "Content-Type: application/json" "-d" data))
                       (list url))))
    (let* ((out (make-string-output-stream))
           (proc (sb-ext:run-program "curl" args :search t :output out :error out :wait t)))
      (values (get-output-stream-string out) (sb-ext:process-exit-code proc)))))

(defun set-failed (fmt &rest args)
  "Mark the action failed and exit non-zero."
  (apply #'action-error fmt args)
  (sb-ext:exit :code 1))

(defun run-action (action-dir main-file)
  "Load and run MAIN-FILE from ACTION-DIR (the user action). Any error exits 1."
  (let ((path (merge-pathnames main-file
                               (concatenate 'string (string-right-trim "/" action-dir) "/"))))
    (handler-case
        (progn
          (unless (probe-file path)
            (format t "~&::error::action entry not found: ~A~%" path)
            (sb-ext:exit :code 1))
          (load path)
          (sb-ext:exit :code 0))
      (sb-sys:interactive-interrupt () (sb-ext:exit :code 130))
      (error (e)
        (format t "~&::error::action failed: ~A~%" e)
        (sb-ext:exit :code 1)))))
