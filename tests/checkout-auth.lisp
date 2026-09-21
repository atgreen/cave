;;; checkout-auth.lisp — how the checkout action presents its credential (cave-72c)
;;;
;;; Cave hides a private repo from an anonymous fetch with 404 rather than a 401
;;; challenge, and git only sends credentials *after* a challenge. A token in the
;;; clone URL therefore never leaves the runner, and the clone fails as
;;; "repository not found" no matter how good the token is. Verified against
;;; production: the same request with an explicit Basic header returns 200 while
;;; the URL-credential form returns 404.
;;;
;;; So checkout has to offer the credential up front. These drive the action with
;;; a stub exec and inspect the git command lines it builds.
;;;
;;;   make test-checkout-auth
;;;
;;; SPDX-License-Identifier: MIT

(asdf:load-system :cave)

(in-package #:cl-user)

(defparameter *commands* nil "Git command lines the action asked us to run.")

(defun stub-exec (argv)
  "Stand in for the in-container exec: record ARGV, claim success."
  (push argv *commands*)
  (values 0 ""))

(defun run-checkout (&key (token "cavjt_deadbeefdeadbeefdeadbeefdeadbeef"))
  (let ((*commands* nil)
        (inputs (make-hash-table :test 'equal)))
    (cave::action-checkout
     (list :exec #'stub-exec
           :workspace "/workspace"
           :clone-url "https://cave.example.com/owner/private-repo.git"
           :commit-sha ""
           :ref "main"
           :job-token token)
     inputs)
    (reverse *commands*)))

(defun remote-commands (commands)
  "The git invocations that actually talk to the server."
  (remove-if-not (lambda (argv)
                   (intersection '("clone" "fetch" "ls-remote") argv :test #'equal))
                 commands))

(defun auth-header-arg (argv)
  "The http.extraHeader config argument in ARGV, if any."
  (find-if (lambda (a)
             (and (stringp a)
                  (search "http.extraHeader=" a)
                  (search "Authorization:" a)))
           argv))

(let* ((commands (run-checkout))
       (remotes (remote-commands commands)))

  (assert remotes () "the action should have tried to reach the remote at all")

  ;; --- The credential is offered up front, on every request that leaves the box ---
  (dolist (argv remotes)
    (assert (auth-header-arg argv) ()
            "a remote git call must carry the job token as a header, since cave ~
             never challenges for it: ~S" argv))

  ;; --- It is a well-formed Basic credential carrying the token ---
  (let* ((argv (first remotes))
         (arg (auth-header-arg argv))
         (b64 (subseq arg (+ (search "Basic " arg) 6)))
         (decoded (cl-base64:base64-string-to-string (string-trim " " b64))))
    (assert (search "cavjt_deadbeefdeadbeefdeadbeefdeadbeef" decoded) ()
            "the header should carry the job token, decoded to ~S" decoded))

  ;; --- git's own options must stay ahead of the subcommand ---
  (let ((argv (first remotes)))
    (let ((sub (position-if (lambda (a) (member a '("clone" "fetch" "ls-remote")
                                                :test #'equal))
                            argv))
          (cfg (position-if (lambda (a) (equal a "-c")) argv)))
      (assert (and cfg sub (< cfg sub)) ()
              "-c has to precede the subcommand or git rejects it: ~S" argv))))

;; --- No token, no header: a public checkout stays anonymous ---
(let ((remotes (remote-commands (run-checkout :token ""))))
  (assert remotes () "the action should still try to clone without a token")
  (dolist (argv remotes)
    (assert (not (auth-header-arg argv)) ()
            "an empty token must not produce an empty credential header: ~S" argv)))

(format t "~&Checkout auth tests passed.~%")
