;;; internal-auth.lisp — who may call /-/internal/ (cave-72c follow-on)
;;;
;;; These endpoints used to trust position alone: loopback or nothing. That
;;; works while everything shares one network namespace, but the git-SSH front
;;; end deliberately does not - it runs on pasta so sshd sees real client
;;; addresses - and so reaches cave through the host, never as loopback. It
;;; authenticates with a shared token instead.
;;;
;;;   make test-internal-auth
;;;
;;; SPDX-License-Identifier: MIT

(asdf:load-system :cave)

(in-package #:cl-user)

(defmacro with-internal-token ((token) &body body)
  `(let ((cave::*config* (list :internal-token ,token)))
     ,@body))

;; --- Loopback is trusted with no token configured (the original behaviour) ---
(with-internal-token (nil)
  (assert (cave::internal-caller-allowed-p "127.0.0.1" nil) ()
          "loopback must be allowed")
  (assert (cave::internal-caller-allowed-p "::1" nil) ()
          "IPv6 loopback must be allowed")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" nil)) ()
          "a non-loopback caller must be refused when no token is configured")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" "anything")) ()
          "a token must not open the door when none is configured - otherwise ~
           configuring nothing would be less safe than configuring something"))

;; --- With a token, an off-box caller may present it ---
(with-internal-token ("s3cret-internal-token")
  (assert (cave::internal-caller-allowed-p "10.89.0.1" "s3cret-internal-token") ()
          "the front end's token must be accepted")
  (assert (cave::internal-caller-allowed-p "127.0.0.1" nil) ()
          "loopback must still work without presenting a token")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" "wrong-token")) ()
          "a wrong token must be refused")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" nil)) ()
          "no token from off-box must be refused")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" "")) ()
          "an empty token must be refused")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" "s3cret-internal-tok")) ()
          "a prefix of the token must be refused"))

;; --- An empty configured token must not become a skeleton key ---
(with-internal-token ("")
  (assert (not (cave::internal-caller-allowed-p "10.89.0.1" "")) ()
          "an empty configured token must not authenticate anyone"))

(format t "~&Internal endpoint auth tests passed.~%")
