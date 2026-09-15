;;; db-logging.lisp — regression tests for database lifecycle logging
;;;
;;; SPDX-License-Identifier: MIT

(asdf:load-system :llog)
(asdf:load-system :postmodern)

(defpackage #:cave
  (:use #:cl)
  (:documentation "Minimal Cave package for isolated database logging tests.")
  (:export #:connect-db #:disconnect-db))

(in-package #:cave)

(defun config-value (&rest args)
  "Return NIL for configuration values not exercised by this isolated test."
  (declare (ignore args))
  nil)

(load "src/db.lisp")

(in-package #:cl-user)

(defun database-lifecycle-output-at (level)
  "Capture connection lifecycle output emitted at logger LEVEL."
  (let* ((stream (make-string-output-stream))
         (llog:*logger*
           (llog:make-logger
            :level level
            :outputs (list (llog:make-stream-output stream)))))
    (cave:connect-db :host "db.example" :port 5432
                     :name "cave-test" :user "cave" :password "test")
    (cave:disconnect-db)
    (get-output-stream-string stream)))

(let ((connect (symbol-function 'postmodern:connect-toplevel))
      (disconnect (symbol-function 'postmodern:disconnect-toplevel))
      (clear-pool (symbol-function 'postmodern:clear-connection-pool))
      (connected cave::*db-connected*)
      (spec cave::*db-spec*))
  (unwind-protect
       (progn
         (setf (symbol-function 'postmodern:connect-toplevel)
               (constantly nil))
         (setf (symbol-function 'postmodern:disconnect-toplevel)
               (constantly nil))
         (setf (symbol-function 'postmodern:clear-connection-pool)
               (constantly nil))
         (setf cave::*db-connected* nil)
         (let ((info-output (database-lifecycle-output-at :info))
               (debug-output (database-lifecycle-output-at :debug)))
           (assert (not (search "Connected to database" info-output)) ()
                   "Database lifecycle messages leaked at INFO: ~S" info-output)
           (assert (search "Connected to database" debug-output))
           (assert (search "Disconnected from database" debug-output))))
    (setf (symbol-function 'postmodern:connect-toplevel) connect
          (symbol-function 'postmodern:disconnect-toplevel) disconnect
          (symbol-function 'postmodern:clear-connection-pool) clear-pool
          cave::*db-connected* connected
          cave::*db-spec* spec)))

(format t "Database logging tests passed.~%")
