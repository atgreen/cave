;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT

(defpackage #:cave
  (:use #:cl)
  (:documentation "Cave — A self-hosted code forge.")

  ;; Config
  (:export #:*config*
           #:load-config
           #:config-value)

  ;; Database
  (:export #:connect-db
           #:disconnect-db
           #:run-migrations)

  ;; Auth
  (:export #:generate-api-token
           #:current-user)

  ;; Domain
  (:export #:cave-user
           #:cave-org
           #:cave-repo
           #:cave-issue
           #:cave-pull-request
           #:cave-review
           #:cave-concern)

  ;; Entry point
  (:export #:main))
