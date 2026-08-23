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
  (:export #:generate-api-token)

  ;; Entry point
  (:export #:main))
