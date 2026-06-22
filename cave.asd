;;; cave.asd
;;;
;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:cave
  :description "Cave — A self-hosted code forge"
  :author      "Cave contributors"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  (:version-string
                :clingon
                :hunchentoot
                :easy-routes
                :llog
                :cl-dotenv
                :slynk
                :bordeaux-threads
                ;; Database
                :postmodern
                ;; Crypto & auth
                :ironclad
                :cl-bcrypt
                :cl-base64
                :flexi-streams
                ;; HTTP client (for OIDC token exchange)
                :dexador
                ;; URI parsing (SSRF validation of remote URLs)
                :quri
                ;; JSON
                :com.inuoe.jzon
                ;; HTML generation
                :spinneret
                ;; gRPC (runner service)
                :ag-grpc
                ;; Email
                :cl-smtp
                ;; Markdown rendering
                :cl-commonmark
                :sanitize-html
                ;; UUIDs
                :frugal-uuid
                ;; Date/time
                :local-time)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/config")
                (:file "src/db")
                (:file "src/auth")
                (:file "src/yaml")
                (:file "src/model")
                (:file "src/git")
                (:file "src/chamber")
                (:file "src/chamber-router")
                (:file "src/chamber-client")
                (:file "src/views")
                (:file "src/metrics")
                (:file "src/notify")
                (:file "src/search-zoekt")
                (:file "src/deps-dashboard")
                (:file "src/osv")
                (:file "src/sbom")
                (:file "src/deps-fix")
                (:file "src/deps-policy")
                (:file "src/workflow")
                (:file "src/runner-service")
                (:file "src/ssh")
                (:file "src/server")
                (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "cave-server"
  :entry-point "cave:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
