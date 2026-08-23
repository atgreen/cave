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
                ;; Embedded OpenID Provider
                :usher
                ;; QR codes for TOTP enrollment
                :cl-qrencode
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
                :local-time
                ;; GitHub-Actions ${{ }} expression grammar (instaparse port)
                :iparse)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/config")
                (:file "src/url")
                (:file "src/db")
                (:file "src/auth")
                (:file "src/yaml")
                (:file "src/expr")
                (:file "src/actions")
                (:file "src/model-accounts")
                (:file "src/model-repos")
                (:file "src/model-issues")
                (:file "src/model-activity")
                (:file "src/git")
                (:file "src/chamber")
                (:file "src/chamber-router")
                (:file "src/chamber-client")
                (:file "src/views-base")
                (:file "src/views-repos")
                (:file "src/views-issues")
                (:file "src/views-runs")
                (:file "src/views-settings")
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
                (:file "src/server-core")
                (:file "src/server-hooks")
                (:file "src/server-accounts")
                (:file "src/server-repos")
                (:file "src/server-releases")
                (:file "src/server-issues")
                (:file "src/server-api")
                (:file "src/main-admin")
                (:file "src/main-git")
                (:file "src/main-runner")
                (:file "src/main-app"))
  :build-operation "program-op"
  :build-pathname "cave-server"
  :entry-point "cave:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
