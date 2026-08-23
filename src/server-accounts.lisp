(in-package #:cave)

;; ----------------------------------------------------------------------------
;; Routes: Auth

;; Backwards compat: /login redirects to OIDC login
(easy-routes:defroute login-redirect ("/login" :method :get) ()
  (let ((next-url (sanitize-next-url (hunchentoot:get-parameter "next"))))
    (hunchentoot:redirect
     (format nil "/-/auth/login?next=~A"
             (hunchentoot:url-encode next-url)))))

(easy-routes:defroute oidc-login ("/-/auth/login" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/")
      (let* ((next-url (sanitize-next-url (hunchentoot:get-parameter "next")))
             (state (generate-oidc-state))
             (verifier (generate-oidc-verifier)))
        ;; Store state + next-url in a short-lived cookie
        (hunchentoot:set-cookie "cave_oidc_state"
                                :value (format nil "~A:~A" state next-url)
                                :path "/"
                                :http-only t
                                :max-age 600)
        ;; Store the PKCE code verifier for the callback
        (hunchentoot:set-cookie "cave_oidc_verifier"
                                :value verifier :path "/" :http-only t :max-age 600)
        (hunchentoot:redirect (oidc-authorization-url
                               state
                               :code-challenge (oidc-code-challenge verifier)
                               :nonce (generate-oidc-state))))))

(easy-routes:defroute oidc-callback ("/-/auth/callback" :method :get) ()
  (let* ((code (hunchentoot:get-parameter "code"))
         (state (hunchentoot:get-parameter "state"))
         (cookie (hunchentoot:cookie-in "cave_oidc_state"))
         (verifier (hunchentoot:cookie-in "cave_oidc_verifier"))
         (colon-pos (when cookie (position #\: cookie)))
         (saved-state (when colon-pos (subseq cookie 0 colon-pos)))
         (rest-of-cookie (when colon-pos (subseq cookie (1+ colon-pos))))
         (is-sudo (and rest-of-cookie (>= (length rest-of-cookie) 5)
                       (string= "sudo:" (subseq rest-of-cookie 0 5))))
         (next-url (sanitize-next-url
                    (cond (is-sudo (subseq rest-of-cookie 5))
                          (rest-of-cookie rest-of-cookie)
                          (t "/")))))
    ;; Clear the state + PKCE verifier cookies
    (hunchentoot:set-cookie "cave_oidc_state" :value "" :path "/" :max-age 0)
    (hunchentoot:set-cookie "cave_oidc_verifier" :value "" :path "/" :max-age 0)
    ;; Validate state — if invalid, redirect to login (e.g. after password reset)
    (unless (and code state saved-state (string= state saved-state))
      (hunchentoot:redirect "/-/auth/login")
      (return-from oidc-callback nil))
    ;; Exchange code for tokens
    (let ((tokens (exchange-oidc-code code verifier)))
      (unless tokens
        (setf (hunchentoot:return-code*) 502)
        (return-from oidc-callback "Failed to exchange authorization code"))
      (let* ((access-token (gethash "access_token" tokens))
             (userinfo (fetch-oidc-userinfo access-token)))
        (unless userinfo
          (setf (hunchentoot:return-code*) 502)
          (return-from oidc-callback "Failed to fetch user info"))
        ;; Provision/update local user
        (let ((user (provision-oidc-user userinfo)))
          (unless (getf user :is-active)
            (setf (hunchentoot:return-code*) 403)
            (return-from oidc-callback "Account is deactivated"))
          ;; Approval gate. Pending users see a friendly waiting page and
          ;; no session is established; rejected users get a final notice.
          (let ((status (getf user :approval-status)))
            (cond
              ((string= status "pending")
               (return-from oidc-callback
                 (html-response (view-account-pending :username (getf user :username)))))
              ((string= status "rejected")
               (setf (hunchentoot:return-code*) 403)
               (return-from oidc-callback
                 (html-response (view-account-rejected :username (getf user :username)))))))
          (if is-sudo
              ;; Sudo flow — set sudo cookie, keep existing session
              (progn
                (set-sudo-cookie)
                (hunchentoot:redirect (or next-url "/-/settings")))
              ;; Normal login — create new session
              (let ((session-token (create-session (getf user :id))))
                (hunchentoot:set-cookie "cave_session"
                                        :value session-token
                                        :path "/"
                                        :http-only t
                                        :max-age (* *session-duration-hours* 3600))
                (hunchentoot:redirect (or next-url "/")))))))))

;; Sudo mode — force re-authentication for dangerous actions
(easy-routes:defroute sudo-redirect ("/-/sudo" :method :get) ()
  (if *current-user*
      (let* ((next-url (or (hunchentoot:get-parameter "next") "/-/settings"))
             (state (generate-oidc-state))
             (verifier (generate-oidc-verifier)))
        ;; Store state with sudo: prefix so callback knows to set sudo cookie
        (hunchentoot:set-cookie "cave_oidc_state"
                                :value (format nil "~A:sudo:~A" state next-url)
                                :path "/"
                                :http-only t
                                :max-age 600)
        (hunchentoot:set-cookie "cave_oidc_verifier"
                                :value verifier :path "/" :http-only t :max-age 600)
        (hunchentoot:redirect (oidc-authorization-url
                               state :force-login t
                               :code-challenge (oidc-code-challenge verifier)
                               :nonce (generate-oidc-state))))
      (hunchentoot:redirect "/-/auth/login")))

(easy-routes:defroute register-page ("/-/register" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/")
      (html-response (view-register))))

(easy-routes:defroute register-submit ("/-/register" :method :post) ()
  (let ((username (hunchentoot:post-parameter "username"))
        (email (hunchentoot:post-parameter "email"))
        (password (hunchentoot:post-parameter "password")))
    (case (usher-register-user username email password)
      (:ok (hunchentoot:redirect "/-/auth/login"))
      (:taken (html-response (view-register :error "That username is already taken."
                                            :username username :email email)))
      (t (html-response (view-register
                         :error "Enter a username and a password of at least 8 characters."
                         :username username :email email))))))

(easy-routes:defroute logout ("/logout" :method :post) ()
  ;; With the embedded provider the cave session IS the auth state — there is no
  ;; separate IdP SSO session to end. Clear the session locally and go home.
  (delete-session (hunchentoot:cookie-in "cave_session"))
  (hunchentoot:set-cookie "cave_session" :value "" :path "/" :max-age 0)
  (hunchentoot:redirect (or (config-value :base-url) "/")))

;; ----------------------------------------------------------------------------
;; Routes: Dashboard

(defun issue-template (owner repo-name)
  "Return the repo's issue template body (.cave/issue_template.md or a .github
fallback) at the default branch, or NIL when there is none."
  (ignore-errors
   (let ((ref (or (chamber-get-default-branch owner repo-name) "main")))
     (loop for path in '(".cave/issue_template.md" ".github/ISSUE_TEMPLATE.md"
                         ".github/issue_template.md" "ISSUE_TEMPLATE.md")
           for content = (ignore-errors (chamber-get-blob owner repo-name ref path))
           when (and content (plusp (length content))) return content))))

(defun pr-code-owners (owner repo-name pr)
  "Distinct CODEOWNERS owner tokens (e.g. @alice) for the files a PR changes,
read from .cave/CODEOWNERS (or a fallback) at the default branch. NIL if none."
  (ignore-errors
   (let ((text (let ((ref (or (chamber-get-default-branch owner repo-name) "main")))
                 (loop for path in '(".cave/CODEOWNERS" "CODEOWNERS" ".github/CODEOWNERS")
                       for c = (ignore-errors (chamber-get-blob owner repo-name ref path))
                       when (and c (plusp (length c))) return c))))
     (when text
       (let* ((rules (parse-codeowners text))
              (disk (repo-disk-path owner repo-name))
              (files (multiple-value-bind (out e code)
                         (git-run disk "diff" "--name-only"
                                  (format nil "~A...~A"
                                          (getf pr :target-branch)
                                          (getf pr :source-branch)))
                       (declare (ignore e))
                       (when (zerop code)
                         (remove-if #'uiop:emptyp
                                    (uiop:split-string out :separator '(#\Newline))))))
              (owners nil))
         (dolist (f files)
           (dolist (o (codeowners-for-path rules f))
             (pushnew o owners :test #'equal)))
         (nreverse owners))))))

(defun notify-code-owners (owner repo-name repo pr code-owners)
  "In-app notify the @username code owners of a new PR (skipping the author)."
  (dolist (token code-owners)
    (when (uiop:string-prefix-p "@" token)
      (let ((user (ignore-errors (find-user-by-username (subseq token 1)))))
        (when (and user (not (eql (getf user :id) (getf pr :author-id))))
          (ignore-errors
           (create-notification
            :user-id (getf user :id) :repo-id (getf repo :id) :kind "pr_codeowner"
            :subject (format nil "You're a code owner on PR #~A: ~A → ~A"
                             (getf pr :number) (getf pr :source-branch)
                             (getf pr :target-branch))
            :link (format nil "/~A/~A/pulls/~A" owner repo-name (getf pr :number)))))))))

(defun compute-landing-hero ()
  "Render the landing hero from cave/cave-landing:index.md, or NIL when that repo
or file is absent. Cached by the file's blob sha (reuses the README cache), so
editing the landing copy is a git push — no redeploy."
  (let ((repo (ignore-errors (find-repo "cave" "cave-landing"))))
    (when repo
      (let* ((ref (or (chamber-get-default-branch "cave" "cave-landing") "main"))
             (info (ignore-errors
                    (chamber-get-blob-info "cave" "cave-landing" ref "index.md"))))
        (when info
          (let* ((key (cons :landing-hero (getf info :hash)))
                 (cached (readme-cache-get key)))
            (or cached
                (let* ((md (ignore-errors
                            (chamber-get-blob "cave" "cave-landing" ref "index.md")))
                       (html (when (and md (plusp (length md)))
                               (render-markdown md))))
                  (when html (readme-cache-put key html))
                  html))))))))

(easy-routes:defroute dashboard ("/" :method :get) ()
  (if *current-user*
      (html-response
       (view-dashboard :orgs (list-user-orgs *current-user-id*)
                       :repos (list-user-repos *current-user-id* :include-private t)
                       :username (getf *current-user* :username)
                       :events (list-recent-events :limit 20)))
      (html-response
       (view-public-landing :repos (list-public-repos :limit 50)
                            :events (list-recent-public-events :limit 20)
                            :hero-html (ignore-errors (compute-landing-hero))))))

;; ----------------------------------------------------------------------------
;; Routes: Org creation

(defun update-repo-primary-language (owner repo-name repo-id ref)
  "Compute and store REPO-ID's primary language (largest by bytes at REF)."
  (set-repo-primary-language
   repo-id (first (first (chamber-language-stats owner repo-name ref)))))

(easy-routes:defroute camo-proxy ("/-/camo/:sig/:hex" :method :get) ()
  "Signed image proxy: verify the HMAC, SSRF-guard the target, fetch, and stream
it only if it's an image. Lets rendered markdown show external images without
leaking the viewer's IP or breaking HTTPS."
  (let ((url (ignore-errors
              (sb-ext:octets-to-string (ironclad:hex-string-to-byte-array hex)
                                       :external-format :utf-8))))
    (unless (and url (string= sig (camo-sig url)))
      (setf (hunchentoot:return-code*) 403)
      (return-from camo-proxy "forbidden"))
    (handler-case
        (let ((safe (ensure-safe-remote-url url)))
          (multiple-value-bind (body status headers)
              (dex:get safe :force-binary t :connect-timeout 5 :read-timeout 10
                       :max-redirects 0)
            (let ((ct (and headers (gethash "content-type" headers))))
              (unless (and (eql status 200) ct (uiop:string-prefix-p "image/" ct))
                (setf (hunchentoot:return-code*) 415)
                (return-from camo-proxy "not an image"))
              (setf (hunchentoot:content-type*) ct)
              (setf (hunchentoot:header-out :cache-control) "public, max-age=86400")
              body)))
      (error ()
        (setf (hunchentoot:return-code*) 502)
        "fetch failed"))))

(easy-routes:defroute explore-page ("/-/explore" :method :get) ()
  (let* ((q (hunchentoot:get-parameter "q"))
         (lang (let ((l (hunchentoot:get-parameter "language")))
                 (and l (plusp (length l)) l)))
         (sort (or (hunchentoot:get-parameter "sort") "recent"))
         (page (max 1 (or (parse-integer (or (hunchentoot:get-parameter "page") "1")
                                         :junk-allowed t)
                          1)))
         (per-page 30)
         (offset (* (1- page) per-page))
         (blank-q (or (null q) (zerop (length (string-trim " " q))))))
    (html-response
     (view-explore :repos (search-public-repos :query q :language lang :sort sort
                                               :limit per-page :offset offset)
                   :total (count-public-repos :query q :language lang)
                   :query q :sort sort :page page :per-page per-page
                   :languages (public-language-facets)
                   :current-language lang
                   :trending (when (and blank-q (not lang) (= page 1))
                               (trending-public-repos :days 7 :limit 6))
                   :users (list-users)
                   :orgs (list-orgs)))))

(easy-routes:defroute new-org-page ("/-/new-org" :method :get) ()
  (when (require-login)
    (html-response (view-new-org))))

(easy-routes:defroute create-org-submit ("/-/new-org" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name"))
          (display-name (hunchentoot:post-parameter "display_name"))
          (description (hunchentoot:post-parameter "description")))
      (handler-case
          (progn
            (create-org :name name
                        :display-name (if (uiop:emptyp display-name) name display-name)
                        :description (unless (uiop:emptyp description) description)
                        :creator-id *current-user-id*)
            (hunchentoot:redirect (format nil "/o/~A" name)))
        (cl-postgres-error:unique-violation ()
          (html-response
           (view-new-org :error (format nil "An organization named \"~A\" already exists." name))))
        (error (e)
          (html-response (view-new-org :error (format nil "~A" e))))))))

;; ----------------------------------------------------------------------------
;; Routes: Admin

(easy-routes:defroute admin-page ("/-/admin" :method :get) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-page "Forbidden"))
    (html-response (view-admin :users (list-users :active-only nil)
                               :pending-users (list-pending-users)
                               :runners (list-runners)))))

(easy-routes:defroute admin-create-runner-token ("/-/admin/runners/token" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-create-runner-token "Forbidden"))
    (let ((token (create-registration-token :created-by-id *current-user-id*)))
      (html-response (view-admin :users (list-users :active-only nil)
                                 :pending-users (list-pending-users)
                                 :runners (list-runners)
                                 :registration-token token)))))

(easy-routes:defroute admin-delete-runner ("/-/admin/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-delete-runner "Forbidden"))
    (let ((rid (parse-integer runner-id :junk-allowed t)))
      (when rid (delete-runner rid)))
    (hunchentoot:redirect "/-/admin")))

(easy-routes:defroute admin-approve-user ("/-/admin/users/:user-id/approve" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-approve-user "Forbidden"))
    (let ((uid (parse-integer user-id :junk-allowed t)))
      (when uid (set-user-approval uid "approved")))
    (hunchentoot:redirect "/-/admin")))

(easy-routes:defroute admin-reject-user ("/-/admin/users/:user-id/reject" :method :post) ()
  (when (require-login)
    (unless (getf *current-user* :is-admin)
      (setf (hunchentoot:return-code*) 403)
      (return-from admin-reject-user "Forbidden"))
    (let ((uid (parse-integer user-id :junk-allowed t)))
      (when uid (set-user-approval uid "rejected")))
    (hunchentoot:redirect "/-/admin")))


;; ----------------------------------------------------------------------------
;; Routes: User settings

(easy-routes:defroute notifications-page ("/-/notifications" :method :get) ()
  (when (require-login)
    (html-response
     (view-notifications :notifications (list-notifications *current-user-id* :limit 100)))))

(easy-routes:defroute notifications-read-submit ("/-/notifications/read" :method :post) ()
  (when (require-login)
    (mark-all-notifications-read *current-user-id*)
    (hunchentoot:redirect "/-/notifications")))

(easy-routes:defroute notification-go ("/-/notifications/:id/go" :method :get) ()
  (when (require-login)
    (let* ((nid (parse-integer id :junk-allowed t))
           (n (and nid (find-notification nid *current-user-id*))))
      (when nid (mark-notification-read nid *current-user-id*))
      (hunchentoot:redirect (or (and n (getf n :link)) "/-/notifications")))))

(easy-routes:defroute settings-page ("/-/settings" :method :get) ()
  (when (require-login)
    (html-response
     (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                    :gpg-keys (list-gpg-keys *current-user-id*)
                    :api-tokens (list-api-tokens *current-user-id*)
                    :runners (list-runners :scope "user" :scope-id *current-user-id*)))))

(easy-routes:defroute set-theme-submit ("/-/settings/theme" :method :post) ()
  (when (require-login)
    (let ((theme (hunchentoot:post-parameter "theme")))
      (when (and theme (not (uiop:emptyp theme)))
        (set-user-theme *current-user-id* theme)))
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute add-ssh-key-submit ("/-/settings/ssh-keys" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name"))
          (public-key (string-trim '(#\Newline #\Return #\Space)
                                   (hunchentoot:post-parameter "public_key"))))
      (handler-case
          (progn (add-ssh-key *current-user-id* name public-key)
                 (sync-authorized-keys)
                 (hunchentoot:redirect "/-/settings"))
        (error (e)
          (html-response
           (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                          :gpg-keys (list-gpg-keys *current-user-id*)
                          :api-tokens (list-api-tokens *current-user-id*)
                          :ssh-error (format nil "~A" e))))))))

(easy-routes:defroute add-gpg-key-submit ("/-/settings/gpg-keys" :method :post) ()
  (when (require-login)
    (let ((name (hunchentoot:post-parameter "name"))
          (public-key (string-trim '(#\Newline #\Return #\Space)
                                   (or (hunchentoot:post-parameter "public_key") ""))))
      (handler-case
          (progn (add-gpg-key *current-user-id* name public-key)
                 (hunchentoot:redirect "/-/settings"))
        (error (e)
          (html-response
           (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                          :gpg-keys (list-gpg-keys *current-user-id*)
                          :api-tokens (list-api-tokens *current-user-id*)
                          :gpg-error (format nil "~A" e))))))))

(easy-routes:defroute delete-gpg-key-submit
    ("/-/settings/gpg-keys/:key-id/delete" :method :post) ()
  (when (require-login)
    (let ((kid (parse-integer key-id :junk-allowed t)))
      (when kid (delete-gpg-key kid *current-user-id*)))
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute change-password-page ("/-/settings/password" :method :get) ()
  (when (require-sudo "/-/settings/password")
    (html-response (view-change-password))))

(easy-routes:defroute change-password-submit ("/-/settings/password" :method :post) ()
  (when (require-sudo "/-/settings/password")
    (let ((new (hunchentoot:post-parameter "new_password"))
          (confirm (hunchentoot:post-parameter "confirm_password")))
      (cond
        ((or (null new) (< (length new) 8))
         (html-response (view-change-password
                         :error "Password must be at least 8 characters.")))
        ((not (string= new confirm))
         (html-response (view-change-password :error "Passwords do not match.")))
        ((usher-set-password (getf *current-user* :username) new)
         (html-response (view-change-password :success t)))
        (t
         (html-response (view-change-password :error "Could not update password.")))))))

(easy-routes:defroute totp-page ("/-/settings/totp" :method :get) ()
  (when (require-sudo "/-/settings/totp")
    (html-response (view-totp :enabled (usher-totp-enabled-p)))))

(easy-routes:defroute totp-enroll-submit ("/-/settings/totp/enroll" :method :post) ()
  (when (require-sudo "/-/settings/totp")
    (multiple-value-bind (uri secret) (usher-totp-enroll)
      (if uri
          (html-response (view-totp-enroll :qr (totp-qr-data-uri uri) :secret secret))
          (hunchentoot:redirect "/-/settings/totp")))))

(easy-routes:defroute totp-confirm-submit ("/-/settings/totp/confirm" :method :post) ()
  (when (require-sudo "/-/settings/totp")
    (let* ((code (hunchentoot:post-parameter "code"))
           (codes (and code (usher-totp-confirm code))))
      (if codes
          (html-response (view-totp-backup-codes :codes codes :enabled-now t))
          (multiple-value-bind (uri secret) (usher-totp-enroll)
            (html-response (view-totp-enroll :qr (and uri (totp-qr-data-uri uri))
                                             :secret secret
                                             :error "Invalid code — try again.")))))))

(easy-routes:defroute totp-disable-submit ("/-/settings/totp/disable" :method :post) ()
  (when (require-sudo "/-/settings/totp")
    (usher-totp-disable)
    (hunchentoot:redirect "/-/settings/totp")))

(easy-routes:defroute totp-backup-codes-submit ("/-/settings/totp/backup-codes" :method :post) ()
  (when (require-sudo "/-/settings/totp")
    (let ((codes (usher-backup-codes-regenerate)))
      (html-response (view-totp-backup-codes :codes codes)))))

(easy-routes:defroute generate-ssh-key-submit
    ("/-/settings/ssh-keys/generate" :method :post) ()
  (when (require-sudo "/-/settings")
    (let ((name (hunchentoot:post-parameter "name")))
      (handler-case
          (multiple-value-bind (private-key _record)
              (generate-ssh-keypair *current-user-id* name)
            (declare (ignore _record))
            (sync-authorized-keys)
            (html-response
             (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                            :gpg-keys (list-gpg-keys *current-user-id*)
                            :api-tokens (list-api-tokens *current-user-id*)
                            :generated-private-key private-key
                            :generated-key-name name)))
        (error (e)
          (html-response
           (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                          :gpg-keys (list-gpg-keys *current-user-id*)
                          :api-tokens (list-api-tokens *current-user-id*)
                          :ssh-error (format nil "~A" e))))))))

(easy-routes:defroute delete-ssh-key-submit
    ("/-/settings/ssh-keys/:key-id/delete" :method :post) ()
  (when (require-login)
    (let ((kid (parse-integer key-id :junk-allowed t)))
      (when kid (delete-ssh-key kid *current-user-id*)))
    (sync-authorized-keys)
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute create-token-submit ("/-/settings/tokens" :method :post) ()
  (when (require-sudo "/-/settings")
    (let ((name (hunchentoot:post-parameter "name")))
      (multiple-value-bind (token-string _record)
          (create-api-token *current-user-id* name)
        (declare (ignore _record))
        (html-response
         (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                        :gpg-keys (list-gpg-keys *current-user-id*)
                        :api-tokens (list-api-tokens *current-user-id*)
                        :new-token token-string))))))

(easy-routes:defroute delete-token-submit
    ("/-/settings/tokens/:token-id/delete" :method :post) ()
  (when (require-login)
    (let ((tid (parse-integer token-id :junk-allowed t)))
      (when tid (delete-api-token tid *current-user-id*)))
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute user-create-runner-token ("/-/settings/runners/token" :method :post) ()
  (when (require-login)
    (let ((token (create-registration-token :scope "user" :scope-id *current-user-id*
                                            :created-by-id *current-user-id*)))
      (html-response
       (view-settings :ssh-keys (list-ssh-keys *current-user-id*)
                      :gpg-keys (list-gpg-keys *current-user-id*)
                      :api-tokens (list-api-tokens *current-user-id*)
                      :runners (list-runners :scope "user" :scope-id *current-user-id*)
                      :registration-token token)))))

(easy-routes:defroute user-delete-runner ("/-/settings/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (let ((rid (parse-integer runner-id :junk-allowed t)))
      (when rid
        ;; Only delete if the runner belongs to this user
        (let ((runner (postmodern:query
                       (:select '* :from 'cave-runners
                        :where (:and (:= 'id rid)
                                     (:= 'scope "user")
                                     (:= 'scope-id *current-user-id*)))
                       :plist)))
          (when runner (delete-runner rid)))))
    (hunchentoot:redirect "/-/settings")))

(easy-routes:defroute download-cli ("/-/downloads/cave" :method :get) ()
  (let ((path (cli-download-path)))
    (unless path
      (setf (hunchentoot:return-code*) 404)
      (return-from download-cli "cave CLI is not installed on this host"))
    (setf (hunchentoot:header-out "Content-Disposition")
          "attachment; filename=\"cave\"")
    (hunchentoot:handle-static-file path "application/octet-stream")))

;; ----------------------------------------------------------------------------
;; Routes: Personal repo creation

(easy-routes:defroute new-personal-repo-page ("/-/new-repo" :method :get) ()
  (when (require-login)
    (html-response (view-new-personal-repo))))

(easy-routes:defroute create-personal-repo-submit ("/-/new-repo" :method :post) ()
  (when (require-login)
    (let* ((mode (or (hunchentoot:post-parameter "mode") "empty"))
           (name (hunchentoot:post-parameter "name"))
           (description (hunchentoot:post-parameter "description"))
           (is-private (hunchentoot:post-parameter "is_private"))
           (url (hunchentoot:post-parameter "url"))
           (auth-token (hunchentoot:post-parameter "auth_token"))
           (interval (parse-integer (or (hunchentoot:post-parameter "interval") "60")
                                    :junk-allowed t))
           (username (getf *current-user* :username))
           ;; Auto-derive name from URL if name is empty
           (name (if (and (or (string= mode "import") (string= mode "mirror"))
                          (or (null name) (uiop:emptyp name))
                          url (not (uiop:emptyp url)))
                     (repo-name-from-url url)
                     name)))
      (handler-case
          (let ((repo (create-repo :owner-id *current-user-id*
                                   :name name
                                   :description description
                                   :is-private (when is-private t))))
            (cond
              ((string= mode "import")
               (import-repo-from-url username name url
                                     :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                   auth-token)))
              ((string= mode "mirror")
               (import-repo-from-url username name url
                                     :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                                   auth-token))
               (create-mirror :repo-id (getf repo :id)
                              :direction "pull"
                              :remote-url url
                              :auth-token (when (and auth-token (not (uiop:emptyp auth-token)))
                                            auth-token)
                              :interval-minutes (or interval 60)))
              (t (init-bare-repo username name)))
            (log-event "repo.created" :user-id *current-user-id*
                                      :repo-id (getf repo :id)
                                      :metadata (format nil "{\"mode\": \"~A\"}" mode))
            (hunchentoot:redirect (format nil "/~A/~A" username name)))
        (error (e)
          (html-response (view-new-personal-repo :error (format nil "~A" e))))))))

;; ----------------------------------------------------------------------------
;; Routes: User profile (public repos listing)

(easy-routes:defroute user-profile-page ("/u/:username" :method :get) ()
  (let ((user (find-user-by-username username)))
    (unless user (return-from user-profile-page (not-found)))
    (let* ((is-self (and *current-user-id* (= *current-user-id* (getf user :id))))
           (repos (list-user-repos (getf user :id) :include-private is-self)))
      (html-response (view-user-profile :user user :repos repos :is-self is-self)))))

;; ----------------------------------------------------------------------------
;; Routes: Orgs (keep /o/ prefix for explicit org access)

(easy-routes:defroute org-page ("/o/:org-name" :method :get) ()
  (let ((org (find-org-by-name org-name)))
    (unless org (return-from org-page (not-found)))
    (let* ((is-member (and *current-user-id*
                           (org-member-role (getf org :id) *current-user-id*)))
           (repos (list-org-repos (getf org :id) :include-private is-member)))
      (html-response (view-org :org org :repos repos :is-member is-member
                               :is-admin (equal is-member "admin"))))))

(easy-routes:defroute org-settings-page ("/o/:org-name/-/settings" :method :get) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-settings-page (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-settings-page "Forbidden"))
      (html-response
       (view-org-settings :org org :members (list-org-members (getf org :id))
                          :runners (list-runners :scope "org" :scope-id (getf org :id)))))))

(easy-routes:defroute org-create-runner-token ("/o/:org-name/-/settings/runners/token" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-create-runner-token (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-create-runner-token "Forbidden"))
      (let ((token (create-registration-token :scope "org" :scope-id (getf org :id)
                                              :created-by-id *current-user-id*)))
        (html-response
         (view-org-settings :org org :members (list-org-members (getf org :id))
                            :runners (list-runners :scope "org" :scope-id (getf org :id))
                            :registration-token token))))))

(easy-routes:defroute org-delete-runner ("/o/:org-name/-/settings/runners/:runner-id/delete" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-delete-runner (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-delete-runner "Forbidden"))
      (let ((rid (parse-integer runner-id :junk-allowed t)))
        (when rid (delete-runner rid)))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

(easy-routes:defroute org-add-member-submit ("/o/:org-name/-/settings/members" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-add-member-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-add-member-submit "Forbidden"))
      (let* ((username (hunchentoot:post-parameter "username"))
             (role (or (hunchentoot:post-parameter "role") "member"))
             (user (find-user-by-username username)))
        (when user
          (handler-case
              (add-org-member (getf org :id) (getf user :id) :role role)
            (error () nil))))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

(easy-routes:defroute org-remove-member-submit
    ("/o/:org-name/-/settings/members/:user-id/remove" :method :post) ()
  (when (require-login)
    (let ((org (find-org-by-name org-name)))
      (unless org (return-from org-remove-member-submit (not-found)))
      (unless (equal (org-member-role (getf org :id) *current-user-id*) "admin")
        (setf (hunchentoot:return-code*) 403)
        (return-from org-remove-member-submit "Forbidden"))
      (let ((uid (parse-integer user-id :junk-allowed t)))
        (when uid (remove-org-member (getf org :id) uid)))
      (hunchentoot:redirect (format nil "/o/~A/-/settings" org-name)))))

;; ----------------------------------------------------------------------------
;; Routes: Owner (user or org) profile — single-segment catch-all

(easy-routes:defroute owner-page ("/:name" :method :get) ()
  ;; Try user first, then org
  (let ((user (find-user-by-username name)))
    (when user
      (let* ((is-self (and *current-user-id* (= *current-user-id* (getf user :id))))
             (repos (list-user-repos (getf user :id) :include-private is-self)))
        (return-from owner-page
          (html-response (view-user-profile :user user :repos repos :is-self is-self))))))
  (let ((org (find-org-by-name name)))
    (when org
      (let* ((is-member (and *current-user-id*
                             (org-member-role (getf org :id) *current-user-id*)))
             (repos (list-org-repos (getf org :id) :include-private is-member)))
        (return-from owner-page
          (html-response (view-org :org org :repos repos :is-member is-member
                                   :is-admin (equal is-member "admin")))))))
  (not-found))

