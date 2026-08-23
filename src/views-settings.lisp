(in-package #:cave)

;;; ========================== REPO SETTINGS ==========================

(defun view-repo-settings (&key owner-name repo members checks mirrors webhooks automations runners registration-token message secrets protected-branches deploy-keys)
  "Render repo settings page."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Settings — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :settings :repo repo)
      (:h1 "Repository settings")
      (when message
        (:div.alert message))

      ;; Protected branches
      (:section
       (:h2 "Protected branches")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-2)"
        "Block direct pushes (force changes through pull requests) and/or require signed commits, enforced at push time. Patterns: exact (main), prefix (release/*), or * for all.")
       (if protected-branches
           (:ul.data-list
            (dolist (p protected-branches)
              (:li (:code (getf p :pattern))
               (when (getf p :block-direct-push) (:span.badge :style "margin-left:.4rem" "no direct push"))
               (when (getf p :require-signed-commits) (:span.badge :style "margin-left:.25rem" "signed"))
               (:form :method "post" :style "display:inline;margin-left:.5rem"
                :action (format nil "/~A/~A/settings/protect/~A/delete" owner-name repo-name (getf p :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No protected branches."))
       (:form :method "post" :action (format nil "/~A/~A/settings/protect" owner-name repo-name)
        (:div.field
         (:label :for "pb_pattern" "Branch pattern")
         (:input :type "text" :id "pb_pattern" :name "pattern" :required t :placeholder "main"))
        (:label :style "display:block;font-size:.85rem"
         (:input :type "checkbox" :name "block_direct_push" :value "1" :checked t)
         " Block direct pushes (require PRs)")
        (:label :style "display:block;font-size:.85rem;margin-bottom:.5rem"
         (:input :type "checkbox" :name "require_signed_commits" :value "1")
         " Require signed commits")
        (:button.btn.btn-primary :type "submit" "Protect branch")))

      ;; Deploy keys
      (:section
       (:h2 "Deploy keys")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-2)"
        "Per-repo SSH keys for CI/deploy without a user account. Read-only by default; grant write to allow pushes.")
       (if deploy-keys
           (:ul.data-list
            (dolist (k deploy-keys)
              (:li (:strong (getf k :name)) (:code :style "margin-left:.4rem" (getf k :fingerprint))
               (:span.badge :style "margin-left:.4rem" (if (getf k :read-write) "read/write" "read-only"))
               (:form :method "post" :style "display:inline;margin-left:.5rem"
                :action (format nil "/~A/~A/settings/deploy-keys/~A/delete" owner-name repo-name (getf k :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No deploy keys."))
       (:form :method "post" :action (format nil "/~A/~A/settings/deploy-keys" owner-name repo-name)
        (:div.field
         (:label :for "dk_name" "Name")
         (:input :type "text" :id "dk_name" :name "name" :required t :placeholder "ci-deploy"))
        (:div.field
         (:label :for "dk_key" "Public key")
         (:textarea :id "dk_key" :name "public_key" :rows "3" :required t
                    :placeholder "ssh-ed25519 AAAA..."))
        (:label :style "display:block;font-size:.85rem;margin-bottom:.5rem"
         (:input :type "checkbox" :name "read_write" :value "1") " Allow write (push)")
        (:button.btn.btn-primary :type "submit" "Add deploy key")))

      ;; CI secrets
      (:section
       (:h2 "Secrets")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-2)"
        "Encrypted CI secrets, injected as environment variables into this repo's workflow jobs and masked in logs. Write-only — values can't be read back.")
       (if secrets
           (:ul.data-list
            (dolist (s secrets)
              (:li (:code s)
               (:form :method "post" :style "display:inline;margin-left:.5rem"
                :action (format nil "/~A/~A/settings/secrets/~A/delete"
                                owner-name repo-name (hunchentoot:url-encode s))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No secrets."))
       (:form :method "post"
        :action (format nil "/~A/~A/settings/secrets" owner-name repo-name)
        (:div.field
         (:label :for "secret_name" "Name")
         (:input :type "text" :id "secret_name" :name "name" :required t
                 :placeholder "GHCR_TOKEN"))
        (:div.field
         (:label :for "secret_value" "Value")
         (:input :type "password" :id "secret_value" :name "value" :required t))
        (:button.btn.btn-primary :type "submit" "Add secret")))

      ;; Merge policy
      (:section
       (:h2 "Merge policy")
       (:form :method "post" :action (format nil "/~A/~A/settings" owner-name repo-name)
        (:input :type "hidden" :name "section" :value "merge")
        (:div.field
         (:label :for "required_approvals" "Required approvals")
         (:input :type "number" :id "required_approvals" :name "required_approvals"
                 :value (princ-to-string (getf repo :required-approvals))
                 :min "0" :max "10" :style "width:5em"))
        (:div.field
         (:label
          (:input :type "checkbox" :name "allow_self_approval" :value "1"
           :checked (getf repo :allow-self-approval))
          " Allow self-approval"))
        (:div.field
         (:label
          (:input :type "checkbox" :name "allow_stale_approvals" :value "1"
           :checked (getf repo :allow-stale-approvals))
          " Allow stale approvals (don't invalidate on new commits)"))
        (:div.field
         (:label
          (:input :type "checkbox" :name "concerns_count" :value "1"
           :checked (getf repo :concerns-count-as-approval))
          " Approve-with-concerns counts as approval"))
        (:div.field
         (:label
          (:input :type "checkbox" :name "block_on_request_changes" :value "1"
           :checked (getf repo :block-on-request-changes))
          " Block merge on request-changes reviews"))
        (:div.field
         (:label
          (:input :type "checkbox" :name "auto_delete_branch" :value "1"
           :checked (getf repo :auto-delete-branch))
          " Auto-delete source branch after merge"))
        (:button.btn.btn-primary :type "submit" "Save merge policy")))

      ;; Members
      (:section
       (:h2 "Members")
       (if members
           (:ul.data-list
            (dolist (m members)
              (:li
               (:strong (getf m :username))
               (:span.badge (getf m :role))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/~A/~A/settings/members/~A/remove"
                                owner-name repo-name (getf m :user-id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No members. The repo owner has full access."))
       (:h3 "Add member")
       (:form :method "post" :action (format nil "/~A/~A/settings/members" owner-name repo-name)
        (:div :style "display:flex;gap:var(--sp-2);align-items:end"
         (:div.field :style "margin-bottom:0"
          (:label :for "member_username" "Username")
          (:input :type "text" :id "member_username" :name "username" :required t))
         (:div.field :style "margin-bottom:0"
          (:label :for "member_role" "Role")
          (:select :id "member_role" :name "role"
           (:option :value "writer" "Writer")
           (:option :value "reviewer" "Reviewer")
           (:option :value "admin" "Admin")))
         (:button.btn.btn-primary :type "submit" "Add member"))))

      ;; Automations
      (:section
       (:h2 "Automations")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Commands executed by runners on repo events.")
       (if automations
           (:ul.data-list
            (dolist (a automations)
              (:li
               (:strong (getf a :name))
               (:span.badge (getf a :trigger))
               (:code :style "margin-left:var(--sp-2)" (getf a :command))
               (when (and (getf a :runner-labels) (not (uiop:emptyp (getf a :runner-labels))))
                 (:span.badge (getf a :runner-labels)))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/~A/~A/settings/automations/~A/delete"
                                owner-name repo-name (getf a :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No automations configured."))
       (:h3 "Add automation")
       (:form :method "post" :action (format nil "/~A/~A/settings/automations" owner-name repo-name)
        (:div.field
         (:label :for "auto_name" "Name")
         (:input :type "text" :id "auto_name" :name "name" :required t
                 :placeholder "e.g. lint"))
        (:div.field
         (:label :for "auto_trigger" "Trigger")
         (:select :id "auto_trigger" :name "trigger"
          (:option :value "post_receive" "Post receive (after push)")
          (:option :value "pre_receive" "Pre receive (block push)")
          (:option :value "changeset_opened" "PR opened")
          (:option :value "changeset_updated" "PR updated")
          (:option :value "changeset_merged" "PR merged")
          (:option :value "manual" "Manual")))
        (:div.field
         (:label :for "auto_command" "Command")
         (:input :type "text" :id "auto_command" :name "command" :required t
                 :placeholder "e.g. make test"))
        (:div.field
         (:label :for "auto_labels" "Runner labels (optional)")
         (:input :type "text" :id "auto_labels" :name "runner_labels"
                 :placeholder "e.g. linux,fast"))
        (:div.field
         (:label :for "auto_timeout" "Timeout (seconds)")
         (:input :type "number" :id "auto_timeout" :name "timeout" :value "60"
                 :min "5" :max "3600" :style "width:5em"))
        (:button.btn.btn-primary :type "submit" "Add automation")))

      ;; Server-side checks
      (:section
       (:h2 "Push checks")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Commands that run on every push. Push is rejected if any check exits non-zero.")
       (if checks
           (:ul.data-list
            (dolist (chk checks)
              (:li
               (:strong (getf chk :name))
               (:code :style "margin-left:var(--sp-2)" (getf chk :command))
               (:span.badge (format nil "~As timeout" (getf chk :timeout-seconds)))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/~A/~A/settings/checks/~A/delete"
                                owner-name repo-name (getf chk :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No push checks configured."))
       (:h3 "Add check")
       (:form :method "post" :action (format nil "/~A/~A/settings/checks" owner-name repo-name)
        (:div.field
         (:label :for "check_name" "Name")
         (:input :type "text" :id "check_name" :name "name" :required t
                 :placeholder "e.g. lint"))
        (:div.field
         (:label :for "check_command" "Command")
         (:input :type "text" :id "check_command" :name "command" :required t
                 :placeholder "e.g. make lint"))
        (:div.field
         (:label :for "check_timeout" "Timeout (seconds)")
         (:input :type "number" :id "check_timeout" :name "timeout" :value "60"
                 :min "5" :max "600" :style "width:5em"))
        (:button.btn.btn-primary :type "submit" "Add check")))

      ;; Webhooks
      (:section
       (:h2 "Webhooks")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "HTTP callbacks fired on push, issue, and pull request events.")
       (if webhooks
           (:ul.data-list
            (dolist (wh webhooks)
              (:li
               (:code :style "flex:1" (getf wh :url))
               (:span.badge (getf wh :events))
               (when (and (getf wh :last-status) (not (eq (getf wh :last-status) :null)))
                 (:span.badge
                  :style (if (and (numberp (getf wh :last-status))
                                  (>= (getf wh :last-status) 200)
                                  (< (getf wh :last-status) 300))
                             "border-color:var(--green);color:var(--green)"
                             "border-color:var(--red);color:var(--red)")
                  (format nil "~A" (getf wh :last-status))))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/~A/~A/settings/webhooks/~A/delete"
                                owner-name repo-name (getf wh :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No webhooks configured."))
       (:h3 "Add webhook")
       (:form :method "post" :action (format nil "/~A/~A/settings/webhooks" owner-name repo-name)
        (:div.field
         (:label :for "wh_url" "Payload URL")
         (:input :type "text" :id "wh_url" :name "url" :required t
                 :placeholder "https://example.com/webhook"))
        (:div.field
         (:label :for "wh_secret" "Secret (optional, for HMAC signature)")
         (:input :type "password" :id "wh_secret" :name "secret"
                 :placeholder "a shared secret"))
        (:div.field
         (:label :for "wh_events" "Events (comma-separated)")
         (:input :type "text" :id "wh_events" :name "events"
                 :value "push,pull_request,issue"))
        (:button.btn.btn-primary :type "submit" "Add webhook")))

      ;; Mirrors
      (:section
       (:h2 "Mirrors")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Push mirrors copy this repo to an external host after each push. Pull mirrors fetch from an external repo periodically.")
       (if mirrors
           (:ul.data-list
            (dolist (m mirrors)
              (:li
               (:span.badge (getf m :direction))
               (:code :style "margin-left:var(--sp-2);flex:1" (getf m :remote-url))
               (when (and (getf m :last-error) (not (eq (getf m :last-error) :null)))
                 (:span.badge :style "border-color:var(--red);color:var(--red)" "error"))
               (when (and (getf m :last-sync-at) (not (eq (getf m :last-sync-at) :null)))
                 (:span :style "color:var(--text-muted);font-size:.75rem;margin-left:var(--sp-2)"
                  (format nil "last sync: ~A" (princ-to-string (getf m :last-sync-at)))))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/~A/~A/settings/mirrors/~A/delete"
                                owner-name repo-name (getf m :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No mirrors configured."))
       (:h3 "Add mirror")
       (:form :method "post" :action (format nil "/~A/~A/settings/mirrors" owner-name repo-name)
        (:div.field
         (:label :for "mirror_direction" "Direction")
         (:select :id "mirror_direction" :name "direction"
          (:option :value "push" "Push (Cave → external)")
          (:option :value "pull" "Pull (external → Cave)")))
        (:div.field
         (:label :for "mirror_url" "Remote URL")
         (:input :type "text" :id "mirror_url" :name "remote_url" :required t
                 :placeholder "https://github.com/user/repo.git"))
        (:div.field
         (:label :for "mirror_token" "Auth token (optional)")
         (:input :type "password" :id "mirror_token" :name "auth_token"
                 :placeholder "GitHub PAT or access token"))
        (:div.field
         (:label :for "mirror_interval" "Pull interval (minutes)")
         (:input :type "number" :id "mirror_interval" :name "interval" :value "60"
                 :min "5" :max "1440" :style "width:5em"))
        (:button.btn.btn-primary :type "submit" "Add mirror")))

      ;; Runners
      (:section
       (:h2 "Runners")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Runners scoped to this repository.")
       (render-runner-management runners registration-token
                                 (format nil "/~A/~A/settings/runners/token" owner-name repo-name)
                                 (format nil "/~A/~A/settings/runners" owner-name repo-name)))

      ;; Danger zone
      (:section
       (:h2 :style "color:var(--red)" "Danger zone")
       (:div :style "border:1px solid var(--red);border-radius:var(--radius);padding:var(--sp-4)"
        (if (getf repo :is-private)
            (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-4)"
             (:div
              (:strong "Make this repository public")
              (:p :style "color:var(--text-muted);font-size:.85rem;margin:0"
               "Anyone will be able to see and search this repository."))
             (:form :method "post"
              :action (format nil "/~A/~A/settings/visibility" owner-name repo-name)
              (:button.btn :type "submit" "Make public")))
            (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-4)"
             (:div
              (:strong "Make this repository private")
              (:p :style "color:var(--text-muted);font-size:.85rem;margin:0"
               "Only members will be able to see this repository."))
             (:form :method "post"
              :action (format nil "/~A/~A/settings/visibility" owner-name repo-name)
              (:input :type "hidden" :name "private" :value "true")
              (:button.btn :type "submit" :style "border-color:var(--red);color:var(--red)"
               "Make private"))))
        (if (getf repo :is-archived)
            (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-4)"
             (:div
              (:strong "Unarchive this repository")
              (:p :style "color:var(--text-muted);font-size:.85rem;margin:0"
               "This will make the repository writable again."))
             (:form :method "post"
              :action (format nil "/~A/~A/settings/unarchive" owner-name repo-name)
              (:button.btn :type "submit" "Unarchive")))
            (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-4)"
             (:div
              (:strong "Archive this repository")
              (:p :style "color:var(--text-muted);font-size:.85rem;margin:0"
               "Mark as read-only. No new pushes, issues, or PRs."))
             (:form :method "post"
              :action (format nil "/~A/~A/settings/archive" owner-name repo-name)
              (:button.btn :type "submit" :style "border-color:var(--red);color:var(--red)"
               "Archive"))))
        (:div :style "display:flex;justify-content:space-between;align-items:center"
         (:div
          (:strong "Delete this repository")
          (:p :style "color:var(--text-muted);font-size:.85rem;margin:0"
           "Permanently delete this repository and all its data."))
         (:form :method "post"
          :action (format nil "/~A/~A/settings/delete" owner-name repo-name)
          (:button.btn :type "submit" :style "border-color:var(--red);color:var(--red)"
           "Delete repository"))))))))

;;; ========================== ADMIN & SETTINGS ==========================

(defun view-admin (&key users pending-users runners registration-token message)
  "Render the admin panel."
  (page (:title "Admin — Cave")
    (:h1 "Instance administration")
    (when pending-users
      (:section
       (:h2 (format nil "Pending approval (~D)" (length pending-users)))
       (:p :style "color:var(--text-muted);font-size:.9rem;margin-bottom:var(--sp-2)"
        "Self-registered users waiting for you to let them in.")
       (:table.data-table
        (:thead (:tr (:th "Username") (:th "Email") (:th "Display name") (:th "Signed up") (:th "")))
        (:tbody
         (dolist (u pending-users)
           (:tr
            (:td (getf u :username))
            (:td (getf u :email))
            (:td (getf u :display-name))
            (:td (princ-to-string (getf u :created-at)))
            (:td
             (:form :method "post" :style "display:inline;margin-right:.5rem"
              :action (format nil "/-/admin/users/~A/approve" (getf u :id))
              (:button.btn.btn-sm.btn-primary :type "submit" "Approve"))
             (:form :method "post" :style "display:inline"
              :action (format nil "/-/admin/users/~A/reject" (getf u :id))
              (:button.btn.btn-sm :type "submit" "Reject")))))))))
    (:section
     (:h2 "Users")
     (when message
       (:div.alert :style "border:1px solid var(--primary);padding:.5rem .75rem;margin-bottom:1rem"
        message))
     (:table.data-table
      (:thead (:tr (:th "Username") (:th "Admin") (:th "Active") (:th "Approval") (:th "Created")))
      (:tbody
       (dolist (u users)
         (:tr
          (:td (getf u :username))
          (:td (if (getf u :is-admin) "yes" "no"))
          (:td (if (getf u :is-active) "yes" "no"))
          (:td (or (getf u :approval-status) "approved"))
          (:td (princ-to-string (getf u :created-at)))))))

    (:section
     (:h2 "Runners")
     (if runners
         (:table.data-table
          (:thead (:tr (:th "Name") (:th "Scope") (:th "Labels") (:th "Status") (:th "Last seen") (:th "")))
          (:tbody
           (dolist (r runners)
             (:tr
              (:td (getf r :name))
              (:td (:span.badge (getf r :scope)))
              (:td (let ((l (getf r :labels)))
                     (if (and l (not (uiop:emptyp l)) (not (eq l :null))) l "")))
              (:td (:span.badge
                    :style (cond ((equal (getf r :status) "online")
                                   "border-color:var(--green);color:var(--green)")
                                  ((equal (getf r :status) "disabled")
                                   "border-color:var(--red);color:var(--red)")
                                  (t ""))
                    (getf r :status)))
              (:td :style "color:var(--text-muted);font-size:.75rem"
               (let ((ls (getf r :last-seen-at)))
                 (if (and ls (not (eq ls :null))) (princ-to-string ls) "never")))
              (:td
               (:form :method "post" :style "display:inline"
                :action (format nil "/-/admin/runners/~A/delete" (getf r :id))
                (:button.btn.btn-sm :type "submit" "Delete")))))))
         (:p.empty "No runners registered."))
     (when registration-token
       (:div.alert :style "border:1px solid var(--accent);padding:.75rem;margin:1rem 0"
        (:strong "Registration token created.") " Use this to register a runner:" (:br)
        (:code :style "word-break:break-all" (getf registration-token :token))
        (:p :style "margin-top:.5rem;color:var(--text-muted);font-size:.85rem"
         "Run: " (:code (format nil "cave-server runner --url grpc://localhost:~A --token ~A"
                                (config-value :grpc-port 9443)
                                (getf registration-token :token))))))
     (:form :method "post" :action "/-/admin/runners/token"
      (:button.btn.btn-primary :type "submit" "Generate registration token"))))))

(defun view-change-password (&key error success)
  "Render the self-service change-password page (behind sudo)."
  (page (:title "Change password — Cave")
    (:h1 "Change password")
    (when error
      (:p :style "color:var(--danger,#c0392b)" error))
    (when success
      (:p :style "color:var(--success,#27ae60)" "Your password has been changed."))
    (:section
     (:form :method "post" :action "/-/settings/password"
      (:div.field
       (:label :for "new_password" "New password")
       (:input :id "new_password" :name "new_password" :type "password"
               :autocomplete "new-password" :minlength "8" :required t))
      (:div.field
       (:label :for "confirm_password" "Confirm new password")
       (:input :id "confirm_password" :name "confirm_password" :type "password"
               :autocomplete "new-password" :minlength "8" :required t))
      (:button.btn :type "submit" "Change password")))
    (:p (:a :href "/-/settings" "← Back to settings"))))

(defun view-register (&key error username email)
  "Self-service registration form."
  (page (:title "Register — Cave")
    (:h1 "Create an account")
    (when error (:p :style "color:var(--danger,#c0392b)" error))
    (:p :style "color:var(--text-muted);font-size:.9rem"
     "New accounts require administrator approval before you can sign in.")
    (:section
     (:form :method "post" :action "/-/register"
      (:div.field
       (:label :for "username" "Username")
       (:input :id "username" :name "username" :value (or username "") :autofocus t :required t))
      (:div.field
       (:label :for "email" "Email")
       (:input :id "email" :name "email" :type "email" :value (or email "")))
      (:div.field
       (:label :for "password" "Password")
       (:input :id "password" :name "password" :type "password"
               :minlength "8" :autocomplete "new-password" :required t))
      (:button.btn.btn-primary :type "submit" "Create account")))
    (:p "Already have an account? " (:a :href "/-/auth/login" "Sign in"))))

(defun view-totp (&key enabled)
  "TOTP status page."
  (page (:title "Two-factor — Cave")
    (:h1 "Two-factor authentication")
    (if enabled
        (progn
          (:p :style "color:var(--success,#27ae60)"
           "Two-factor authentication is enabled on your account.")
          (:section
           (:form :method "post" :action "/-/settings/totp/backup-codes"
                  :style "display:inline"
            (:button.btn :type "submit" "Regenerate backup codes"))
           (:form :method "post" :action "/-/settings/totp/disable"
                  :style "display:inline;margin-left:var(--sp-2)"
            (:button.btn :type "submit" "Disable two-factor"))))
        (progn
          (:p "Protect your account with a time-based one-time password (TOTP) "
              "from an authenticator app (Google Authenticator, Aegis, 1Password, etc.).")
          (:form :method "post" :action "/-/settings/totp/enroll"
           (:button.btn :type "submit" "Enable two-factor"))))
    (:p :style "margin-top:var(--sp-3)" (:a :href "/-/settings" "← Back to settings"))))

(defun view-totp-enroll (&key qr secret error)
  "TOTP enrollment page: QR + secret + confirm form."
  (page (:title "Enable two-factor — Cave")
    (:h1 "Enable two-factor authentication")
    (when error (:p :style "color:var(--danger,#c0392b)" error))
    (:section
     (:p "1. Scan this QR code with your authenticator app, or enter the secret manually:")
     (when qr (:p (:img :src qr :alt "TOTP QR code"
                        :style "width:220px;height:220px;image-rendering:pixelated")))
     (when secret (:p "Secret: " (:code secret)))
     (:p :style "margin-top:var(--sp-3)" "2. Enter the 6-digit code to confirm:")
     (:form :method "post" :action "/-/settings/totp/confirm"
      (:div.field
       (:input :name "code" :inputmode "numeric" :pattern "[0-9]*" :placeholder "123456"
               :autocomplete "one-time-code" :autofocus t :required t))
      (:button.btn :type "submit" "Confirm")))
    (:p (:a :href "/-/settings/totp" "Cancel"))))

(defun view-totp-backup-codes (&key codes enabled-now)
  "One-time display of backup codes."
  (page (:title "Backup codes — Cave")
    (:h1 "Backup codes")
    (when enabled-now
      (:p :style "color:var(--success,#27ae60)"
       "Two-factor authentication is now enabled."))
    (:p "Save these single-use backup codes somewhere safe — each works once if "
        "you lose access to your authenticator. They are shown only now.")
    (:section
     (:ul :style "font-family:monospace;font-size:1.1rem;line-height:1.9;list-style:none;padding-left:0"
      (dolist (c codes) (:li c))))
    (:p (:a.btn :href "/-/settings/totp" "Done"))))

(defun view-notifications (&key notifications)
  "Render the in-app notification feed."
  (page (:title "Notifications — Cave")
    (:div :style "display:flex;justify-content:space-between;align-items:center"
     (:h1 "Notifications")
     (when notifications
       (:form :method "post" :action "/-/notifications/read" :style "margin:0"
        (:button.btn.btn-sm :type "submit" "Mark all read"))))
    (if notifications
        (:ul.data-list
         (dolist (n notifications)
           (:li :style (if (getf n :is-read)
                           "opacity:.6"
                           "border-left:3px solid var(--primary,#7c9a5e);padding-left:.5rem")
            (:a :href (format nil "/-/notifications/~A/go" (getf n :id))
             (getf n :subject))
            (:span :style "color:var(--text-muted);font-size:.8rem;margin-left:.5rem"
             (or (format-relative-time (getf n :created-at)) "")))))
        (:p.empty "You're all caught up — no notifications."))))

(defun view-settings (&key ssh-keys gpg-keys api-tokens new-token ssh-error gpg-error
                           generated-private-key generated-key-name
                           runners registration-token)
  "Render user settings page."
  (let ((cli-path (cli-download-path)))
    (page (:title "Settings — Cave")
      (:h1 "Settings")

      (:section
       (:h2 "Theme")
       (:form :method "post" :action "/-/settings/theme"
        (:div :style "display:flex;gap:var(--sp-2);align-items:end"
         (:div.field :style "margin-bottom:0"
          (:label :for "theme" "Color theme")
          (:select :id "theme" :name "theme"
           (dolist (name '("terminal-warmth" "solarized-dark" "nord" "dracula" "light"))
             (:option :value name
              :selected (equal name (effective-theme))
              name))
           ;; Custom themes
           (dolist (ct (list-user-themes *current-user-id*))
             (:option :value (getf ct :name)
              :selected (equal (getf ct :name) (getf *current-user* :theme))
              (format nil "~A (custom)" (getf ct :name))))))
         (:button.btn.btn-primary :type "submit" "Apply"))))

      (:section
       (:h2 "Security")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Change your password or manage two-factor authentication. (Re-authentication is required.)")
       (:a.btn :href "/-/settings/password" "Change password")
       (:a.btn :href "/-/settings/totp" :style "margin-left:var(--sp-2)"
        "Two-factor authentication"))

      (:section
       (:h2 "CLI")
       (if cli-path
           (progn
             (:p "Download the cave CLI for issue and API workflows.")
             (:p
              (:a.btn.btn-primary :href "/-/downloads/cave" "Download cave"))
             (:p :style "color:var(--text-muted);font-size:.85rem"
              "Save it somewhere on your PATH and run " (:code "chmod +x cave") "."))
           (:p.empty "cave CLI is not installed on this host yet."))
       (:pre :style "background:var(--surface);padding:1rem;border-radius:var(--radius);border:1px solid var(--border);font-size:.85rem;overflow-x:auto"
        "export CAVE_BASE_URL=" (config-value :base-url)
        "
export CAVE_TOKEN=<your-api-token>
./cave --repo OWNER/REPO issue list"))

      (:section
       (:h2 "SSH keys")
       (if ssh-keys
           (:ul.data-list
            (dolist (k ssh-keys)
              (:li
               (:strong (getf k :name))
               (:code (getf k :fingerprint))
               (:form :method "post" :style "display:inline"
                :action (format nil "/-/settings/ssh-keys/~A/delete" (getf k :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No SSH keys registered."))

       ;; Show generated private key for download (one-time)
       (when generated-private-key
         (:div.alert :style "border:1px solid var(--primary);padding:1rem;margin:1rem 0"
          (:strong (format nil "SSH key '~A' generated." generated-key-name))
          " Save this private key now — it will not be shown again."
          (:pre :style "background:var(--bg);padding:.75rem;border-radius:var(--radius);margin-top:.75rem;font-size:.8rem;overflow-x:auto;white-space:pre-wrap;word-break:break-all"
           generated-private-key)
          (:p :style "margin-top:.75rem;color:var(--text-muted);font-size:.85rem"
           "Save to " (:code "~/.ssh/cave_ed25519") " and run: "
           (:code "chmod 600 ~/.ssh/cave_ed25519"))))

       ;; Generate keypair
       (:h3 "Generate SSH key")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:.5rem"
        "Generate an ed25519 keypair. The public key is stored here; you download the private key.")
       (:form :method "post" :action "/-/settings/ssh-keys/generate"
        (:div.field
         (:label :for "gen_name" "Key name")
         (:input :type "text" :id "gen_name" :name "name" :required t
                 :placeholder "e.g. laptop"))
        (:button.btn.btn-primary :type "submit" "Generate keypair"))

       ;; Or paste existing
       (:h3 "Add existing SSH key")
       (when ssh-error (:div.alert.alert-error ssh-error))
       (:form :method "post" :action "/-/settings/ssh-keys"
        (:div.field
         (:label :for "key_name" "Name")
         (:input :type "text" :id "key_name" :name "name" :required t
                 :placeholder "e.g. work-laptop"))
        (:div.field
         (:label :for "public_key" "Public key")
         (:textarea :id "public_key" :name "public_key" :rows "4" :required t
                    :placeholder "ssh-ed25519 AAAA..."))
        (:button.btn :type "submit" "Add key")))

      (:section
       (:h2 "GPG keys")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Register a GPG public key so your GPG-signed commits show as Verified.")
       (if gpg-keys
           (:ul.data-list
            (dolist (k gpg-keys)
              (:li
               (:strong (getf k :name))
               (:code (getf k :key-id))
               (:form :method "post" :style "display:inline"
                :action (format nil "/-/settings/gpg-keys/~A/delete" (getf k :id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No GPG keys registered."))

       (:h3 "Add GPG key")
       (when gpg-error (:div.alert.alert-error gpg-error))
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:.5rem"
        "Export with " (:code "gpg --armor --export <keyid>") " and paste the block below.")
       (:form :method "post" :action "/-/settings/gpg-keys"
        (:div.field
         (:label :for "gpg_key_name" "Name")
         (:input :type "text" :id "gpg_key_name" :name "name" :required t
                 :placeholder "e.g. signing key"))
        (:div.field
         (:label :for "gpg_public_key" "Public key")
         (:textarea :id "gpg_public_key" :name "public_key" :rows "6" :required t
                    :placeholder "-----BEGIN PGP PUBLIC KEY BLOCK-----"))
        (:button.btn :type "submit" "Add key")))

      ;; Runners
      (:section
       (:h2 "Runners")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Runners scoped to your personal repositories.")
       (render-runner-management runners registration-token
                                 "/-/settings/runners/token"
                                 "/-/settings/runners"))

      (:section
       (:h2 "API tokens")
       (when new-token
         (:div.alert :style "border:1px solid var(--primary);padding:.75rem;margin-bottom:1rem"
          (:strong "New token created.") " Copy it now — you won't see it again:" (:br)
          (:code :style "word-break:break-all" new-token)))
       (if api-tokens
           (:ul.data-list
            (dolist (tok api-tokens)
              (:li
               (:strong (getf tok :name))
               (:code (format nil "~A..." (getf tok :token-prefix)))
               (:form :method "post" :style "display:inline"
                :action (format nil "/-/settings/tokens/~A/delete" (getf tok :id))
                (:button.btn.btn-sm :type "submit" "Revoke")))))
           (:p.empty "No API tokens."))
       (:h3 "Create API token")
       (:form :method "post" :action "/-/settings/tokens"
        (:div.field
         (:label :for "token_name" "Name")
         (:input :type "text" :id "token_name" :name "name" :required t
                 :placeholder "e.g. ci-bot"))
        (:button.btn.btn-primary :type "submit" "Create token"))))))

;;; --- Search Results ---

(defun view-search-results (&key query repo-scope results)
  "Render the search results page."
  (page (:title (if (string= query "") "Search" (format nil "~A — Search" query)))
    (:h1 "Search")
    (:form.search-form :method "get" :action "/-/search"
     (:div.search-bar
      (:input.search-input :type "text" :name "q" :value query
       :placeholder "Search code..." :autofocus "autofocus")
      (when repo-scope
        (:input :type "hidden" :name "repo" :value repo-scope))
      (:button.btn.btn-primary :type "submit" "Search"))
     (when repo-scope
       (:div.search-scope
        (:span "Searching in ")
        (:a :href (format nil "/~A" repo-scope) repo-scope)
        " "
        (:a.btn.btn-sm :href (format nil "/-/search?q=~A"
                               (hunchentoot:url-encode query))
         "Search all repos"))))
    (when (getf results :error)
      (:div.alert (:raw (spinneret:escape-string (getf results :error)))))
    (let ((files (getf results :files)))
      (cond
        ((string= query "")
         nil)
        ((null files)
         (:p.text-muted "No results found."))
        (t
         (:p.search-stats
          (format nil "~A file~:P matched" (length files)))
         (dolist (fm files)
           (let ((repo (getf fm :repository))
                 (file (getf fm :file-name))
                 (lang (getf fm :language))
                 (matches (getf fm :matches)))
             (:details.search-result :open "open"
              (:summary.search-result-header
               (:span.search-chevron)
               (:a.search-result-link
                :href (format nil "/~A/blob/HEAD?path=~A&search=~A"
                        repo (hunchentoot:url-encode file)
                        (hunchentoot:url-encode query))
                (:span.search-repo repo)
                " / "
                (:span.search-file file))
               (unless (string= lang "")
                 (:span.search-lang lang)))
              (:raw (render-search-code-table matches repo file query))))))))))

(defun render-search-code-table (matches repo file query)
  "Render the code matches table as an HTML string with no Spinneret whitespace.
   Line numbers link to the blob page at that line with the search term."
  (let ((base-url (format nil "/~A/blob/HEAD?path=~A&search=~A"
                          repo
                          (hunchentoot:url-encode file)
                          (hunchentoot:url-encode query))))
  (with-output-to-string (s)
    (write-string "<table class=\"search-code\">" s)
    (dolist (m matches)
      (let ((line-num (getf m :line-num))
            (before (getf m :before-lines))
            (after (getf m :after-lines))
            (fragments (getf m :fragments)))
        ;; Before context
        (let ((ctx-start (- line-num (length before))))
          (loop for ctx-line in before
                for n from ctx-start
                do (format s "<tr class=\"search-ctx\" data-href=\"~A&line=~A\"><td class=\"line-num\">~A</td><td class=\"line-content\">~A</td></tr>"
                           base-url n n (spinneret:escape-string
                                         (string-right-trim '(#\Newline #\Return) ctx-line)))))
        ;; Matched line
        (format s "<tr class=\"search-match\" data-href=\"~A&line=~A\"><td class=\"line-num\">~A</td><td class=\"line-content\">"
                base-url line-num line-num)
        (dolist (frag fragments)
          (let ((pre (string-right-trim '(#\Newline #\Return) (getf frag :pre)))
                (match (string-right-trim '(#\Newline #\Return) (getf frag :match)))
                (post (string-right-trim '(#\Newline #\Return) (getf frag :post))))
            (write-string (spinneret:escape-string pre) s)
            (format s "<mark>~A</mark>" (spinneret:escape-string match))
            (write-string (spinneret:escape-string post) s)))
        (write-string "</td></tr>" s)
        ;; After context
        (loop for ctx-line in after
              for n from (1+ line-num)
              do (format s "<tr class=\"search-ctx\" data-href=\"~A&line=~A\"><td class=\"line-num\">~A</td><td class=\"line-content\">~A</td></tr>"
                         base-url n n (spinneret:escape-string
                                       (string-right-trim '(#\Newline #\Return) ctx-line))))
        ;; Separator
        (unless (eq m (car (last matches)))
          (write-string "<tr class=\"search-sep\"><td colspan=\"2\"></td></tr>" s))))
    (write-string "</table>" s)
    (write-string "<script>document.querySelectorAll('.search-code tr[data-href]').forEach(function(tr){tr.style.cursor='pointer';tr.addEventListener('click',function(){window.location=tr.dataset.href})})</script>" s))))
