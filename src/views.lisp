;;; views.lisp — HTML views using Spinneret
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; All HTML generation lives here. Each page is a function that returns
;;; an HTML string. No template files, no compilation step, no stale state.

(defun gravatar-url (email &key (size 20))
  "Return a Gravatar URL for EMAIL. Falls back to identicon."
  (let* ((clean (string-trim '(#\Space) (string-downcase (or email ""))))
         (hash (ironclad:byte-array-to-hex-string
                (ironclad:digest-sequence :md5
                 (flexi-streams:string-to-octets clean :external-format :utf-8)))))
    (format nil "https://gravatar.com/avatar/~A?d=identicon&s=~A" hash size)))

(defun render-avatar (email &key (size 20) (class "avatar"))
  "Render a Gravatar img element."
  (spinneret:with-html
    (:img :src (gravatar-url email :size size)
     :class class :width (princ-to-string size) :height (princ-to-string size))))

(defmacro page ((&key title) &body body)
  "Wrap BODY in a full HTML page with nav and container."
  `(spinneret:with-html-string
     (:doctype)
     (:html :lang "en"
            :data-theme (when *current-user* (getf *current-user* :theme))
       (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title (or ,title "Cave"))
        (:link :rel "icon" :href "/static/img/favicon.svg" :type "image/svg+xml")
        (:link :rel "preconnect" :href "https://fonts.googleapis.com")
        (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "")
        (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,600;0,700;1,400&family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;1,400&display=swap")
        (:link :rel "stylesheet" :href "/static/css/cave.css")
        ;; Inject custom theme CSS if active
        (when *current-user*
          (let ((theme-css (get-user-theme-css *current-user-id*
                                               (getf *current-user* :theme))))
            (when theme-css
              (:style (:raw theme-css))))))
       (:body
        (:nav.nav
         (:div.nav-inner
          (:a.nav-brand :href "/" "Cave")
          (:div.nav-right
           (if *current-user*
               (progn
                 (when (config-value :zoekt-enabled)
                   (:form.nav-search :method "get" :action "/-/search"
                    (:input.nav-search-input :type "text" :name "q"
                     :placeholder "Search code..." :autocomplete "off")))
                 (:a.btn.btn-sm :href "/-/new-org" "New org")
                 (:a.btn.btn-sm :href "/-/settings" "Settings")
                 (when (getf *current-user* :is-admin)
                   (:a.btn.btn-sm :href "/-/admin" "Admin"))
                 (render-avatar (getf *current-user* :email) :size 20)
                 (:span.nav-user (getf *current-user* :username))
                 (:form :method "post" :action "/logout" :style "display:inline"
                  (:button.btn.btn-sm :type "submit" "Log out")))
               (:a.btn.btn-sm :href "/-/auth/login" "Log in")))))
        (:main.container ,@body)
        (:footer.site-footer
         (:span (format nil "Cave ~A" +version+)))))))

;;; --- Breadcrumb helper ---

(defun render-breadcrumbs (crumbs)
  "Render breadcrumb navigation. CRUMBS is a list of (url text) pairs, last is just text."
  (spinneret:with-html
    (:nav.breadcrumb
     (loop for (crumb . rest) on crumbs
           do (if rest
                  (progn
                    (:a :href (first crumb) (second crumb))
                    (:raw " / "))
                  (:strong (if (listp crumb) (second crumb) crumb)))))))

;;; ========================== AUTH PAGES ==========================
;;; Login is handled by Keycloak — no local login form needed.

;;; ========================== DASHBOARD ==========================

(defun format-relative-time (ts &key (now (get-universal-time)))
  "Format a timestamp as 'N units ago'. TS may be a universal-time integer or NIL."
  (when (integerp ts)
    (let ((delta (max 0 (- now ts))))
      (cond
        ((< delta 60) "just now")
        ((< delta 3600)
         (let ((m (floor delta 60))) (format nil "~D minute~:P ago" m)))
        ((< delta 86400)
         (let ((h (floor delta 3600))) (format nil "~D hour~:P ago" h)))
        ((< delta (* 86400 30))
         (let ((d (floor delta 86400))) (format nil "~D day~:P ago" d)))
        ((< delta (* 86400 365))
         (let ((mo (floor delta (* 86400 30)))) (format nil "~D month~:P ago" mo)))
        (t (let ((y (floor delta (* 86400 365)))) (format nil "~D year~:P ago" y)))))))

(defun event-metadata (event)
  "Parse the JSON metadata column into a hash table, or NIL."
  (let ((md (getf event :metadata)))
    (when (and md (not (eq md :null)) (stringp md) (plusp (length md)))
      (handler-case (com.inuoe.jzon:parse md) (error () nil)))))

(defun metadata-get (md key)
  "Lookup KEY (string) in a parsed-jzon hash table; return NIL if absent/null."
  (when (hash-table-p md)
    (let ((v (gethash key md)))
      (unless (eq v 'null) v))))

(defun short-ref (ref)
  "Strip refs/heads/ or refs/tags/ from REF."
  (cond
    ((null ref) nil)
    ((uiop:string-prefix-p "refs/heads/" ref) (subseq ref 11))
    ((uiop:string-prefix-p "refs/tags/" ref) (subseq ref 10))
    (t ref)))

(defun format-push-event (actor md)
  "Render a git.push event from its parsed metadata."
  (let* ((ref (short-ref (metadata-get md "ref")))
         (count (metadata-get md "count"))
         (tip (metadata-get md "tip"))
         (created (metadata-get md "created"))
         (deleted (metadata-get md "deleted")))
    (cond
      ((not ref)
       (format nil "~A pushed" actor))
      (deleted
       (format nil "~A deleted ~A" actor ref))
      (created
       (if (and count (numberp count))
           (format nil "~A created ~A (~D commit~:P)" actor ref count)
           (format nil "~A created ~A" actor ref)))
      ((and count (numberp count) tip)
       (format nil "~A pushed ~D commit~:P to ~A: ~A" actor count ref tip))
      ((and count (numberp count))
       (format nil "~A pushed ~D commit~:P to ~A" actor count ref))
      (t
       (format nil "~A pushed to ~A" actor ref)))))

(defun format-event (event)
  "Format an event as a short English sentence."
  (let ((type (getf event :event-type))
        (actor (or (getf event :actor) "someone"))
        (md (event-metadata event)))
    (cond
      ((equal type "issue.created") (format nil "~A opened an issue" actor))
      ((equal type "review.submitted") (format nil "~A submitted a review" actor))
      ((equal type "pr.merged") (format nil "~A merged a pull request" actor))
      ((equal type "repo.created") (format nil "~A created a repository" actor))
      ((equal type "repo.forked") (format nil "~A forked a repository" actor))
      ((equal type "changeset.merged") (format nil "~A merged a changeset" actor))
      ((equal type "git.push") (format-push-event actor md))
      ((equal type "git.clone") (format nil "~A cloned" actor))
      (t (format nil "~A: ~A" actor type)))))

(defun view-public-landing (&key repos events)
  "Anonymous landing — list public repos so visitors can browse without an account."
  (page (:title "Cave")
    (:section
     (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-3)"
      (:h2 "Public repositories")
      (:span
       (:a.btn :href "/-/auth/login" "Log in")
       " "
       (:a.btn.btn-primary :href "/-/auth/login" "Register")))
     (:p :style "color:var(--text-muted);margin-bottom:var(--sp-4)"
      "Browse public projects below. To create your own, log in or register.")
     (if repos
         (:ul.repo-list
          (dolist (repo repos)
            (let ((pushed (or (getf repo :last-pushed-at)
                              (getf repo :updated-at)))
                  (owner (getf repo :owner-name)))
              (:li
               (:a :href (format nil "/~A/~A" owner (getf repo :name))
                (format nil "~A/~A" owner (getf repo :name)))
               (when (getf repo :description)
                 (:span.desc (getf repo :description)))
               (when (format-relative-time pushed)
                 (:span.repo-meta :style "margin-left:auto;color:var(--text-muted);font-size:.85rem"
                  "Updated " (format-relative-time pushed)))))))
         (:p.empty "No public repositories yet.")))
    (when events
      (:section
       (:h2 "Recent activity")
       (:ul.issue-list
        (dolist (ev events)
          (:li
           (:span (format-event ev))
           (let ((rel (format-relative-time (getf ev :created-at))))
             (when rel
               (:span :style "color:var(--text-muted);font-size:.85rem;margin-left:.5rem" rel)))
           (when (and (getf ev :repo-name) (not (eq (getf ev :repo-name) :null)))
             (:span.badge :style "margin-left:auto" (getf ev :repo-name))))))))))

(defun view-dashboard (&key orgs repos username events)
  "Render the dashboard."
  (page (:title "Dashboard — Cave")
    (:h1 "Dashboard")

    (:section
     (:div :style "display:flex;justify-content:space-between;align-items:center"
      (:h2 "Your repositories")
      (:a.btn.btn-primary :href "/-/new-repo" "New repository"))
     (if repos
         (:ul.repo-list
          (dolist (repo repos)
            (let ((pushed (or (getf repo :last-pushed-at)
                              (getf repo :updated-at))))
              (:li
               (:a :href (format nil "/~A/~A" username (getf repo :name))
                (getf repo :name))
               (when (getf repo :is-private)
                 (:span.badge "private"))
               (when (getf repo :description)
                 (:span.desc (getf repo :description)))
               (when (format-relative-time pushed)
                 (:span.repo-meta :style "margin-left:auto;color:var(--text-muted);font-size:.85rem"
                  "Updated " (format-relative-time pushed)))))))
         (:p.empty "No personal repositories yet.")))

    (:section
     (:h2 "Your organizations")
     (if orgs
         (:ul.org-list
          (dolist (org orgs)
            (:li
             (:a :href (format nil "/o/~A" (getf org :name))
              (getf org :display-name))
             (:span.badge (getf org :role)))))
         (:p.empty "You're not a member of any organization yet.")))

    (when events
      (:section
       (:h2 "Recent activity")
       (:ul.issue-list
        (dolist (ev events)
          (:li
           (:span (format-event ev))
           (let ((rel (format-relative-time (getf ev :created-at))))
             (when rel
               (:span :style "color:var(--text-muted);font-size:.85rem;margin-left:.5rem" rel)))
           (when (and (getf ev :repo-name) (not (eq (getf ev :repo-name) :null)))
             (:span.badge :style "margin-left:auto" (getf ev :repo-name))))))))))

;;; ========================== PERSONAL REPO CREATION ==========================

(defun render-new-repo-form (action &key error)
  "Render the tabbed new-repo form (Empty / Import / Mirror)."
  (spinneret:with-html
  (when error (:div.alert.alert-error error))
  (:div.repo-create-tabs
   (:raw "<span class=\"repo-tab-active\" data-mode=\"empty\">Empty</span>")
   (:raw "<span class=\"repo-tab\" data-mode=\"import\">Import from URL</span>")
   (:raw "<span class=\"repo-tab\" data-mode=\"mirror\">Mirror</span>"))
  (:form :method "post" :action action
   (:input :type "hidden" :id "repo-mode" :name "mode" :value "empty")
   ;; URL fields (import + mirror only)
   (:div.field.import-field :style "display:none"
    (:label :for "url" "Repository URL")
    (:input :type "text" :id "url" :name "url"
            :placeholder "https://github.com/user/repo.git"))
   (:div.field.import-field :style "display:none"
    (:label :for "auth_token" "Access token (optional)")
    (:input :type "password" :id "auth_token" :name "auth_token"
            :placeholder "GitHub PAT or access token"))
   ;; Mirror-only: interval
   (:div.field.mirror-field :style "display:none"
    (:label :for "interval" "Sync interval (minutes)")
    (:input :type "number" :id "interval" :name "interval" :value "60"
            :min "5" :max "1440"))
   ;; Common fields
   (:div.field
    (:label :for "name" "Repository name")
    (:input :type "text" :id "name" :name "name"
            :pattern "[a-zA-Z0-9._-]+"
            :placeholder "Leave blank to derive from URL"))
   (:div.field
    (:label :for "description" "Description (optional)")
    (:input :type "text" :id "description" :name "description"))
   (:div.field
    (:label (:input :type "checkbox" :name "is_private" :value "1") " Private"))
   (:button.btn.btn-primary :type "submit" "Create repository"))
  (:script (:raw "
document.querySelectorAll('.repo-tab,.repo-tab-active').forEach(function(tab) {
  tab.addEventListener('click', function() {
    document.querySelectorAll('.repo-tab,.repo-tab-active').forEach(function(t) { t.className = 'repo-tab'; });
    tab.className = 'repo-tab-active';
    var mode = tab.dataset.mode;
    document.getElementById('repo-mode').value = mode;
    var showImport = mode === 'import' || mode === 'mirror';
    document.querySelectorAll('.import-field').forEach(function(f) { f.style.display = showImport ? '' : 'none'; });
    document.querySelectorAll('.mirror-field').forEach(function(f) { f.style.display = mode === 'mirror' ? '' : 'none'; });
    var nameInput = document.getElementById('name');
    nameInput.required = mode === 'empty';
    nameInput.placeholder = mode === 'empty' ? '' : 'Leave blank to derive from URL';
  });
});"))
  ))

(defun view-new-personal-repo (&key error)
  "Render the personal repo creation form."
  (page (:title "New repository — Cave")
    (:h1 "New repository")
    (render-new-repo-form "/-/new-repo" :error error)))

;;; ========================== USER PROFILE ==========================

(defun view-user-profile (&key user repos is-self)
  "Render a user's public profile / repo listing."
  (let ((username (getf user :username)))
    (page (:title (format nil "~A — Cave" username))
      (:h1 username)
      (when is-self
        (:a.btn.btn-primary :href "/-/new-repo" "New repository"))
      (:h2 "Repositories")
      (if repos
          (:ul.repo-list
           (dolist (repo repos)
             (:li
              (:a :href (format nil "/~A/~A" username (getf repo :name))
               (getf repo :name))
              (when (getf repo :is-private) (:span.badge "private"))
              (when (getf repo :description) (:span.desc (getf repo :description))))))
          (:p.empty "No repositories.")))))

;;; ========================== ORG PAGES ==========================

(defun view-new-org (&key error)
  "Render the new org form."
  (page (:title "New organization — Cave")
    (:h1 "Create organization")
    (when error (:div.alert.alert-error error))
    (:form :method "post" :action "/-/new-org"
     (:div.field
      (:label :for "name" "Name (URL-safe, lowercase)")
      (:input :type "text" :id "name" :name "name" :required t
              :pattern "[a-z0-9][a-z0-9._-]*" :autofocus t))
     (:div.field
      (:label :for "display_name" "Display name")
      (:input :type "text" :id "display_name" :name "display_name"))
     (:div.field
      (:label :for "description" "Description (optional)")
      (:input :type "text" :id "description" :name "description"))
     (:button.btn.btn-primary :type "submit" "Create organization"))))

(defun view-org (&key org repos is-member is-admin)
  "Render an org page."
  (let ((org-name (getf org :name)))
    (page (:title (format nil "~A — Cave" (getf org :display-name)))
      (:h1 (getf org :display-name))
      (when (getf org :description)
        (:p (getf org :description)))
      (:div :style "display:flex;gap:var(--sp-2);margin-bottom:var(--sp-4)"
       (when is-member
         (:a.btn.btn-primary :href (format nil "/o/~A/-/new-repo" org-name)
          "New repository"))
       (when is-admin
         (:a.btn :href (format nil "/o/~A/-/settings" org-name)
          "Settings")))
      (:h2 "Repositories")
      (if repos
          (:ul.repo-list
           (dolist (repo repos)
             (:li
              (:a :href (format nil "/~A/~A" org-name (getf repo :name))
               (getf repo :name))
              (when (getf repo :is-private)
                (:span.badge "private"))
              (when (getf repo :description)
                (:span.desc (getf repo :description))))))
          (:p.empty "No repositories yet.")))))

(defun view-org-settings (&key org members runners registration-token)
  "Render org settings page with member management."
  (let ((org-name (getf org :name)))
    (page (:title (format nil "Settings — ~A" (getf org :display-name)))
      (:h1 (format nil "~A settings" (getf org :display-name)))

      (:section
       (:h2 "Members")
       (if members
           (:ul.data-list
            (dolist (m members)
              (:li
               (render-avatar (getf m :email) :size 20)
               (:strong (getf m :username))
               (:span.badge (getf m :role))
               (:form :method "post" :style "display:inline;margin-left:auto"
                :action (format nil "/o/~A/-/settings/members/~A/remove"
                                org-name (getf m :user-id))
                (:button.btn.btn-sm :type "submit" "Remove")))))
           (:p.empty "No members."))
       (:h3 "Add member")
       (:form :method "post" :action (format nil "/o/~A/-/settings/members" org-name)
        (:div :style "display:flex;gap:var(--sp-2);align-items:end"
         (:div.field :style "margin-bottom:0"
          (:label :for "member_username" "Username")
          (:input :type "text" :id "member_username" :name "username" :required t))
         (:div.field :style "margin-bottom:0"
          (:label :for "member_role" "Role")
          (:select :id "member_role" :name "role"
           (:option :value "member" "Member")
           (:option :value "admin" "Admin")))
         (:button.btn.btn-primary :type "submit" "Add member"))))

      ;; Runners
      (:section
       (:h2 "Runners")
       (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-3)"
        "Runners scoped to this organization's repositories.")
       (render-runner-management runners registration-token
                                 (format nil "/o/~A/-/settings/runners/token" org-name)
                                 (format nil "/o/~A/-/settings/runners" org-name))))))

;;; ========================== REPO PAGES ==========================

(defun view-new-repo (&key org error)
  "Render the new repo form for an org."
  (let ((org-name (getf org :name)))
    (page (:title "New repository — Cave")
      (:h1 (format nil "New repository in ~A" org-name))
      (render-new-repo-form (format nil "/o/~A/-/new-repo" org-name) :error error))))

(defun render-repo-tabs (owner-name repo-name &optional active-tab &key repo)
  "Render the repo breadcrumb and navigation tab bar."
  (spinneret:with-html
    (render-breadcrumbs
     (list (list (format nil "/~A" owner-name) owner-name)
           repo-name))
    (when (and repo (getf repo :is-private)) (:span.badge "private"))
    (when (and repo (getf repo :is-archived)) (:span.badge "archived"))
    (:nav.repo-tabs
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :overview))
      :href (format nil "/~A/~A" owner-name repo-name) "Overview")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :code))
      :href (format nil "/~A/~A/code" owner-name repo-name) "Code")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :issues))
      :href (format nil "/~A/~A/issues" owner-name repo-name) "Issues")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :pulls))
      :href (format nil "/~A/~A/pulls" owner-name repo-name) "Pull requests")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :runs))
      :href (format nil "/~A/~A/runs" owner-name repo-name) "Runs")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :releases))
      :href (format nil "/~A/~A/releases" owner-name repo-name) "Releases")
     (when (and repo *current-user-id*
                (repo-member-role (getf repo :id) *current-user-id*))
       (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :pulse))
        :href (format nil "/~A/~A/pulse" owner-name repo-name) "Pulse"))
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :settings))
      :href (format nil "/~A/~A/settings" owner-name repo-name) "Settings"))))

(defun render-file-tree (file-tree owner-name repo-name default-branch &optional current-path)
  "Render a file tree table. Shared by view-repo and view-tree."
  (spinneret:with-html
    (:table.file-tree
     (:tbody
      ;; Parent directory link when in a subdirectory
      (when (and current-path (not (uiop:emptyp current-path)))
        (let ((parent (let ((slash (position #\/ current-path :from-end t)))
                        (if slash (subseq current-path 0 slash) ""))))
          (:tr
           (:td.file-icon "..")
           (:td (:a :href (if (uiop:emptyp parent)
                              (format nil "/~A/~A" owner-name repo-name)
                              (format nil "/~A/~A/tree/~A?path=~A"
                                      owner-name repo-name default-branch parent))
                 ".."))
           (:td.file-size ""))))
      (dolist (entry file-tree)
        (let* ((name (getf entry :name))
               (is-dir (equal (getf entry :type) "tree"))
               (entry-path (if (and current-path (not (uiop:emptyp current-path)))
                               (format nil "~A/~A" current-path name)
                               name)))
          (:tr :class (when is-dir "file-dir")
           (:td.file-icon (if is-dir "/" "."))
           (:td
            (:a :href (if is-dir
                          (format nil "/~A/~A/tree/~A?path=~A"
                                  owner-name repo-name default-branch entry-path)
                          (format nil "/~A/~A/blob/~A?path=~A"
                                  owner-name repo-name default-branch entry-path))
             name))
           (:td.file-size
            (let ((size (getf entry :size)))
              (when (and size (not is-dir))
                (cond
                  ((>= size (* 1024 1024)) (format nil "~,1f MB" (/ size (* 1024.0 1024.0))))
                  ((>= size 1024) (format nil "~,1f KB" (/ size 1024.0)))
                  (t (format nil "~A B" size)))))))))))))

(defun render-clone-widget (owner-name repo-name)
  "SSH/HTTPS toggle with a copy-to-clipboard button."
  (let ((ssh-url (ssh-clone-url owner-name repo-name))
        (https-url (https-clone-url owner-name repo-name)))
    (spinneret:with-html
      (:div.clone-widget
       :style "display:flex;align-items:stretch;gap:0;margin:var(--sp-3) 0;max-width:640px;border:1px solid var(--border);border-radius:var(--radius);overflow:hidden;font-family:var(--font-mono);font-size:.85rem"
       (:button.clone-tab.clone-tab-active :type "button" :data-scheme "ssh"
        :style "padding:.4rem .75rem;background:var(--surface);border:none;border-right:1px solid var(--border);color:var(--text);cursor:pointer;font-family:inherit;font-size:inherit"
        "SSH")
       (:button.clone-tab :type "button" :data-scheme "https"
        :style "padding:.4rem .75rem;background:transparent;border:none;border-right:1px solid var(--border);color:var(--text-muted);cursor:pointer;font-family:inherit;font-size:inherit"
        "HTTPS")
       (:input.clone-url-input :type "text" :readonly t
        :data-ssh ssh-url :data-https https-url :value ssh-url
        :style "flex:1;padding:.4rem .6rem;background:var(--bg);border:none;color:var(--text);font-family:inherit;font-size:inherit;outline:none")
       (:button.clone-copy :type "button" :title "Copy URL"
        :style "padding:.4rem .75rem;background:var(--surface);border:none;border-left:1px solid var(--border);color:var(--text);cursor:pointer;font-family:inherit;font-size:inherit"
        "Copy"))
      (:script (:raw "
(function(){
  var root = document.currentScript.previousElementSibling;
  if (!root || !root.classList.contains('clone-widget')) return;
  var input = root.querySelector('.clone-url-input');
  var copyBtn = root.querySelector('.clone-copy');
  root.querySelectorAll('.clone-tab,.clone-tab-active').forEach(function(t){
    t.addEventListener('click', function(){
      root.querySelectorAll('.clone-tab,.clone-tab-active').forEach(function(x){
        x.className = 'clone-tab';
        x.style.background = 'transparent';
        x.style.color = 'var(--text-muted)';
      });
      t.className = 'clone-tab-active';
      t.style.background = 'var(--surface)';
      t.style.color = 'var(--text)';
      input.value = input.dataset[t.dataset.scheme];
    });
  });
  copyBtn.addEventListener('click', function(){
    input.select();
    navigator.clipboard.writeText(input.value).then(function(){
      var orig = copyBtn.textContent;
      copyBtn.textContent = 'Copied';
      setTimeout(function(){ copyBtn.textContent = orig; }, 1200);
    });
  });
})();
")))))

(defun view-repo (&key owner-name repo empty default-branch
                       readme-html readme-filename)
  "Render the repo overview page (README + clone URL)."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "~A/~A — Cave" org-name repo-name))
      (render-repo-tabs org-name repo-name :overview :repo repo)
      (when (getf repo :description) (:p (getf repo :description)))
      ;; Clone widget — SSH/HTTPS toggle with copy button
      (render-clone-widget org-name repo-name)
      ;; Fork button (don't show on own repos)
      (when (and *current-user*
                 (not (equal (getf *current-user* :username) org-name)))
        (:form :method "post" :style "margin-bottom:var(--sp-4)"
         :action (format nil "/~A/~A/fork" org-name repo-name)
         (:button.btn :type "submit"
          (format nil "Fork to ~A/~A" (getf *current-user* :username) repo-name))))

      (if empty
          (:section
           (:p.empty "This repository is empty. Push some code to get started:")
           (:pre :style "background:var(--surface);padding:1rem;border-radius:var(--radius);border:1px solid var(--border);font-size:.85rem;overflow-x:auto"
            (format nil "git remote add origin ~A~%git push -u origin main"
                    (ssh-clone-url org-name repo-name))))
          ;; README
          (when readme-html
            (:section
             (:h2 (or readme-filename "README"))
             (:div.readme-content (:raw readme-html))))))))

(defun view-code (&key owner-name repo branches tags default-branch
                       commit-count recent-commits file-tree)
  "Render the repo code/file browser page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "Code — ~A/~A" org-name repo-name))
      (render-repo-tabs org-name repo-name :code :repo repo)
      (render-clone-widget org-name repo-name)

      ;; Branch/tag bar + last commit
      (:div.repo-info-bar
       (:div.repo-info-left
        (:span.badge default-branch)
        (:span.repo-info-stat
         (format nil "~A ~:[branches~;branch~]" (length branches) (= (length branches) 1)))
        (when tags
          (:span.repo-info-stat
           (format nil "~A ~:[tags~;tag~]" (length tags) (= (length tags) 1)))))
       (when commit-count
         (:span.repo-info-stat
          (format nil "~A ~:[commits~;commit~]" commit-count (= commit-count 1)))))
      ;; Last commit bar
      (when recent-commits
        (let ((last (first recent-commits)))
          (:div.repo-last-commit
           (:a :href (format nil "/~A/~A/commit/~A" org-name repo-name
                              (getf last :hash))
            (:code.repo-last-hash (getf last :short-hash)))
           (:span.repo-last-msg (getf last :subject))
           (:span.repo-last-author (getf last :author)))))
      ;; File tree
      (when file-tree
        (render-file-tree file-tree org-name repo-name default-branch))

      ;; Recent commits
      (when recent-commits
        (:section
         (:h2 "Recent commits")
         (:ul.issue-list
          (dolist (c recent-commits)
            (:li
             (:a :href (format nil "/~A/~A/commit/~A" org-name repo-name (getf c :hash))
              (:code :style "color:var(--link);font-size:.8rem" (getf c :short-hash)))
             (:span (getf c :subject))
             (:span :style "margin-left:auto;color:var(--text-muted);font-size:.8rem"
              (getf c :author))))))))))

;;; ========================== TREE & BLOB PAGES ==========================

(defun view-tree (&key owner-name repo ref path file-tree)
  "Render a directory listing at a path."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" path owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo)
      (render-breadcrumbs
       (append (list (list (format nil "/~A" owner-name) owner-name)
                     (list (format nil "/~A/~A" owner-name repo-name) repo-name))
               (when (and path (not (uiop:emptyp path)))
                 (let ((parts (uiop:split-string path :separator '(#\/)))
                       (crumbs nil)
                       (built ""))
                   (dolist (part parts)
                     (setf built (if (uiop:emptyp built) part (format nil "~A/~A" built part)))
                     (push (list (format nil "/~A/~A/tree/~A?path=~A"
                                         owner-name repo-name ref built)
                                 part)
                           crumbs))
                   ;; Last one is just text, not a link
                   (let ((reversed (nreverse crumbs)))
                     (append (butlast reversed)
                             (list (second (car (last reversed))))))))))
      (:span.badge ref)
      (if file-tree
          (render-file-tree file-tree owner-name repo-name ref path)
          (:p.empty "Empty directory.")))))

(defun view-blob (&key owner-name repo ref path content is-binary file-size language)
  "Render a file content page with Monaco editor."
  (let ((repo-name (getf repo :name))
        (filename (let ((slash (position #\/ path :from-end t)))
                    (if slash (subseq path (1+ slash)) path))))
    (page (:title (format nil "~A — ~A/~A" path owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo)
      (render-breadcrumbs
       (append (list (list (format nil "/~A" owner-name) owner-name)
                     (list (format nil "/~A/~A" owner-name repo-name) repo-name))
               (let ((parts (uiop:split-string path :separator '(#\/)))
                     (crumbs nil)
                     (built ""))
                 (dolist (part parts)
                   (setf built (if (uiop:emptyp built) part (format nil "~A/~A" built part)))
                   (push (list (format nil "/~A/~A/blob/~A?path=~A"
                                       owner-name repo-name ref built)
                               part)
                         crumbs))
                 (let ((reversed (nreverse crumbs)))
                   (append (butlast reversed)
                           (list (second (car (last reversed)))))))))
      (:div.blob-meta
       (:span filename)
       (:span.badge language)
       (when file-size
         (:span.blob-size
          (cond ((> file-size (* 1024 1024))
                 (format nil "~,1f MB" (/ file-size (* 1024.0 1024.0))))
                ((> file-size 1024)
                 (format nil "~,1f KB" (/ file-size 1024.0)))
                (t (format nil "~A bytes" file-size)))))
       (:a.btn.btn-sm :href (format nil "/~A/~A/raw/~A?path=~A"
                                     owner-name repo-name ref path)
        "Raw"))
      (cond
        (is-binary
         (:div :style "padding:var(--sp-6);background:var(--surface);border:1px solid var(--border);border-top:none;text-align:center;color:var(--text-muted)"
          (:p "Binary file — not displayed.")
          (:a.btn :href (format nil "/~A/~A/raw/~A?path=~A"
                                 owner-name repo-name ref path)
           "Download")))
        ((and file-size (> file-size (* 1024 1024)))
         (:div :style "padding:var(--sp-6);background:var(--surface);border:1px solid var(--border);border-top:none;text-align:center;color:var(--text-muted)"
          (:p "File too large for preview.")
          (:a.btn :href (format nil "/~A/~A/raw/~A?path=~A"
                                 owner-name repo-name ref path)
           "Download")))
        (t
         (:div#editor-container :style "height:600px")
         (:script :src "https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/min/vs/loader.min.js" "")
         (:script
          (:raw (format nil "
require.config({ paths: { vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/min/vs' } });
require(['vs/editor/editor.main'], function() {
  var s = document.createElement('script');
  s.src = '/static/js/lisp-language.js';
  s.onload = init; s.onerror = init;
  document.head.appendChild(s);
  function init() {
  var container = document.getElementById('editor-container');
  var lineCount = ~A.split('\\n').length;
  container.style.height = Math.min(Math.max(lineCount * 19 + 20, 200), 800) + 'px';
  var ed = monaco.editor.create(container, {
    value: ~A,
    language: ~A,
    theme: document.documentElement.dataset.theme === 'light' ? 'vs' : 'vs-dark',
    readOnly: true,
    minimap: { enabled: false },
    scrollBeyondLastLine: false,
    fontSize: 13,
    fontFamily: \"'IBM Plex Mono', monospace\",
    automaticLayout: true,
    lineNumbers: 'on',
    renderLineHighlight: 'none',
    overviewRulerLanes: 0,
    hideCursorInOverviewRuler: true,
    scrollbar: { verticalScrollbarSize: 8 }
  });
  var params = new URLSearchParams(window.location.search);
  var line = parseInt(params.get('line'));
  var search = params.get('search');
  if (line) {
    ed.revealLineInCenter(line);
    ed.setPosition({ lineNumber: line, column: 1 });
  }
  if (search) {
    var fc = ed.getContribution('editor.contrib.findController');
    fc.setSearchString(search);
    fc.start({ forceRevealReplace: false, seedSearchStringFromSelection: 'none',
               shouldFocus: 0, shouldAnimate: true, loop: true });
  }
  }
});"
                  (com.inuoe.jzon:stringify content)
                  (com.inuoe.jzon:stringify content)
                  (com.inuoe.jzon:stringify (or language "plaintext"))))))))))

;;; ========================== COMMIT PAGE ==========================

(defun render-diff2html (raw-diff)
  "Render a raw unified diff using diff2html. Emits a div + scripts."
  (spinneret:with-html
    (:div#diff2html-container "")
    (:raw "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/diff2html@3.4.48/bundles/css/diff2html.min.css'>")
    (:script :src "https://cdn.jsdelivr.net/npm/diff2html@3.4.48/bundles/js/diff2html-ui.min.js" "")
    (:script
     (:raw (format nil "
document.addEventListener('DOMContentLoaded', function() {
  var diff = ~A;
  var target = document.getElementById('diff2html-container');
  var config = {drawFileList: true, matching: 'lines', outputFormat: 'line-by-line',
                highlight: true, colorScheme: 'dark'};
  var ui = new Diff2HtmlUI(target, diff, config);
  ui.draw();
  ui.highlightCode();
});
" (com.inuoe.jzon:stringify (or raw-diff "")))))))

(defun view-commit (&key owner-name repo commit diff-raw diff-stat)
  "Render a commit detail page with diff."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" (getf commit :short-hash) owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo)
      (render-breadcrumbs
       (list (list (format nil "/~A" owner-name) owner-name)
             (list (format nil "/~A/~A" owner-name repo-name) repo-name)
             (getf commit :short-hash)))
      (:div.commit-header
       (:h1.commit-subject (getf commit :subject))
       (when (and (getf commit :body) (not (uiop:emptyp (getf commit :body))))
         (:pre.commit-body (getf commit :body))))
      (:div.commit-meta
       (:strong (getf commit :author))
       (:span :style "margin-left:var(--sp-2);color:var(--text-muted)"
        (getf commit :date))
       (:code :style "margin-left:auto" (getf commit :hash)))
      (when diff-raw
        (render-diff2html diff-raw)))))

;;; ========================== ISSUE PAGES ==========================

(defun view-issues (&key owner-name repo issues current-status)
  "Render the issues list."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "Issues — ~A/~A" org-name repo-name))
      (render-repo-tabs org-name repo-name :issues :repo repo)
      (:div.issues-header
       (:div.issue-filters
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "open"))
         :href "?status=open" "Open")
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "closed"))
         :href "?status=closed" "Closed"))
       (when *current-user*
         (:a.btn.btn-primary :href (format nil "/~A/~A/issues/new" org-name repo-name)
          "New issue")))
      (if issues
          (:ul.issue-list
           (dolist (iss issues)
             (:li
              (:a :href (format nil "/~A/~A/issues/~A" org-name repo-name
                                 (getf iss :number))
               (:span.issue-number (format nil "#~A" (getf iss :number)))
               (format nil " ~A" (getf iss :title)))
              (:span.badge (getf iss :status)))))
          (:p.empty "No issues found.")))))

(defun view-new-issue (&key owner-name repo)
  "Render the new issue form."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title "New issue — Cave")
      (render-repo-tabs org-name repo-name :issues :repo repo)
      (:h1 "New issue")
      (:form :method "post" :action (format nil "/~A/~A/issues/new" org-name repo-name)
       (:div.field
        (:label :for "title" "Title")
        (:input :type "text" :id "title" :name "title" :required t :autofocus t))
       (:div.field
        (:label :for "body" "Description (optional)")
        (:textarea :id "body" :name "body" :rows "12"))
       (:button.btn.btn-primary :type "submit" "Create issue")))))

(defun view-issue (&key owner-name repo issue author comments)
  "Render an issue detail page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name))
        (issue-num (getf issue :number)))
    (page (:title (format nil "#~A ~A — Cave" issue-num (getf issue :title)))
      (render-repo-tabs org-name repo-name :issues :repo repo)
      (:div.issue-header
       (:h1 (format nil "#~A ~A" issue-num (getf issue :title)))
       (:span.badge (getf issue :status)))
      (:div.issue-meta
       (render-avatar (getf author :email) :size 16)
       (format nil " Opened by ~A" (getf author :username)))
      (when (getf issue :body)
        (:div.issue-body (getf issue :body)))

      ;; Comments
      (:section
       (:h2 (format nil "Comments (~A)" (length comments)))
       (if comments
           (dolist (c comments)
             (:div.comment
              (:div.comment-header
               (render-avatar (getf c :email) :size 16)
               (:strong (getf c :username))
               (:span.comment-date (princ-to-string (getf c :created-at))))
              (:div.comment-body (getf c :body))))
           (:p.empty "No comments yet.")))

      ;; Comment form + close/reopen
      (when *current-user*
        (:section
         (:form :method "post"
          :action (format nil "/~A/~A/issues/~A/comment" org-name repo-name issue-num)
          (:div.field
           (:label :for "comment_body" "Add a comment")
           (:textarea :id "comment_body" :name "body" :rows "4"))
          (:div :style "display:flex;gap:var(--sp-2);align-items:center"
           (:button.btn.btn-primary :type "submit" :name "action" :value "comment" "Comment")
           (if (equal (getf issue :status) "open")
               (:button.btn :type "submit" :name "action" :value "close"
                :style "border-color:var(--red);color:var(--red)"
                "Close issue")
               (:button.btn :type "submit" :name "action" :value "reopen"
                :style "border-color:var(--green);color:var(--green-bright)"
                "Reopen issue")))))))))

;;; ========================== PULL REQUEST PAGES ==========================

(defun view-pull-requests (&key owner-name repo pulls current-status)
  "Render the pull requests list."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "Pull requests — ~A/~A" org-name repo-name))
      (render-repo-tabs org-name repo-name :pulls :repo repo)
      (:div.issues-header
       (:div.issue-filters
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "open"))
         :href "?status=open" "Open")
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "merged"))
         :href "?status=merged" "Merged")
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "closed"))
         :href "?status=closed" "Closed"))
       (when *current-user*
         (:a.btn.btn-primary :href (format nil "/~A/~A/pulls/new" org-name repo-name)
          "New pull request")))
      (if pulls
          (:ul.issue-list
           (dolist (cs pulls)
             (:li
              (:a :href (format nil "/~A/~A/pulls/~A" org-name repo-name
                                 (getf cs :number))
               (:span.issue-number (format nil "#~A" (getf cs :number)))
               (format nil " ~A → ~A" (getf cs :source-branch) (getf cs :target-branch)))
              (:span.badge
               (cond ((getf cs :is-merged) "merged")
                     ((getf cs :is-closed) "closed")
                     (t "open"))))))
          (:p.empty "No pull requests found.")))))

(defun view-new-pull-request (&key owner-name repo branches default-branch)
  "Render the new pull request form."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "New pull request — %s/%s" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :pulls :repo repo)
      (:h1 "New pull request")
      (:form :method "post" :action (format nil "/~A/~A/pulls/new" owner-name repo-name)
       (:div.field
        (:label :for "source_branch" "Source branch")
        (:select :id "source_branch" :name "source_branch" :required t
         (dolist (b branches)
           (unless (equal b default-branch)
             (:option :value b :selected (when (equal b (first branches)) t) b)))))
       (:div.field
        (:label :for "target_branch" "Target branch")
        (:select :id "target_branch" :name "target_branch" :required t
         (dolist (b branches)
           (:option :value b :selected (when (equal b default-branch) t) b))))
       (:button.btn.btn-primary :type "submit" "Create pull request")))))

(defun render-inline-comments (comments)
  "Render inline diff comments in a comment row."
  (spinneret:with-html
    (dolist (c comments)
      (:div.diff-inline-comment
       (:span.diff-inline-comment-author (getf c :username))
       (:span.diff-inline-comment-date (princ-to-string (getf c :created-at)))
       (:div.diff-inline-comment-body (getf c :body))))))

(defun hljs-language (filename)
  "Map filename to highlight.js language class."
  (let ((ext (pathname-type (pathname filename)))
        (base (pathname-name (pathname filename))))
    (cond
      ((member ext '("lisp" "cl" "asd" "lsp") :test #'equalp) "lisp")
      ((member ext '("js" "mjs") :test #'equalp) "javascript")
      ((member ext '("ts" "tsx") :test #'equalp) "typescript")
      ((equalp ext "py") "python")
      ((equalp ext "rb") "ruby")
      ((member ext '("c" "h") :test #'equalp) "c")
      ((member ext '("cpp" "cc" "cxx" "hpp") :test #'equalp) "cpp")
      ((equalp ext "go") "go")
      ((equalp ext "rs") "rust")
      ((equalp ext "java") "java")
      ((equalp ext "sql") "sql")
      ((equalp ext "css") "css")
      ((equalp ext "html") "html")
      ((member ext '("md" "markdown") :test #'equalp) "markdown")
      ((equalp ext "json") "json")
      ((member ext '("yml" "yaml") :test #'equalp) "yaml")
      ((member ext '("sh" "bash" "zsh") :test #'equalp) "bash")
      ((equalp ext "xml") "xml")
      ((string-equal base "Makefile") "makefile")
      ((string-equal base "Dockerfile") "dockerfile")
      (t "plaintext"))))

(defun render-diff (diff-files owner-name repo-name ref
                    &key diff-comments comment-action can-comment)
  "Render parsed diff files as HTML with inline comments and syntax highlighting."
  (spinneret:with-html
    (dolist (file diff-files)
      (let ((filename (getf file :filename))
            (lang (hljs-language (getf file :filename))))
        (:div.diff-file
         (:div.diff-file-header
          (:a :href (format nil "/~A/~A/blob/~A?path=~A"
                            owner-name repo-name ref filename)
           filename))
         (:div.diff-body
          (dolist (line (getf file :lines))
            (let* ((type (getf line :type))
                   (content (getf line :content))
                   (old-ln (getf line :old-line))
                   (new-ln (getf line :new-line))
                   (side (if new-ln "new" "old"))
                   (ln (or new-ln old-ln))
                   (comment-key (when ln (format nil "~A:~A:~A" filename ln side)))
                   (line-comments (when (and comment-key diff-comments)
                                   (gethash comment-key diff-comments))))
              (case type
                (:hunk
                 (:div.diff-line.diff-line-hunk
                  (:span.diff-hunk-text content)))
                (:add
                 (:div.diff-line.diff-line-add
                  :data-file filename :data-line new-ln :data-side "new"
                  (:span.diff-line-num "")
                  (:span.diff-line-num (princ-to-string new-ln))
                  (:span.diff-add-btn :onclick "caveToggleCommentForm(this)"
                   (when can-comment "+"))
                  (:span.diff-gutter "+")
                  (:code.diff-code :class (format nil "language-~A" lang) content)))
                (:del
                 (:div.diff-line.diff-line-del
                  :data-file filename :data-line old-ln :data-side "old"
                  (:span.diff-line-num (princ-to-string old-ln))
                  (:span.diff-line-num "")
                  (:span.diff-add-btn :onclick "caveToggleCommentForm(this)"
                   (when can-comment "+"))
                  (:span.diff-gutter "-")
                  (:code.diff-code :class (format nil "language-~A" lang) content)))
                (:context
                 (:div.diff-line.diff-line-context
                  :data-file filename :data-line (or new-ln "") :data-side "new"
                  (:span.diff-line-num (if old-ln (princ-to-string old-ln) ""))
                  (:span.diff-line-num (if new-ln (princ-to-string new-ln) ""))
                  (:span.diff-add-btn :onclick "caveToggleCommentForm(this)"
                   (when can-comment "+"))
                  (:span.diff-gutter " ")
                  (:code.diff-code :class (format nil "language-~A" lang) content))))
              ;; Existing comments
              (when line-comments
                (:div.diff-comment-row
                 (render-inline-comments line-comments)))
              ;; Hidden comment form
              (when (and can-comment ln)
                (:div.diff-comment-form :id (format nil "cf-~A-~A-~A" filename ln side)
                 (:form :method "post" :action comment-action
                  (:input :type "hidden" :name "file_path" :value filename)
                  (:input :type "hidden" :name "line_number"
                   :value (princ-to-string ln))
                  (:input :type "hidden" :name "side" :value side)
                  (:textarea :name "body" :rows "3" :required t
                   :placeholder "Write a comment...")
                  (:div :style "display:flex;gap:var(--sp-2);margin-top:var(--sp-2)"
                   (:button.btn.btn-primary.btn-sm :type "submit" "Comment")
                   (:button.btn.btn-sm :type "button"
                    :onclick "this.closest('.diff-comment-form').classList.remove('active')"
                    "Cancel")))))))))))
    ;; Inline JS for toggling comment forms
    (when can-comment
      (:script (:raw "
function caveToggleCommentForm(btn) {
  var row = btn.closest('.diff-line');
  var file = row.dataset.file;
  var line = row.dataset.line;
  var side = row.dataset.side;
  var formId = 'cf-' + file + '-' + line + '-' + side;
  var form = document.getElementById(formId);
  if (form) {
    form.classList.toggle('active');
    if (form.classList.contains('active')) {
      form.querySelector('textarea').focus();
    }
  }
}
")))))

(defun view-pull-request (&key owner-name repo pr author reviews eligibility
                             can-merge stack stack-items diff-raw
                             diff-comments-json comment-action
                             commit-statuses)
  "Render a pull request detail page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name))
        (cs-num (getf pr :number)))
    (page (:title (format nil "#~A ~A — Cave" cs-num (getf pr :source-branch)))
      (render-repo-tabs org-name repo-name :pulls :repo repo)

      (:div.issue-header
       (:h1 (format nil "#~A ~A" cs-num (getf pr :source-branch)))
       (:span.badge
        (cond ((getf pr :is-merged) "merged")
              ((getf pr :is-closed) "closed")
              (t "open"))))

      (:div.issue-meta
       (format nil "~A → ~A · version ~A · by ~A"
               (getf pr :source-branch)
               (getf pr :target-branch)
               (getf pr :version)
               (getf author :username))
       (when (getf pr :head-commit)
         (:code :style "margin-left:.5rem" (getf pr :head-commit))))

      ;; Stack
      (when stack
        (:section
         (:h2 (format nil "Stack: ~A" (getf stack :name)))
         (:ol.stack-list
          (dolist (item stack-items)
            (:li :class (when (= (getf item :number) cs-num) "stack-current")
             (:a :href (format nil "/~A/~A/pulls/~A" org-name repo-name
                                (getf item :number))
              (format nil "#~A ~A" (getf item :number) (getf item :source-branch)))
             (:span.badge
              (cond ((getf item :is-merged) "merged")
                    ((getf item :is-closed) "closed")
                    (t "open"))))))))

      ;; Diff with inline comments
      (when diff-raw
        (:section
         (:h2 "Changes")
         (render-diff2html diff-raw)
         (when (and comment-action *current-user*)
           (:script
            (:raw (format nil "
var caveComments = ~A;
var caveCommentAction = ~A;
// Wait for diff2html to finish rendering
function caveInitComments() {
  if (!document.querySelector('.d2h-code-linenumber:not(.d2h-info)')) {
    setTimeout(caveInitComments, 200); return;
  }
  (function() {
    // Inject existing comments
    caveComments.forEach(function(c) {
      var file = c.file_path, line = c.line_number, side = c.side;
      var wrapper = caveGetFileWrapper(file);
      if (!wrapper) return;
      var tr = caveGetLineRow(wrapper, line, side);
      if (!tr) return;
      var commentRow = document.createElement('tr');
      commentRow.className = 'cave-inline-comment';
      commentRow.innerHTML = '<td colspan=\"2\" class=\"cave-comment-cell\"><div class=\"cave-ic\"><strong>' +
        caveEsc(c.username) + '</strong> <span class=\"cave-ic-date\">' + caveEsc(c.created_at) + '</span>' +
        '<div class=\"cave-ic-body\">' + caveEsc(c.body) + '</div></div></td>';
      tr.parentNode.insertBefore(commentRow, tr.nextSibling);
    });
    // Inject + buttons on each line
    document.querySelectorAll('.d2h-code-linenumber:not(.d2h-info)').forEach(function(td) {
      var btn = document.createElement('span');
      btn.className = 'cave-add-comment-btn';
      btn.textContent = '+';
      btn.onclick = function() { caveShowCommentForm(td); };
      td.style.position = 'relative';
      td.appendChild(btn);
    });
  })();
}
setTimeout(caveInitComments, 500);
function caveEsc(s) { var d=document.createElement('div'); d.textContent=s; return d.innerHTML; }
function caveGetFileWrapper(filename) {
  var wrappers = document.querySelectorAll('.d2h-file-wrapper');
  for (var i=0; i<wrappers.length; i++) {
    var nameEl = wrappers[i].querySelector('.d2h-file-name');
    if (nameEl && nameEl.textContent.trim() === filename) return wrappers[i];
  }
  return null;
}
function caveGetLineRow(wrapper, lineNum, side) {
  var cls = side === 'old' ? 'line-num1' : 'line-num2';
  var divs = wrapper.querySelectorAll('.' + cls);
  for (var i=0; i<divs.length; i++) {
    if (parseInt(divs[i].textContent) === lineNum) return divs[i].closest('tr');
  }
  return null;
}
function caveShowCommentForm(td) {
  var existing = td.closest('tr').nextElementSibling;
  if (existing && existing.classList.contains('cave-comment-form-row')) {
    existing.remove(); return;
  }
  var tr = td.closest('tr');
  var wrapper = td.closest('.d2h-file-wrapper');
  var filename = wrapper.querySelector('.d2h-file-name').textContent.trim();
  var num1 = td.querySelector('.line-num1');
  var num2 = td.querySelector('.line-num2');
  var lineNum = num2 && num2.textContent.trim() ? num2.textContent.trim() : (num1 ? num1.textContent.trim() : '0');
  var side = num2 && num2.textContent.trim() ? 'new' : 'old';
  var formRow = document.createElement('tr');
  formRow.className = 'cave-comment-form-row';
  formRow.innerHTML = '<td colspan=\"2\" class=\"cave-comment-cell\">' +
    '<form method=\"post\" action=\"' + caveCommentAction + '\">' +
    '<input type=\"hidden\" name=\"file_path\" value=\"' + caveEsc(filename) + '\">' +
    '<input type=\"hidden\" name=\"line_number\" value=\"' + lineNum + '\">' +
    '<input type=\"hidden\" name=\"side\" value=\"' + side + '\">' +
    '<textarea name=\"body\" rows=\"3\" required placeholder=\"Write a comment...\"></textarea>' +
    '<div style=\"display:flex;gap:8px;margin-top:4px\">' +
    '<button type=\"submit\" class=\"btn btn-primary btn-sm\">Comment</button>' +
    '<button type=\"button\" class=\"btn btn-sm\" onclick=\"this.closest(&#39;tr&#39;).remove()\">Cancel</button>' +
    '</div></form></td>';
  tr.parentNode.insertBefore(formRow, tr.nextSibling);
  formRow.querySelector('textarea').focus();
}
"
                    (or diff-comments-json "[]")
                    (com.inuoe.jzon:stringify (or comment-action ""))))))))

      ;; Commit statuses (CI)
      (when commit-statuses
        (:section
         (:h2 "Checks")
         (:ul.eligibility-list
          (dolist (s commit-statuses)
            (:li :class (cond ((equal (getf s :state) "success") "rule-pass")
                              ((equal (getf s :state) "pending") "")
                              (t "rule-fail"))
             (format nil "~A ~A — ~A"
                     (cond ((equal (getf s :state) "success") "✓")
                           ((equal (getf s :state) "pending") "⏳")
                           (t "✗"))
                     (getf s :context)
                     (or (getf s :description) (getf s :state)))
             (when (and (getf s :target-url) (not (eq (getf s :target-url) :null)))
               (:a :href (getf s :target-url) :style "margin-left:var(--sp-2);font-size:.8rem"
                "details")))))))

      ;; Merge eligibility
      (when (and eligibility
                 (not (getf pr :is-merged))
                 (not (getf pr :is-closed)))
        (:section
         (:h2 "Merge eligibility")
         (:ul.eligibility-list
          (dolist (rule eligibility)
            (:li :class (if (getf rule :pass) "rule-pass" "rule-fail")
             (format nil "~A ~A"
                     (if (getf rule :pass) "✓" "✗")
                     (getf rule :description)))))
         (when can-merge
           (:form :method "post"
            :action (format nil "/~A/~A/pulls/~A/merge" org-name repo-name cs-num)
            (:div :style "display:flex;gap:var(--sp-2)"
             (:button.btn.btn-primary :type "submit" :name "strategy" :value "merge"
              "Merge")
             (:button.btn :type "submit" :name "strategy" :value "squash"
              "Squash and merge"))))))

      ;; Reviews
      (:section
       (:h2 "Reviews")
       (if reviews
           (dolist (r reviews)
             (:div :class (format nil "review~@[ review-stale~]" (getf r :is-stale))
              (:div.review-header
               (render-avatar (getf r :reviewer-email) :size 16)
               (:strong (getf r :reviewer-username))
               (:span :class (format nil "badge review-state-~A" (getf r :state))
                (getf r :state))
               (when (getf r :is-stale) (:span.badge "stale"))
               (:span.review-version (format nil "v~A" (getf r :changeset-version))))
              (when (getf r :body)
                (:div.review-body (getf r :body)))
              (when (getf r :concerns)
                (:ul.concern-list
                 (dolist (c (getf r :concerns))
                   (:li :class (format nil "concern concern-~A" (getf c :status))
                    (:span.concern-text (getf c :body))
                    (if (equal (getf c :status) "open")
                        (:form :method "post" :style "display:inline"
                         :action (format nil "/~A/~A/concerns/~A/resolve"
                                         org-name repo-name (getf c :id))
                         (:button.btn.btn-sm :type "submit" "Resolve"))
                        (:span.badge "resolved"))))))))
           (:p.empty "No reviews yet.")))

      ;; Submit review form
      (when (and *current-user*
                 (not (getf pr :is-merged))
                 (not (getf pr :is-closed)))
        (:section
         (:h2 "Submit review")
         (:form :method "post"
          :action (format nil "/~A/~A/pulls/~A/review" org-name repo-name cs-num)
          (:div.field
           (:label :for "review_body" "Comment")
           (:textarea :id "review_body" :name "body" :rows "4"))
          (:div.field
           (:label :for "concern_text" "Concern text (for approve-with-concerns)")
           (:input :type "text" :id "concern_text" :name "concern_text"
                   :placeholder "Optional — describe the concern"))
          (:div.review-actions
           (:button.btn.btn-primary :type "submit" :name "state" :value "approve" "Approve")
           (:button.btn :type "submit" :name "state" :value "approve_with_concerns"
            "Approve with concerns")
           (:button.btn :type "submit" :name "state" :value "request_changes"
            :style "border-color:var(--danger)" "Request changes")
           (:button.btn :type "submit" :name "state" :value "comment" "Comment"))))))))

;;; ========================== AUTOMATION RUNS ==========================

(defun render-status-badge (status)
  "Render a status badge with appropriate color."
  (spinneret:with-html
    (:span.badge
     :style (cond ((equal status "success")
                    "border-color:var(--green);color:var(--green)")
                   ((member status '("failure" "timed_out" "cancelled") :test #'equal)
                    "border-color:var(--red);color:var(--red)")
                   ((member status '("running" "assigned") :test #'equal)
                    "border-color:var(--accent);color:var(--accent)")
                   ((member status '("skipped" "pending") :test #'equal)
                    "border-color:var(--text-muted);color:var(--text-muted)")
                   (t ""))
     status)))

(defun render-short-sha (sha)
  "Render a short commit SHA."
  (when (and sha (not (eq sha :null)) (plusp (length sha)))
    (spinneret:with-html
      (:code :style "font-size:.75rem;color:var(--text-muted)"
       (subseq sha 0 (min 7 (length sha)))))))

(defun render-pulse-chart (days event-counts)
  "Stacked bar chart, one column per day, colored per event type. Pure HTML+CSS."
  (let* ((today (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
                  (declare (ignore s m h))
                  (encode-universal-time 0 0 12 d mo y)))
         (day-secs 86400)
         (day-labels
          (loop for i from (1- days) downto 0
                collect (multiple-value-bind (s m h d mo y)
                            (decode-universal-time (- today (* i day-secs)))
                          (declare (ignore s m h))
                          (format nil "~4,'0D-~2,'0D-~2,'0D" y mo d))))
         ;; bucket: key = (day type), value = count
         (bucket (let ((h (make-hash-table :test 'equal)))
                   (dolist (row event-counts)
                     (setf (gethash (cons (getf row :day) (getf row :type)) h)
                           (getf row :count)))
                   h))
         (types '(("git.push" "push" "#7c9a5e")
                  ("git.clone" "clone" "#e8a84c")
                  ("repo.forked" "fork" "#9e9a8f")
                  ("issue.created" "issue" "#c9a03e")
                  ("pr.merged" "merge" "#c25450")
                  ("changeset.merged" "merge" "#c25450")))
         (per-day-totals (mapcar (lambda (day)
                                   (let ((sum 0))
                                     (dolist (tspec types)
                                       (incf sum (or (gethash (cons day (first tspec)) bucket) 0)))
                                     sum))
                                 day-labels))
         (max-total (max 1 (reduce #'max per-day-totals)))
         (col-width 28)
         (chart-h 120))
    (spinneret:with-html
      (:div.pulse-chart
       :style (format nil "display:flex;align-items:flex-end;gap:2px;height:~Apx;border-bottom:1px solid var(--border);padding:0 .5rem"
                      chart-h)
       (dolist (day day-labels)
         (let ((day-total (or (gethash (cons day "_") bucket) 0)))
           (declare (ignore day-total))
           (:div :style (format nil "display:flex;flex-direction:column-reverse;width:~Apx;align-items:stretch" col-width)
            :title day
            (dolist (tspec types)
              (let* ((type (first tspec))
                     (color (third tspec))
                     (n (or (gethash (cons day type) bucket) 0))
                     (height (if (zerop n) 0
                                 (max 1 (round (* (/ n max-total) chart-h))))))
                (when (plusp n)
                  (:div :style (format nil "background:~A;height:~Apx" color height)
                   :title (format nil "~A: ~A ~A" day n type)))))))))
      ;; X-axis labels (every 2 days)
      (:div :style (format nil "display:flex;gap:2px;padding:.25rem .5rem;font-family:var(--font-mono);font-size:.7rem;color:var(--text-muted)")
       (loop for day in day-labels for i from 0
             do (:div :style (format nil "width:~Apx;text-align:center" col-width)
                 (when (zerop (mod i 2)) (subseq day 5)))))
      ;; Legend
      (:div :style "display:flex;gap:1rem;flex-wrap:wrap;margin-top:.5rem;font-family:var(--font-mono);font-size:.8rem;color:var(--text-muted)"
       (dolist (tspec (remove-duplicates types :test #'string= :key #'second))
         (:span :style "display:inline-flex;align-items:center;gap:.3rem"
          (:span :style (format nil "display:inline-block;width:10px;height:10px;background:~A;border-radius:2px"
                                (third tspec)))
          (second tspec)))))))

(defun render-daily-line-chart (days rows label color)
  "Single-series line/bar chart. ROWS is a list of plists (:day yyyy-mm-dd :count n)."
  (let* ((today (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
                  (declare (ignore s m h))
                  (encode-universal-time 0 0 12 d mo y)))
         (day-secs 86400)
         (day-labels
          (loop for i from (1- days) downto 0
                collect (multiple-value-bind (s m h d mo y)
                            (decode-universal-time (- today (* i day-secs)))
                          (declare (ignore s m h))
                          (format nil "~4,'0D-~2,'0D-~2,'0D" y mo d))))
         (bucket (let ((h (make-hash-table :test 'equal)))
                   (dolist (r rows)
                     (setf (gethash (getf r :day) h) (or (getf r :count) 0)))
                   h))
         (counts (mapcar (lambda (d) (or (gethash d bucket) 0)) day-labels))
         (max-c (max 1 (reduce #'max counts)))
         (col-width 28)
         (chart-h 80))
    (spinneret:with-html
      (:div :style (format nil "display:flex;align-items:flex-end;gap:2px;height:~Apx;border-bottom:1px solid var(--border);padding:0 .5rem"
                           chart-h)
       (dolist (day day-labels)
         (let* ((n (or (gethash day bucket) 0))
                (height (if (zerop n) 0 (max 1 (round (* (/ n max-c) chart-h))))))
           (:div :style (format nil "width:~Apx;background:~A;height:~Apx"
                                col-width
                                (if (plusp n) color "transparent")
                                height)
            :title (format nil "~A: ~A ~A" day n label)))))
      (:div :style (format nil "display:flex;gap:2px;padding:.25rem .5rem;font-family:var(--font-mono);font-size:.7rem;color:var(--text-muted)")
       (loop for day in day-labels for i from 0
             do (:div :style (format nil "width:~Apx;text-align:center" col-width)
                 (when (zerop (mod i 2)) (subseq day 5))))))))

(defun view-pulse (&key owner-name repo days event-counts contributors
                        views unique-visitors referrers)
  "Repo Pulse — activity, traffic, and referrers over the last N days."
  (let* ((repo-name (getf repo :name))
         (total-events (reduce #'+ event-counts :key (lambda (r) (or (getf r :count) 0)) :initial-value 0))
         (by-type (let ((h (make-hash-table :test 'equal)))
                    (dolist (row event-counts) (incf (gethash (getf row :type) h 0)
                                                     (or (getf row :count) 0)))
                    h))
         (total-views (reduce #'+ views :key (lambda (r) (or (getf r :count) 0)) :initial-value 0))
         (peak-unique (reduce #'max unique-visitors
                              :key (lambda (r) (or (getf r :count) 0)) :initial-value 0)))
    (page (:title (format nil "Pulse — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :pulse :repo repo)
      (:section
       (:h2 (format nil "Activity in the last ~D days" days))
       (:p :style "color:var(--text-muted);font-size:.9rem;margin-bottom:var(--sp-3)"
        (format nil "~D event~:P total — ~D push~:P, ~D clone~:P, ~D issue~:P, ~D merge~:P."
                total-events
                (gethash "git.push" by-type 0)
                (gethash "git.clone" by-type 0)
                (gethash "issue.created" by-type 0)
                (+ (gethash "pr.merged" by-type 0) (gethash "changeset.merged" by-type 0))))
       (render-pulse-chart days event-counts))

      (:section
       (:div :style "display:grid;grid-template-columns:1fr 1fr;gap:var(--sp-4)"
        (:div
         (:h3 :style "margin-bottom:var(--sp-1)" "Total views")
         (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-2)"
          (format nil "~D over ~D days" total-views days))
         (render-daily-line-chart days views "view" "#7c9a5e"))
        (:div
         (:h3 :style "margin-bottom:var(--sp-1)" "Unique visitors")
         (:p :style "color:var(--text-muted);font-size:.85rem;margin-bottom:var(--sp-2)"
          (format nil "peak ~D on a day" peak-unique))
         (render-daily-line-chart days unique-visitors "visitor" "#d4a054"))))

      (:section
       (:div :style "display:grid;grid-template-columns:1fr 1fr;gap:var(--sp-4)"
        (:div
         (:h3 "Top contributors")
         (if contributors
             (:table.file-tree
              (:tbody
               (dolist (c contributors)
                 (:tr
                  (:td (:a :href (format nil "/~A" (getf c :username)) (getf c :username)))
                  (:td.file-size (format nil "~D event~:P" (getf c :count)))))))
             (:p.empty "No activity yet.")))
        (:div
         (:h3 "Referring sites")
         (if referrers
             (:table.file-tree
              (:tbody
               (dolist (r referrers)
                 (:tr
                  (:td (getf r :host))
                  (:td.file-size (format nil "~D view~:P" (getf r :count)))))))
             (:p.empty "No external referrers yet."))))))))

(defun format-bytes (n)
  "Human-readable byte size for an integer N."
  (cond
    ((or (null n) (eq n :null)) "—")
    ((< n 1024) (format nil "~D B" n))
    ((< n (* 1024 1024)) (format nil "~,1F KB" (/ n 1024.0)))
    ((< n (* 1024 1024 1024)) (format nil "~,1F MB" (/ n 1024.0 1024.0)))
    (t (format nil "~,2F GB" (/ n 1024.0 1024.0 1024.0)))))

(defun view-releases (&key owner-name repo releases assets-by-release can-create)
  "List of releases for a repo."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Releases — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :releases :repo repo)
      (:div :style "display:flex;justify-content:space-between;align-items:center;margin-bottom:var(--sp-3)"
       (:h2 "Releases")
       (when can-create
         (:a.btn.btn-primary :href (format nil "/~A/~A/releases/new" owner-name repo-name)
          "Draft a new release")))
      (if releases
          (:ul.repo-list
           (loop for r in releases for i from 0
                 do (let* ((tag (getf r :tag-name))
                           (assets (gethash (getf r :id) assets-by-release)))
                      (:li
                       (:div :style "display:flex;align-items:baseline;gap:.75rem;flex-wrap:wrap"
                        (:a :href (format nil "/~A/~A/releases/~A" owner-name repo-name tag)
                         :style "font-size:1.1rem;font-weight:600"
                         (or (getf r :name) tag))
                        (:span.badge :style "font-family:var(--font-mono)" tag)
                        (when (zerop i) (:span.badge :style "background:var(--accent-bg);color:var(--accent)" "Latest"))
                        (when (getf r :is-prerelease) (:span.badge "pre-release"))
                        (when (getf r :is-draft) (:span.badge "draft"))
                        (:span :style "color:var(--text-muted);font-size:.85rem;margin-left:auto"
                         (let ((rel (format-relative-time (getf r :published-at))))
                           (format nil "~@[by ~A ~]~@[~A~]" (getf r :author) rel))))
                       (when assets
                         (:div :style "margin-top:.4rem;color:var(--text-muted);font-size:.85rem"
                          (format nil "~D asset~:P" (length assets))))))))
          (:p.empty "No releases yet.")))))

(defun view-release (&key owner-name repo release assets can-edit)
  "Single release detail page."
  (let* ((repo-name (getf repo :name))
         (tag (getf release :tag-name))
         (body (getf release :body))
         (rendered-body (when (and body (plusp (length body)))
                          (render-markdown body))))
    (page (:title (format nil "~A — ~A/~A" tag owner-name repo-name))
      (render-repo-tabs owner-name repo-name :releases :repo repo)
      (:div :style "display:flex;align-items:baseline;gap:.75rem;flex-wrap:wrap;margin-bottom:var(--sp-3)"
       (:h2 :style "margin:0" (or (getf release :name) tag))
       (:span.badge :style "font-family:var(--font-mono)" tag)
       (when (getf release :is-prerelease) (:span.badge "pre-release"))
       (when (getf release :is-draft) (:span.badge "draft"))
       (:span :style "color:var(--text-muted);font-size:.85rem;margin-left:auto"
        (let ((rel (format-relative-time (getf release :published-at))))
          (format nil "~@[by ~A ~]~@[~A~]" (getf release :author) rel))))

      (when rendered-body
        (:section.readme-content (:raw rendered-body)))

      (:section
       (:h3 "Assets")
       (if assets
           (:table.file-tree
            (:tbody
             (dolist (a assets)
               (:tr
                (:td (:a :href (format nil "/~A/~A/releases/download/~A/~A"
                                       owner-name repo-name tag (getf a :name))
                      (getf a :name)))
                (:td.file-size (format-bytes (getf a :size)))
                (:td.file-size (format nil "~D download~:P" (or (getf a :download-count) 0)))
                (when can-edit
                  (:td (:form :method "post" :style "display:inline"
                        :action (format nil "/~A/~A/releases/~A/assets/~A/delete"
                                        owner-name repo-name tag (getf a :id))
                        (:button.btn.btn-sm :type "submit" "Delete"))))))))
           (:p.empty "No assets attached.")))

      (when can-edit
        (:section
         (:h3 "Upload asset")
         (:form :method "post" :enctype "multipart/form-data"
          :action (format nil "/~A/~A/releases/~A/upload" owner-name repo-name tag)
          (:div.field
           (:input :type "file" :name "asset" :required t))
          (:button.btn.btn-primary :type "submit" "Upload"))
         (:p :style "color:var(--text-muted);font-size:.85rem;margin-top:.3rem"
          "Up to 100 MB per file.")

         (:form :method "post" :style "margin-top:var(--sp-4)"
          :action (format nil "/~A/~A/releases/~A/delete" owner-name repo-name tag)
          :onsubmit "return confirm('Delete this release? Assets will be removed too.');"
          (:button.btn :type "submit" :style "color:var(--red)" "Delete release")))))))

(defun view-new-release (&key owner-name repo existing-tags error)
  "Form to create a new release."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "New release — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :releases :repo repo)
      (:h2 "Draft a new release")
      (when error (:div.alert.alert-error error))
      (:form :method "post" :action (format nil "/~A/~A/releases/new" owner-name repo-name)
       (:div.field
        (:label :for "tag_name" "Tag (existing tag, or a new one)")
        (:input :type "text" :id "tag_name" :name "tag_name"
                :placeholder "v1.0.0" :required t
                :list "existing-tags"
                :pattern "[A-Za-z0-9._/+\\-]+")
        (when existing-tags
          (:datalist :id "existing-tags"
           (dolist (tg existing-tags)
             (:option :value tg)))))
       (:div.field
        (:label :for "name" "Release name (optional)")
        (:input :type "text" :id "name" :name "name"
                :placeholder "Defaults to the tag name"))
       (:div.field
        (:label :for "body" "Description (Markdown supported)")
        (:textarea :id "body" :name "body" :rows 12
                   :style "width:100%;font-family:var(--font-mono);font-size:.9rem"))
       (:div.field
        (:label (:input :type "checkbox" :name "is_prerelease" :value "1")
         " Mark as pre-release"))
       (:button.btn.btn-primary :type "submit" "Publish release")))))

(defun view-runs (&key owner-name repo runs workflow-runs)
  "Render the runs list — both automations and workflows."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Runs — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :runs :repo repo)

      ;; Workflow runs
      (when workflow-runs
        (:section
         (:h2 "Workflow runs")
         (:ul.data-list
          (dolist (wr workflow-runs)
            (:li :style "flex-wrap:wrap"
             (:a :href (format nil "/~A/~A/runs/w/~A" owner-name repo-name (getf wr :id))
                 :style "font-weight:600;color:var(--primary)"
              (getf wr :workflow-name))
             (:span.badge (getf wr :trigger-event))
             (render-status-badge (getf wr :status))
             (render-short-sha (getf wr :commit-sha))
             (:span :style "font-size:.75rem;color:var(--text-muted)"
              (getf wr :workflow-file))
             (:span :style "margin-left:auto;color:var(--text-muted);font-size:.75rem"
              (princ-to-string (getf wr :created-at))))))))

      ;; Automation runs
      (:section
       (:h2 "Automation runs")
       (if runs
           (:ul.data-list
            (dolist (r runs)
              (:li :style "flex-wrap:wrap"
               (:strong (getf r :definition-name))
               (:span.badge (getf r :trigger-event))
               (render-status-badge (getf r :status))
               (render-short-sha (getf r :commit-sha))
               (:span :style "margin-left:auto;color:var(--text-muted);font-size:.75rem"
                (princ-to-string (getf r :created-at))))))
           (:p.empty "No automation runs yet."))))))

(defun view-workflow-run (&key owner-name repo run jobs)
  "Render a workflow run detail page with jobs and steps."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" (getf run :workflow-name) owner-name repo-name))
      (render-repo-tabs owner-name repo-name :runs :repo repo)
      (:div :style "margin-bottom:var(--sp-4)"
       (:h2 :style "margin-bottom:var(--sp-1)" (getf run :workflow-name))
       (:div :style "display:flex;gap:var(--sp-2);align-items:center;flex-wrap:wrap"
        (render-status-badge (getf run :status))
        (:span.badge (getf run :trigger-event))
        (render-short-sha (getf run :commit-sha))
        (:span :style "color:var(--text-muted);font-size:.85rem"
         (getf run :workflow-file))
        (when (and (getf run :ref) (not (eq (getf run :ref) :null)))
          (:span :style "color:var(--text-muted);font-size:.85rem"
           (getf run :ref)))
        (:span :style "color:var(--text-muted);font-size:.85rem"
         (princ-to-string (getf run :created-at)))))

      ;; Jobs
      (dolist (job-data jobs)
        (let* ((job (getf job-data :job))
               (steps (getf job-data :steps)))
          (:section :style "border:1px solid var(--border);border-radius:var(--radius);padding:var(--sp-3);margin-bottom:var(--sp-3)"
           (:div :style "display:flex;gap:var(--sp-2);align-items:center;margin-bottom:var(--sp-2)"
            (:h3 :style "margin:0" (getf job :name))
            (render-status-badge (getf job :status))
            (:code :style "font-size:.75rem;color:var(--text-muted)" (getf job :image))
            (let ((needs (getf job :needs)))
              (when (and needs (not (eq needs :null)) (not (uiop:emptyp needs)))
                (:span :style "font-size:.75rem;color:var(--text-muted)"
                 (format nil "needs: ~A" needs)))))
           ;; Steps
           (if steps
               (:div :style "display:flex;flex-direction:column;gap:2px"
                (dolist (step steps)
                  (let ((step-name (let ((n (getf step :name)))
                                     (if (or (null n) (eq n :null))
                                         (format nil "Step ~A" (getf step :step-order))
                                         n)))
                        (step-log (getf step :log)))
                    (:details :style "border:1px solid var(--border);border-radius:var(--radius)"
                     (:summary :style "padding:var(--sp-1) var(--sp-2);cursor:pointer;display:flex;gap:var(--sp-2);align-items:center;font-size:.85rem"
                      (render-status-badge (getf step :status))
                      (:span step-name)
                      (let ((cmd (getf step :command)))
                        (:code :style "color:var(--text-muted);font-size:.75rem"
                         (subseq cmd 0 (min 80 (length cmd)))))
                      (let ((ec (getf step :exit-code)))
                        (when (and ec (not (eq ec :null)))
                          (:span :style "margin-left:auto;font-size:.75rem;color:var(--text-muted)"
                           (format nil "exit ~A" ec)))))
                     (:pre :id (format nil "step-log-~A" (getf step :id))
                      :style "margin:0;padding:var(--sp-2);background:var(--bg);font-size:.8rem;overflow-x:auto;border-top:1px solid var(--border);min-height:1em"
                      (when (and step-log (not (eq step-log :null)) (plusp (length step-log)))
                        step-log))))))
               (:p.empty "No steps.")))))

      ;; SSE URL (hidden, read by JS below)
      (:div :id "sse-url" :style "display:none"
       :data-active (if (member (getf run :status) '("queued" "running") :test #'equal) "1" "0")
       (format nil "/~A/~A/runs/w/~A/logs" owner-name repo-name (getf run :id)))
      (:raw (format nil "<script>~A</script>"
              (concatenate 'string
               "var u=document.getElementById('sse-url');"
               "if(u&&u.dataset.active==='1'){var es=new EventSource(u.textContent.trim());"
               "es.addEventListener('step-log',function(e){"
               "var sp=e.data.indexOf(' ');var sid=e.data.substring(0,sp);"
               "var t=e.data.substring(sp+1).split('\\\\n').join('\\n');"
               "var p=document.getElementById('step-log-'+sid);"
               "if(p){if(!p.dataset.sse){p.textContent='';p.dataset.sse='1';}p.textContent+=t;p.parentElement.open=true;p.scrollTop=p.scrollHeight;}});"
               "es.addEventListener('step-status',function(e){"
               "var p=e.data.split(' ');var el=document.getElementById('step-log-'+p[0]);"
               "if(el){var b=el.parentElement.querySelector('.badge');if(b)b.textContent=p[1];}});"
               "es.addEventListener('run-status',function(e){"
               "var b=document.querySelector('h2').nextElementSibling.querySelector('.badge');"
               "if(b)b.textContent=e.data;"
               "if(e.data==='success'||e.data==='failure'||e.data==='cancelled')"
               "{setTimeout(function(){location.reload();},1000);}});"
               "es.addEventListener('done',function(){es.close();location.reload();});"
               "es.onerror=function(){es.close();};}"))))))



(defun render-runner-management (runners registration-token token-action delete-action-prefix)
  "Render runner list, registration token display, and token generation form."
  (spinneret:with-html
    (if runners
        (:table.data-table
         (:thead (:tr (:th "Name") (:th "Labels") (:th "Status") (:th "Last seen") (:th "")))
         (:tbody
          (dolist (r runners)
            (:tr
             (:td (getf r :name))
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
               :action (format nil "~A/~A/delete" delete-action-prefix (getf r :id))
               (:button.btn.btn-sm :type "submit" "Delete")))))))
        (:p.empty "No runners registered."))
    (when registration-token
      (:div.alert :style "border:1px solid var(--accent);padding:.75rem;margin:1rem 0"
       (:strong "Registration token created.") " Use this to register a runner:" (:br)
       (:code :style "word-break:break-all" (getf registration-token :token))
       (:p :style "margin-top:.5rem;color:var(--text-muted);font-size:.85rem"
        "Run: " (:code (format nil "cave runner --url grpc://localhost:~A --token ~A"
                               (config-value :grpc-port 9443)
                               (getf registration-token :token))))))
    (:form :method "post" :action token-action
     (:button.btn.btn-primary :type "submit" "Generate registration token"))))

;;; ========================== REPO SETTINGS ==========================

(defun view-repo-settings (&key owner-name repo members checks mirrors webhooks automations runners registration-token message)
  "Render repo settings page."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Settings — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :settings :repo repo)
      (:h1 "Repository settings")
      (when message
        (:div.alert message))

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

(defun view-admin (&key users runners registration-token message)
  "Render the admin panel."
  (page (:title "Admin — Cave")
    (:h1 "Instance administration")
    (:section
     (:h2 "Users")
     (when message
       (:div.alert :style "border:1px solid var(--primary);padding:.5rem .75rem;margin-bottom:1rem"
        message))
     (:table.data-table
      (:thead (:tr (:th "Username") (:th "Admin") (:th "Active") (:th "Created")))
      (:tbody
       (dolist (u users)
         (:tr
          (:td (getf u :username))
          (:td (if (getf u :is-admin) "yes" "no"))
          (:td (if (getf u :is-active) "yes" "no"))
          (:td (princ-to-string (getf u :created-at)))))))
     (:p :style "margin-top:1rem"
      (:a.btn :href (let ((issuer (config-value :oidc-issuer "")))
                      (if (search "/realms/" issuer)
                          (format nil "~A/admin/" (subseq issuer 0 (search "/realms/" issuer)))
                          "#"))
       "Manage users in Keycloak"))

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
         "Run: " (:code (format nil "cave runner --url grpc://localhost:~A --token ~A"
                                (config-value :grpc-port 9443)
                                (getf registration-token :token))))))
     (:form :method "post" :action "/-/admin/runners/token"
      (:button.btn.btn-primary :type "submit" "Generate registration token"))))))

(defun view-settings (&key ssh-keys api-tokens new-token ssh-error
                           generated-private-key generated-key-name
                           runners registration-token)
  "Render user settings page."
  (let ((cav-path (cav-download-path)))
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
              :selected (equal name (getf *current-user* :theme))
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
        "Manage your password and two-factor authentication.")
       (:a.btn :href (let ((issuer (config-value :oidc-issuer "")))
                       (if (search "/realms/" issuer)
                           (format nil "~A/account/#/security/signingin"
                                   (subseq issuer 0 (+ (search "/realms/" issuer)
                                                       (length "/realms/cave"))))
                           "#"))
        "Manage password & 2FA"))

      (:section
       (:h2 "CLI")
       (if cav-path
           (progn
             (:p "Download the Cave CLI for issue and API workflows.")
             (:p
              (:a.btn.btn-primary :href "/-/downloads/cav" "Download cav"))
             (:p :style "color:var(--text-muted);font-size:.85rem"
              "Save it somewhere on your PATH and run " (:code "chmod +x cav") "."))
           (:p.empty "cav is not installed on this Cave host yet."))
       (:pre :style "background:var(--surface);padding:1rem;border-radius:var(--radius);border:1px solid var(--border);font-size:.85rem;overflow-x:auto"
        "export CAVE_BASE_URL=" (config-value :base-url)
        "
export CAVE_TOKEN=<your-api-token>
./cav --repo OWNER/REPO issue list"))

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
