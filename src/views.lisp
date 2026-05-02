;;; views.lisp — HTML views using Spinneret
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; All HTML generation lives here. Each page is a function that returns
;;; an HTML string. No template files, no compilation step, no stale state.

(defmacro page ((&key title) &body body)
  "Wrap BODY in a full HTML page with nav and container."
  `(spinneret:with-html-string
     (:doctype)
     (:html :lang "en"
       (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title (or ,title "Cave"))
        (:link :rel "icon" :href "/static/img/favicon.svg" :type "image/svg+xml")
        (:link :rel "preconnect" :href "https://fonts.googleapis.com")
        (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "")
        (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,600;0,700;1,400&family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;1,400&display=swap")
        (:link :rel "stylesheet" :href "/static/css/cave.css"))
       (:body
        (:nav.nav
         (:div.nav-inner
          (:a.nav-brand :href "/" "Cave")
          (:div.nav-right
           (if *current-user*
               (progn
                 (:a.btn.btn-sm :href "/-/new-org" "New org")
                 (:a.btn.btn-sm :href "/-/settings" "Settings")
                 (when (getf *current-user* :is-admin)
                   (:a.btn.btn-sm :href "/-/admin" "Admin"))
                 (:span.nav-user (getf *current-user* :username))
                 (:form :method "post" :action "/logout" :style "display:inline"
                  (:button.btn.btn-sm :type "submit" "Log out")))
               (:a.btn.btn-sm :href "/-/auth/login" "Log in")))))
        (:main.container ,@body)))))

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

(defun view-dashboard (&key orgs repos username)
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
            (:li
             (:a :href (format nil "/~A/~A" username (getf repo :name))
              (getf repo :name))
             (when (getf repo :is-private)
               (:span.badge "private"))
             (when (getf repo :description)
               (:span.desc (getf repo :description))))))
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
         (:p.empty "You're not a member of any organization yet.")))))

;;; ========================== PERSONAL REPO CREATION ==========================

(defun view-new-personal-repo (&key error)
  "Render the personal repo creation form."
  (page (:title "New repository — Cave")
    (:h1 "New repository")
    (when error (:div.alert.alert-error error))
    (:form :method "post" :action "/-/new-repo"
     (:div.field
      (:label :for "name" "Repository name")
      (:input :type "text" :id "name" :name "name" :required t
              :pattern "[a-zA-Z0-9._-]+" :autofocus t))
     (:div.field
      (:label :for "description" "Description (optional)")
      (:input :type "text" :id "description" :name "description"))
     (:div.field
      (:label (:input :type "checkbox" :name "is_private" :value "1") " Private"))
     (:button.btn.btn-primary :type "submit" "Create repository"))))

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

(defun view-org (&key org repos is-member)
  "Render an org page."
  (let ((org-name (getf org :name)))
    (page (:title (format nil "~A — Cave" (getf org :display-name)))
      (:h1 (getf org :display-name))
      (when (getf org :description)
        (:p (getf org :description)))
      (when is-member
        (:a.btn.btn-primary :href (format nil "/o/~A/-/new-repo" org-name)
         "New repository"))
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

;;; ========================== REPO PAGES ==========================

(defun view-new-repo (&key org)
  "Render the new repo form."
  (let ((org-name (getf org :name)))
    (page (:title "New repository — Cave")
      (:h1 (format nil "New repository in ~A" org-name))
      (:form :method "post" :action (format nil "/o/~A/-/new-repo" org-name)
       (:div.field
        (:label :for "name" "Repository name")
        (:input :type "text" :id "name" :name "name" :required t
                :pattern "[a-zA-Z0-9._-]+" :autofocus t))
       (:div.field
        (:label :for "description" "Description (optional)")
        (:input :type "text" :id "description" :name "description"))
       (:div.field
        (:label (:input :type "checkbox" :name "is_private" :value "1") " Private"))
       (:button.btn.btn-primary :type "submit" "Create repository")))))

(defun render-repo-tabs (owner-name repo-name &optional active-tab)
  "Render the repo navigation tab bar. ACTIVE-TAB is :code, :issues, or :pulls."
  (spinneret:with-html
    (:nav.repo-tabs
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :code))
      :href (format nil "/~A/~A" owner-name repo-name) "Code")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :issues))
      :href (format nil "/~A/~A/issues" owner-name repo-name) "Issues")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :pulls))
      :href (format nil "/~A/~A/pulls" owner-name repo-name) "Pull requests"))))

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
                 "..")))))
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
             name)))))))))

(defun view-repo (&key owner-name repo role empty branches recent-commits
                       issues pulls default-branch file-tree
                       readme-html readme-filename)
  "Render a repo page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "~A/~A — Cave" org-name repo-name))
      (render-breadcrumbs
       (list (list (format nil "/~A" org-name) org-name)
             repo-name))
      (when (getf repo :is-private) (:span.badge "private"))
      (when (getf repo :description) (:p (getf repo :description)))

      (render-repo-tabs org-name repo-name :code)

      (if empty
          (:section
           (:p.empty "This repository is empty. Push some code to get started:")
           (:pre :style "background:var(--surface);padding:1rem;border-radius:var(--radius);border:1px solid var(--border);font-size:.85rem;overflow-x:auto"
            (format nil "git remote add origin ~A~%git push -u origin main"
                    (ssh-clone-url org-name repo-name))))
          (progn
            ;; File tree
            (when file-tree
              (:section
               (:h2 "Files")
               (when branches
                 (:div :style "margin-bottom:var(--sp-3)"
                  (:span.badge default-branch)
                  (:span :style "color:var(--text-muted);font-size:.8rem;margin-left:var(--sp-2)"
                   (format nil "~A branch~:P" (length branches)))))
               (render-file-tree file-tree org-name repo-name default-branch)))

            ;; README
            (when readme-html
              (:section
               (:h2 (or readme-filename "README"))
               (:div.readme-content (:raw readme-html))))

            ;; Clone URL
            (:section
             (:h2 "Clone")
             (:code.clone-url (ssh-clone-url org-name repo-name)))

            ;; Recent commits
            (when recent-commits
              (:section
               (:h2 "Recent commits")
               (:ul.issue-list
                (dolist (c recent-commits)
                  (:li
                   (:code :style "color:var(--link);font-size:.8rem" (getf c :short-hash))
                   (:span (getf c :subject))
                   (:span :style "margin-left:auto;color:var(--text-muted);font-size:.8rem"
                    (getf c :author))))))))))))

;;; ========================== TREE & BLOB PAGES ==========================

(defun view-tree (&key owner-name repo ref path file-tree)
  "Render a directory listing at a path."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" path owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code)
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
      (render-repo-tabs owner-name repo-name :code)
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
  var container = document.getElementById('editor-container');
  var lineCount = ~A.split('\\n').length;
  container.style.height = Math.min(Math.max(lineCount * 19 + 20, 200), 800) + 'px';
  monaco.editor.create(container, {
    value: ~A,
    language: ~A,
    theme: 'vs-dark',
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
});"
                  (com.inuoe.jzon:stringify content)
                  (com.inuoe.jzon:stringify content)
                  (com.inuoe.jzon:stringify (or language "plaintext"))))))))))

;;; ========================== ISSUE PAGES ==========================

(defun view-issues (&key owner-name repo issues current-status)
  "Render the issues list."
  (let ((org-name owner-name)
        (repo-name (getf repo :name)))
    (page (:title (format nil "Issues — ~A/~A" org-name repo-name))
      (render-repo-tabs org-name repo-name :issues)
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
      (render-repo-tabs org-name repo-name :issues)
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
      (render-repo-tabs org-name repo-name :issues)
      (:div.issue-header
       (:h1 (format nil "#~A ~A" issue-num (getf issue :title)))
       (:span.badge (getf issue :status)))
      (:div.issue-meta
       (format nil "Opened by ~A" (getf author :username)))
      (when (getf issue :body)
        (:div.issue-body (getf issue :body)))

      ;; Comments
      (:section
       (:h2 (format nil "Comments (~A)" (length comments)))
       (if comments
           (dolist (c comments)
             (:div.comment
              (:div.comment-header
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
      (render-repo-tabs org-name repo-name :pulls)
      (:div.issues-header
       (:div.issue-filters
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "open"))
         :href "?status=open" "Open")
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "merged"))
         :href "?status=merged" "Merged")
        (:a :class (format nil "btn btn-sm~@[ btn-active~]" (equal current-status "closed"))
         :href "?status=closed" "Closed")))
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

(defun render-inline-comments (comments)
  "Render inline diff comments in a comment row."
  (spinneret:with-html
    (dolist (c comments)
      (:div.diff-inline-comment
       (:span.diff-inline-comment-author (getf c :username))
       (:span.diff-inline-comment-date (princ-to-string (getf c :created-at)))
       (:div.diff-inline-comment-body (getf c :body))))))

(defun render-diff (diff-files owner-name repo-name ref
                    &key diff-comments comment-action can-comment)
  "Render parsed diff files as HTML with inline comments."
  (spinneret:with-html
    (dolist (file diff-files)
      (let ((filename (getf file :filename)))
        (:div.diff-file
         (:div.diff-file-header
          (:a :href (format nil "/~A/~A/blob/~A?path=~A"
                            owner-name repo-name ref filename)
           filename))
         (:table.diff-table
          (:tbody
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
                  (:tr.diff-line-hunk
                   (:td :colspan "5" content)))
                 (:add
                  (:tr.diff-line-add
                   :data-file filename :data-line new-ln :data-side "new"
                   (:td.diff-line-num "")
                   (:td.diff-line-num (princ-to-string new-ln))
                   (:td.diff-add-btn
                    :onclick "caveToggleCommentForm(this)" (when can-comment "+"))
                   (:td.diff-gutter "+")
                   (:td content)))
                 (:del
                  (:tr.diff-line-del
                   :data-file filename :data-line old-ln :data-side "old"
                   (:td.diff-line-num (princ-to-string old-ln))
                   (:td.diff-line-num "")
                   (:td.diff-add-btn
                    :onclick "caveToggleCommentForm(this)" (when can-comment "+"))
                   (:td.diff-gutter "-")
                   (:td content)))
                 (:context
                  (:tr.diff-line-context
                   :data-file filename :data-line (or new-ln "") :data-side "new"
                   (:td.diff-line-num (if old-ln (princ-to-string old-ln) ""))
                   (:td.diff-line-num (if new-ln (princ-to-string new-ln) ""))
                   (:td.diff-add-btn
                    :onclick "caveToggleCommentForm(this)" (when can-comment "+"))
                   (:td.diff-gutter " ")
                   (:td content))))
               ;; Render existing comments below this line
               (when line-comments
                 (:tr.diff-comment-row
                  (:td :colspan "5"
                   (render-inline-comments line-comments))))
               ;; Hidden comment form
               (when (and can-comment ln)
                 (:tr.diff-comment-form :id (format nil "cf-~A-~A-~A" filename ln side)
                  (:td :colspan "5"
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
                      :onclick "this.closest('tr').classList.remove('active')"
                      "Cancel"))))))))))))))
    ;; Inline JS for toggling comment forms
    (when can-comment
      (:script (:raw "
function caveToggleCommentForm(btn) {
  var row = btn.closest('tr');
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
"))))

(defun view-pull-request (&key owner-name repo pr author reviews eligibility
                             can-merge stack stack-items diff-files diff-stat
                             diff-comments)
  "Render a pull request detail page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name))
        (cs-num (getf pr :number)))
    (page (:title (format nil "#~A ~A — Cave" cs-num (getf pr :source-branch)))
      (render-repo-tabs org-name repo-name :pulls)

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

      ;; Diff
      (when diff-files
        (:section
         (:h2 (format nil "Changes (~A file~:P)" (length diff-files)))
         (when diff-stat
           (:pre.diff-stat diff-stat))
         (render-diff diff-files org-name repo-name
                      (getf pr :source-branch)
                      :diff-comments diff-comments
                      :can-comment (when *current-user* t)
                      :comment-action (format nil "/~A/~A/pulls/~A/diff-comment"
                                              org-name repo-name cs-num))))

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
            (:button.btn.btn-primary :type "submit" "Merge pull request")))))

      ;; Reviews
      (:section
       (:h2 "Reviews")
       (if reviews
           (dolist (r reviews)
             (:div :class (format nil "review~@[ review-stale~]" (getf r :is-stale))
              (:div.review-header
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

;;; ========================== ADMIN & SETTINGS ==========================

(defun view-admin (&key users message)
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
       "Manage users in Keycloak")))))

(defun view-settings (&key ssh-keys api-tokens new-token ssh-error
                           generated-private-key generated-key-name)
  "Render user settings page."
  (let ((cav-path (cav-download-path)))
    (page (:title "Settings — Cave")
      (:h1 "Settings")

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
