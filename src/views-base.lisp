;;; views.lisp — HTML views using Spinneret
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; All HTML generation lives here. Each page is a function that returns
;;; an HTML string. No template files, no compilation step, no stale state.

(defun identicon-data-uri (seed)
  "Deterministic GitHub-style identicon (5x5 mirrored grid) for SEED, as an
inline SVG data URI. Self-contained — no external avatar service, so it never
leaks the viewer's IP the way a remote Gravatar fetch would."
  (let* ((bytes (ironclad:digest-sequence
                 :sha256 (sb-ext:string-to-octets (or seed "anon") :external-format :utf-8)))
         (color (format nil "#~2,'0x~2,'0x~2,'0x"
                        ;; Bias toward mid-range so it's visible on light/dark.
                        (+ 64 (mod (aref bytes 0) 160))
                        (+ 64 (mod (aref bytes 1) 160))
                        (+ 64 (mod (aref bytes 2) 160))))
         (cells nil))
    (loop for col from 0 below 3 do
      (loop for row from 0 below 5 do
        (when (evenp (aref bytes (+ 3 (+ (* col 5) row))))
          (dolist (c (if (= col 2) (list 2) (list col (- 4 col))))
            (push (cons c row) cells)))))
    (let ((svg (with-output-to-string (s)
                 (format s "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 5 5'>")
                 (format s "<rect width='5' height='5' fill='#f0f0f0'/>")
                 (dolist (cell cells)
                   (format s "<rect x='~A' y='~A' width='1' height='1' fill='~A'/>"
                           (car cell) (cdr cell) color))
                 (format s "</svg>"))))
      (format nil "data:image/svg+xml;base64,~A"
              (cl-base64:string-to-base64-string svg)))))

(defun render-avatar (email &key (size 20) (class "avatar") (alt ""))
  "Render a deterministic identicon avatar for EMAIL. ALT defaults to empty
(decorative) for callers that show the name as adjacent text; pass a name for
standalone use."
  (spinneret:with-html
    (:img :src (identicon-data-uri email)
     :class class :width (princ-to-string size) :height (princ-to-string size)
     :alt alt
     :style "border-radius:3px;vertical-align:middle")))

(defun effective-theme ()
  "The active theme name.  Defaults to \"light\" unless the logged-in user has
explicitly chosen another theme."
  (let ((th (and *current-user* (getf *current-user* :theme))))
    (if (and (stringp th) (not (uiop:emptyp th))) th "light")))

(defmacro page ((&key title) &body body)
  "Wrap BODY in a full HTML page with nav and container."
  `(spinneret:with-html-string
     (:doctype)
     (:html :lang "en"
            :data-theme (effective-theme)
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
          (:a.nav-brand :href "/"
           (:raw "<svg class=\"nav-logo\" viewBox=\"0 0 256 256\" aria-hidden=\"true\"><g fill=\"none\" stroke=\"currentColor\" stroke-width=\"16\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M104 62a24 24 0 0 1 48 0\"/><line x1=\"104\" y1=\"62\" x2=\"104\" y2=\"78\"/><line x1=\"152\" y1=\"62\" x2=\"152\" y2=\"78\"/><path d=\"M84 84H172\"/><path d=\"M92 92H164\"/><path d=\"M96 92L84 176Q84 188 96 188H160Q172 188 172 176L160 92\"/><path d=\"M92 196H164\"/></g><g fill=\"currentColor\"><path d=\"M112 142l-14 14 14 14 8-8 -6-6 6-6z\"/><rect x=\"123\" y=\"136\" width=\"10\" height=\"40\" rx=\"5\" transform=\"rotate(15 128 156)\"/><path d=\"M144 142l14 14 -14 14 -8-8 6-6 -6-6z\"/></g></svg>")
           (:span "Cave"))
          (:div.nav-right
           (if *current-user*
               (progn
                 (when (config-value :zoekt-enabled)
                   (:form.nav-search :method "get" :action "/-/search"
                    (:input.nav-search-input :type "text" :name "q"
                     :placeholder "Search code..." :autocomplete "off")))
                 (let ((unread (ignore-errors (count-unread-notifications *current-user-id*))))
                   (:a.btn.btn-sm :href "/-/notifications" :title "Notifications"
                    :aria-label (if (and unread (plusp unread))
                                    (format nil "Notifications, ~A unread" unread)
                                    "Notifications")
                    (:raw "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\" style=\"vertical-align:middle\"><path d=\"M8 2a4 4 0 0 0-4 4c0 3-1.2 4.2-1.7 4.7a.5.5 0 0 0 .35.85h10.7a.5.5 0 0 0 .35-.85C13.2 10.2 12 9 12 6a4 4 0 0 0-4-4z\"/><path d=\"M6.5 13a1.5 1.5 0 0 0 3 0\"/></svg>")
                    (when (and unread (plusp unread))
                      (:span :style "color:var(--green,#7c9a5e);font-weight:600;margin-left:.35em"
                       (format nil "~A" unread)))))
                 (:a.btn.btn-sm :href "/-/explore" "Explore")
                 (:a.btn.btn-sm :href "/-/new-org" "New org")
                 (:a.btn.btn-sm :href "/-/settings" "Settings")
                 (when (getf *current-user* :is-admin)
                   (:a.btn.btn-sm :href "/-/admin" "Admin"))
                 (render-avatar (getf *current-user* :email) :size 20
                                :alt (format nil "~A avatar" (getf *current-user* :username)))
                 (:span.nav-user (getf *current-user* :username))
                 (:form :method "post" :action "/logout" :style "display:inline"
                  (:button.btn.btn-sm :type "submit" "Sign out")))
               (progn
                 (:a.btn.btn-sm :href "/-/explore" "Explore")
                 (:a.btn.btn-sm :href "/-/auth/login" "Sign in"))))))
        (:main.container ,@body)
        (:footer.site-footer
         (:span (format nil "Cave ~A" +version+)))
        ;; Lazy-render math (KaTeX) and diagrams (Mermaid) in any markdown on the
        ;; page. The CDN libraries load only when the relevant syntax is present.
        (:script (:raw +markdown-enhance-js+))))))

(defparameter +markdown-enhance-js+
  "(function(){
  var sel='.readme-content,.issue-body,.comment-body,.markdown-body';
  var md=document.querySelector(sel);
  if(!md)return;
  if(document.querySelector('code.language-mermaid,code.mermaid')){
    var m=document.createElement('script');
    m.src='https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js';
    m.onload=function(){
      document.querySelectorAll('code.language-mermaid,code.mermaid').forEach(function(c){
        var pre=c.closest('pre')||c,d=document.createElement('div');
        d.className='mermaid';d.textContent=c.textContent;pre.replaceWith(d);
      });
      var dark=(document.documentElement.getAttribute('data-theme')||'').indexOf('light')<0;
      mermaid.initialize({startOnLoad:false,theme:dark?'dark':'default'});
      mermaid.run();
    };
    document.head.appendChild(m);
  }
  if(md.textContent.indexOf('$')>=0){
    var l=document.createElement('link');l.rel='stylesheet';
    l.href='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css';
    document.head.appendChild(l);
    var k=document.createElement('script');
    k.src='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js';
    k.onload=function(){
      var a=document.createElement('script');
      a.src='https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js';
      a.onload=function(){
        document.querySelectorAll(sel).forEach(function(el){
          renderMathInElement(el,{delimiters:[
            {left:'$$',right:'$$',display:true},
            {left:'$',right:'$',display:false}]});
        });
      };
      document.head.appendChild(a);
    };
    document.head.appendChild(k);
  }
})();"
  "Client-side enhancer: renders KaTeX math and Mermaid diagrams in markdown.")

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
;;; Login is handled by the embedded Usher OIDC provider — no local login form needed.

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

(defun ->actor (value)
  "Coerce a postmodern :null / NIL / string actor field into a display name."
  (cond
    ((or (null value) (eq value :null)) "someone")
    (t value)))

(defun format-event (event)
  "Format an event as a short English sentence."
  (let ((type (getf event :event-type))
        (actor (->actor (getf event :actor)))
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

(defun view-account-pending (&key username)
  "Shown after OIDC callback for a self-registered user awaiting admin approval."
  (page (:title "Account pending — Cave")
    (:section :style "max-width:38rem;margin:var(--sp-6) auto;text-align:center"
     (:h1 "Almost there")
     (:p :style "color:var(--text-muted);font-size:1.05rem;line-height:1.5"
      (format nil "Thanks for signing up~@[, ~A~]. Your account is waiting for an administrator to approve it."
              username))
     (:p :style "color:var(--text-muted);margin-top:var(--sp-3)"
      "You'll be able to sign in once an admin gives the nod. Feel free to close this tab — there's nothing else to do here.")
     (:p :style "margin-top:var(--sp-4)"
      (:a.btn :href "/" "Back to the front page")))))

(defun view-account-rejected (&key username)
  "Shown after OIDC callback for a user whose application was rejected."
  (page (:title "Account not approved — Cave")
    (:section :style "max-width:38rem;margin:var(--sp-6) auto;text-align:center"
     (:h1 "Account not approved")
     (:p :style "color:var(--text-muted);font-size:1.05rem;line-height:1.5"
      (format nil "~@[~A, t~]~:[T~;t~]his account isn't approved for access to this instance."
              username username))
     (:p :style "color:var(--text-muted);margin-top:var(--sp-3)"
      "If you think that's a mistake, get in touch with the administrator directly.")
     (:p :style "margin-top:var(--sp-4)"
      (:a.btn :href "/" "Back to the front page")))))

(defun explore-url (q sort language page)
  "Build an /-/explore URL preserving the current query, sort, language, and page."
  (format nil "/-/explore?q=~A&sort=~A~@[&language=~A~]&page=~A"
          (hunchentoot:url-encode (or q "")) (or sort "recent")
          (when (and language (plusp (length language))) (hunchentoot:url-encode language))
          page))

(defun render-lang-tag (name &key (show-name t))
  "A colored language dot (Linguist color) optionally followed by the name.
No-op for a blank or :null NAME."
  (when (and name (not (eq name :null)) (plusp (length name)))
    (spinneret:with-html
      (:span :style "display:inline-flex;align-items:center;gap:.3rem;color:var(--text-muted);font-size:.78rem"
       (:span :title name
        :style (format nil "display:inline-block;width:.65em;height:.65em;border-radius:50%;flex:0 0 auto;background:~A"
                       (or (language-color name) "var(--text-muted,#888)")))
       (when show-name name)))))

(defun %repo-list-item (r &key meta)
  "Render one repo list <li> with owner/name, description, language, and META."
  (spinneret:with-html
    (:li
     (:a :href (format nil "/~A/~A" (getf r :owner-name) (getf r :name))
      (format nil "~A/~A" (getf r :owner-name) (getf r :name)))
     (let ((d (getf r :description)))
       (when (and d (not (eq d :null)) (plusp (length d)))
         (:span.desc d)))
     (render-lang-tag (getf r :primary-language))
     (when meta
       (:span :style "margin-left:auto;color:var(--text-muted);font-size:.8rem" meta)))))

(defun view-explore (&key repos total query sort (page 1) (per-page 30) trending users orgs
                          languages current-language)
  "Explore page: trending repos, searchable/sortable/paginated public repo list,
a language filter, and a people/organizations directory."
  (let* ((q (or query ""))
         (sort (or sort "recent"))
         (total (or total 0))
         (pages (max 1 (ceiling total per-page))))
    (page (:title "Explore — Cave")
      (:h1 "Explore")
      ;; Trending
      (when trending
        (:section
         (:h2 :style "font-size:1rem" "Trending this week")
         (:ul.repo-list
          (dolist (r trending)
            (%repo-list-item r :meta (format nil "~A view~:P" (getf r :views)))))))
      ;; Search + sort (language preserved across a search via a hidden field)
      (:form :method "get" :action "/-/explore"
       :style "display:flex;gap:.5rem;margin:var(--sp-3) 0;flex-wrap:wrap"
       (when current-language
         (:input :type "hidden" :name "language" :value current-language))
       (:input :type "text" :name "q" :value q :placeholder "Search repositories…"
               :style "flex:1;min-width:12rem")
       (:select :name "sort"
        (dolist (opt '(("recent" . "Recently updated") ("newest" . "Newest")
                       ("name" . "Name")))
          (:option :value (car opt) :selected (equal sort (car opt)) (cdr opt))))
       (:button.btn :type "submit" "Search")
       (:a.btn :href "/-/search" "Search code"))
      ;; Language filter chips
      (when languages
        (:div :style "display:flex;gap:.35rem;flex-wrap:wrap;align-items:center;margin-bottom:var(--sp-3)"
         (:span :style "font-size:.8rem;color:var(--text-muted)" "Languages:")
         (:a :class (format nil "badge~@[ btn-active~]" (null current-language))
          :style "text-decoration:none"
          :href (explore-url q sort nil 1) "all")
         (dolist (l languages)
           (let* ((name (getf l :language))
                  (color (or (language-color name) "var(--text-muted,#888)")))
             (:a :class (format nil "badge~@[ btn-active~]" (equal name current-language))
              :style "text-decoration:none;display:inline-flex;align-items:center;gap:.3rem"
              :href (explore-url q sort name 1)
              (:span :style (format nil "display:inline-block;width:.6em;height:.6em;border-radius:50%;background:~A" color))
              (format nil "~A (~A)" name (getf l :n)))))))
      ;; Repositories
      (:section
       (:h2 :style "font-size:1rem" (format nil "Repositories (~A)" total))
       (if repos
           (:ul.repo-list
            (dolist (r repos)
              (%repo-list-item
               r :meta (let ((pushed (or (getf r :last-pushed-at) (getf r :updated-at))))
                         (when (format-relative-time pushed)
                           (format nil "updated ~A" (format-relative-time pushed)))))))
           (:p.empty "No repositories found."))
       (when (> pages 1)
         (:div :style "display:flex;gap:.5rem;justify-content:center;align-items:center;margin-top:var(--sp-3)"
          (when (> page 1)
            (:a.btn.btn-sm :href (explore-url q sort current-language (1- page)) "← Prev"))
          (:span :style "color:var(--text-muted);font-size:.85rem"
           (format nil "Page ~A of ~A" page pages))
          (when (< page pages)
            (:a.btn.btn-sm :href (explore-url q sort current-language (1+ page)) "Next →")))))
      ;; People & organizations
      (:div :style "display:grid;grid-template-columns:1fr 1fr;gap:var(--sp-4);align-items:start;margin-top:var(--sp-4)"
       (:section
        (:h2 :style "font-size:1rem" "People")
        (if users
            (:ul.repo-list
             (dolist (u users)
               (:li (render-avatar (getf u :email) :size 16)
                (:a :href (format nil "/~A" (getf u :username))
                 :style "margin-left:.4rem" (getf u :username)))))
            (:p.empty "No users.")))
       (:section
        (:h2 :style "font-size:1rem" "Organizations")
        (if orgs
            (:ul.repo-list
             (dolist (o orgs)
               (:li (:a :href (format nil "/~A" (getf o :name)) (getf o :name))
                (let ((d (getf o :description)))
                  (when (and d (not (eq d :null)) (plusp (length d)))
                    (:span.desc d))))))
            (:p.empty "No organizations.")))))))

(defun view-public-landing (&key repos events hero-html)
  "Anonymous landing page. The hero/intro is rendered from the cave/cave-landing
repo's index.md (HERO-HTML) when present, so the copy is editable via git with no
redeploy; otherwise a built-in default is shown. Cave always appends the live
data: featured repositories, recent activity, and instance stats."
  (let ((featured (if (> (length repos) 8) (subseq repos 0 8) repos)))
    (page (:title "Cave")
      ;; Hero — from cave/cave-landing:index.md, or a built-in default.
      (:section :style "text-align:center;padding:var(--sp-5) 0 var(--sp-4)"
       (if hero-html
           (:div.readme-content
            :style "max-width:48rem;margin:0 auto;text-align:left"
            (:raw hero-html))
           (progn
             (:h1 :style "font-size:2.4rem;margin:0 0 .35rem;letter-spacing:.12em" "Cave")
             (:p :style "color:var(--text);font-size:1.1rem;margin:0 0 .25rem"
              "A self-hosted code forge in Common Lisp")
             (:p :style "color:var(--text-muted);font-size:.9rem;margin:0"
              "push · review · merge · deploy — own your infrastructure")))
       ;; A custom hero (cave-landing:index.md) already links API docs + CLI, so
       ;; only surface the API-docs button on the built-in default hero — avoids
       ;; the duplicate "API docs" in one viewport on instances with custom copy.
       (:div :style "display:flex;gap:.5rem;justify-content:center;flex-wrap:wrap;margin-top:1.25rem"
        (unless hero-html
          (:a.btn :href "/api/v1/docs" "API docs"))
        (:a.btn :href "/-/auth/login" "Sign in")
        (:a.btn.btn-primary :href "/-/register" "Register")))
      ;; Two columns: featured repos | recent activity
      (:div :style "display:grid;grid-template-columns:1fr 1fr;gap:var(--sp-4);align-items:start"
       (:section
        (:div :style "display:flex;justify-content:space-between;align-items:baseline"
         (:h2 "Repositories")
         (:a :href "/-/explore" :style "font-size:.85rem" "Browse all →"))
        (if featured
            (:ul.repo-list
             (dolist (repo featured)
               (let ((owner (getf repo :owner-name))
                     (desc (getf repo :description)))
                 (:li
                  (:a :href (format nil "/~A/~A" owner (getf repo :name))
                   (format nil "~A/~A" owner (getf repo :name)))
                  (when (and desc (not (eq desc :null)) (plusp (length desc)))
                    (:span.desc desc))))))
            (:p.empty "No public repositories yet.")))
       (:section
        (:h2 "Recent activity")
        (if events
            (:ul.issue-list
             (dolist (ev events)
               (:li
                (:span (format-event ev))
                (let ((rel (format-relative-time (getf ev :created-at))))
                  (when rel
                    (:span :style "color:var(--text-muted);font-size:.8rem;margin-left:.5rem" rel))))))
            (:p.empty "No activity yet."))))
      ;; Stats footer
      (:p :style "text-align:center;color:var(--text-muted);font-size:.85rem;margin-top:var(--sp-4)"
       (format nil "~D public repositor~:@P · running Cave ~A" (length repos) +version+)))))

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

