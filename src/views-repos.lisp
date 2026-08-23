(in-package #:cave)

;;; ========================== REPO PAGES ==========================

(defun view-new-repo (&key org error)
  "Render the new repo form for an org."
  (let ((org-name (getf org :name)))
    (page (:title "New repository — Cave")
      (:h1 (format nil "New repository in ~A" org-name))
      (render-new-repo-form (format nil "/o/~A/-/new-repo" org-name) :error error))))

(defun render-repo-tabs (owner-name repo-name &optional active-tab &key repo
                                                                        ref default-branch)
  "Render the repo breadcrumb and navigation tab bar. When REF is a non-default
   ref, the Overview and Code tabs carry ?ref=<ref> so the selected branch/tag
   persists as the user moves between those two views."
  (let ((q (if (and ref default-branch (not (equal ref default-branch)))
               (format nil "?ref=~A" (hunchentoot:url-encode ref))
               "")))
   (spinneret:with-html
    (render-breadcrumbs
     (list (list (format nil "/~A" owner-name) owner-name)
           repo-name))
    (when (and repo (getf repo :is-private)) (:span.badge "private"))
    (when (and repo (getf repo :is-archived)) (:span.badge "archived"))
    (:nav.repo-tabs
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :overview))
      :href (format nil "/~A/~A~A" owner-name repo-name q) "Overview")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :code))
      :href (format nil "/~A/~A/code~A" owner-name repo-name q) "Code")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :issues))
      :href (format nil "/~A/~A/issues" owner-name repo-name)
      (let ((n (and repo (count-open-issues (getf repo :id)))))
        (if (and n (plusp n)) (format nil "Issues (~A)" n) "Issues")))
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :pulls))
      :href (format nil "/~A/~A/pulls" owner-name repo-name)
      (let ((n (and repo (count-open-changesets (getf repo :id)))))
        (if (and n (plusp n)) (format nil "Pull requests (~A)" n) "Pull requests")))
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :runs))
      :href (format nil "/~A/~A/runs" owner-name repo-name) "Runs")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :releases))
      :href (format nil "/~A/~A/releases" owner-name repo-name) "Releases")
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :security))
      :href (format nil "/~A/~A/deps" owner-name repo-name) "Security")
     (when (and repo *current-user-id*
                (repo-member-role (getf repo :id) *current-user-id*))
       (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :pulse))
        :href (format nil "/~A/~A/pulse" owner-name repo-name) "Pulse"))
     (:a :class (format nil "repo-tab~@[ repo-tab-active~]" (eq active-tab :settings))
      :href (format nil "/~A/~A/settings" owner-name repo-name) "Settings")))))

(defparameter *folder-svg*
  "<svg class=\"ficon\" viewBox=\"0 0 16 16\" width=\"16\" height=\"16\" aria-hidden=\"true\"><path fill=\"currentColor\" d=\"M1.75 1A1.75 1.75 0 0 0 0 2.75v10.5C0 14.216.784 15 1.75 15h12.5A1.75 1.75 0 0 0 16 13.25v-8.5A1.75 1.75 0 0 0 14.25 3H7.5a.25.25 0 0 1-.2-.1l-.9-1.2C6.07 1.26 5.55 1 5 1Z\"/></svg>")

(defparameter *file-svg-path*
  "M2 1.75C2 .784 2.784 0 3.75 0h6.586c.464 0 .909.184 1.237.513l2.914 2.914c.329.328.513.773.513 1.237v9.586A1.75 1.75 0 0 1 13.75 16h-10A1.75 1.75 0 0 1 2 14.25Zm1.75-.25a.25.25 0 0 0-.25.25v12.5c0 .138.112.25.25.25h10a.25.25 0 0 0 .25-.25V6h-2.75A1.75 1.75 0 0 1 9.5 4.25V1.5Zm6.75.062V4.25c0 .138.112.25.25.25h2.688l-.011-.013-2.914-2.914-.013-.011Z")

(defun render-file-icon (is-dir name)
  "Inline SVG icon: a folder for directories, a document for files (tinted by
   the file's language color when recognized). Emitted as raw markup; colors
   come from the fixed language table, never from user input."
  (spinneret:with-html
    (if is-dir
        (:raw *folder-svg*)
        (multiple-value-bind (lang color) (file-language-info name)
          (declare (ignore lang))
          (:raw (format nil "<svg class=\"ficon\" viewBox=\"0 0 16 16\" width=\"16\" height=\"16\" aria-hidden=\"true\"~@[ style=\"color:~A\"~]><path fill=\"currentColor\" d=\"~A\"/></svg>"
                        color *file-svg-path*))))))

(defun render-language-bar (stats)
  "A thin proportional language bar + legend with percentages. STATS is a list
   of (name color bytes), largest first."
  (when stats
    (let* ((total (reduce #'+ stats :key #'third))
           (shown (subseq stats 0 (min 8 (length stats))))
           (rest (nthcdr 8 stats))
           (other (reduce #'+ rest :key #'third)))
      (when (plusp total)
        (flet ((pct (b) (* 100.0 (/ b total))))
          (spinneret:with-html
            (:div.lang-bar
             (dolist (row shown)
               (destructuring-bind (name color bytes) row
                 (declare (ignore name))
                 (:span.lang-seg
                  :style (format nil "width:~,2F%;background:~A" (pct bytes) (or color "#ccc"))
                  :title (format nil "~A ~,1F%" (first row) (pct bytes)))))
             (when (plusp other)
               (:span.lang-seg :style (format nil "width:~,2F%;background:#ccc" (pct other))
                :title "Other")))
            (:div.lang-legend
             (dolist (row shown)
               (destructuring-bind (name color bytes) row
                 (:span.lang-legend-item
                  (:span.lang-dot :style (format nil "background:~A" (or color "#ccc")))
                  (:span.lang-name name)
                  (:span.lang-pct (format nil "~,1F%" (pct bytes))))))
             (when (plusp other)
               (:span.lang-legend-item
                (:span.lang-dot :style "background:#ccc")
                (:span.lang-name "Other")
                (:span.lang-pct (format nil "~,1F%" (pct other))))))))))))

(defun render-file-tree (file-tree owner-name repo-name default-branch
                         &optional current-path &key last-commits)
  "Render a file tree table. Shared by view-code and view-tree. When LAST-COMMITS
   (a name -> commit-plist hash-table) is given, each row shows the most recent
   commit that touched the entry and how long ago, like other forges."
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
                              (repo-url owner-name repo-name)
                              (tree-url owner-name repo-name default-branch parent))
                 ".."))
           (:td.file-commit "")
           (:td.file-age ""))))
      (dolist (entry file-tree)
        (let* ((name (getf entry :name))
               (is-dir (equal (getf entry :type) "tree"))
               (entry-path (if (and current-path (not (uiop:emptyp current-path)))
                               (format nil "~A/~A" current-path name)
                               name))
               (commit (and last-commits (gethash name last-commits))))
          (:tr :class (when is-dir "file-dir")
           (:td.file-icon (render-file-icon is-dir name))
           (:td
            (:a :href (if is-dir
                          (tree-url owner-name repo-name default-branch entry-path)
                          (blob-url owner-name repo-name default-branch entry-path))
             name))
           (:td.file-commit
            (when commit
              (:a :href (format nil "/~A/~A/commit/~A"
                                owner-name repo-name (getf commit :hash))
               :title (getf commit :subject)
               (getf commit :subject))))
           (:td.file-age
            (when commit
              (format-relative-time (getf commit :time)))))))))))

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

(defun view-repo (&key owner-name repo empty default-branch current-ref
                       branches tags readme-html readme-filename)
  "Render the repo overview page (README + clone URL)."
  (let* ((org-name owner-name)
         (repo-name (getf repo :name))
         (current-ref (or current-ref default-branch)))
    (page (:title (format nil "~A/~A — Cave" org-name repo-name))
      (render-repo-tabs org-name repo-name :overview :repo repo
                        :ref current-ref :default-branch default-branch)
      (when (getf repo :description) (:p (getf repo :description)))
      ;; Branch/tag switcher — picking a ref re-renders the README at that ref
      ;; (stays on the overview, preserving the selection).
      (unless empty
        (:div.repo-info-bar
         (:div.repo-info-left
          (render-ref-switcher org-name repo-name current-ref branches tags
                               :can-write (and *current-user-id*
                                               (repo-member-role (getf repo :id)
                                                                 *current-user-id*))
                               :href-fn (lambda (r)
                                          (if (equal r default-branch)
                                              (format nil "/~A/~A" org-name repo-name)
                                              (format nil "/~A/~A?ref=~A" org-name repo-name
                                                      (hunchentoot:url-encode r)))))
          ;; Match the Code tab's bar: don't leave this container empty.
          (:span.repo-info-stat
           (format nil "~A ~:[branches~;branch~]" (length branches) (= (length branches) 1)))
          (when tags
            (:span.repo-info-stat
             (format nil "~A ~:[tags~;tag~]" (length tags) (= (length tags) 1)))))))
      ;; Clone widget — SSH/HTTPS toggle with copy button
      (render-clone-widget org-name repo-name)
      ;; Watch / unwatch toggle — subscribe to in-app notifications
      (when *current-user*
        (:form :method "post" :style "display:inline-block;margin-bottom:var(--sp-4);margin-right:var(--sp-2)"
         :action (format nil "/~A/~A/watch" org-name repo-name)
         (:button.btn :type "submit"
          (if (watching-repo-p (getf repo :id) *current-user-id*)
              "Unwatch" "Watch"))))
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

(defun render-verified-badge (sig)
  "Render the green Verified / amber Unverified pill if SIG is non-NIL."
  (when sig
    (spinneret:with-html
      (cond
        ((getf sig :verified)
         (:span :title (or (getf sig :fingerprint) "")
          :style "display:inline-block;padding:.05rem .35rem;margin-left:.4rem;border-radius:3px;font-size:.65rem;font-family:var(--font-mono);background:var(--green-bg, rgba(124,154,94,.15));color:var(--green,#7c9a5e);border:1px solid var(--green,#7c9a5e)"
          "Verified"))
        ((getf sig :scheme)
         (:span :title (format nil "Signed with ~A but signature not verified"
                               (getf sig :scheme))
          :style "display:inline-block;padding:.05rem .35rem;margin-left:.4rem;border-radius:3px;font-size:.65rem;font-family:var(--font-mono);background:var(--yellow-bg, rgba(201,160,62,.15));color:var(--yellow,#c9a03e);border:1px solid var(--yellow,#c9a03e)"
          "Unverified"))))))

(defun render-ref-switcher (owner-name repo-name current-ref branches tags
                            &key can-write href-fn)
  "A branch/tag dropdown: filter, Branches/Tags tabs, click-to-switch, and — for
   members — inline 'Create branch <name> from <current>' when the filter matches
   no existing branch. HREF-FN maps a ref name to the URL to navigate to; it lets
   the same switcher keep you on the Overview, Code, or tree page (preserving the
   selected ref) rather than always jumping to /tree/<ref>."
  (let ((href-fn (or href-fn
                     (lambda (r) (format nil "/~A/~A/tree/~A" owner-name repo-name r)))))
   (spinneret:with-html
    (:div.ref-switcher
     (:button.ref-switcher-btn :type "button" :onclick "caveToggleRefMenu(this)"
       (:span.ref-switcher-name current-ref) " ▾")
     (:div.ref-switcher-menu :hidden t
      (:input.ref-filter :type "text" :placeholder "Filter branches/tags…"
        :oninput "caveFilterRefs(this)" :autocomplete "off")
      (:div.ref-switcher-tabs
       (:button.ref-kind-tab.active :type "button" :data-kind "branches"
         :onclick "caveShowRefKind(this,'branches')" "Branches")
       (:button.ref-kind-tab :type "button" :data-kind "tags"
         :onclick "caveShowRefKind(this,'tags')" "Tags"))
      (:ul.ref-list :data-kind "branches"
       (dolist (b branches)
         (:li (:a :href (funcall href-fn b)
                  :class (when (equal b current-ref) "current") b))))
      (:ul.ref-list :data-kind "tags" :hidden t
       (if tags
           (dolist (tg tags)
             (:li (:a :href (funcall href-fn tg)
                      :class (when (equal tg current-ref) "current") tg)))
           (:li.ref-empty "No tags")))
      (when can-write
        (:form.ref-create :method "post"
          :action (format nil "/~A/~A/branches" owner-name repo-name) :hidden t
          (:input :type "hidden" :name "from" :value current-ref)
          (:input :type "hidden" :name "name" :class "ref-create-name")
          (:button :type "submit"
            "Create branch “" (:span.ref-create-label) "” from " current-ref)))))
    (:style (:raw "
.ref-switcher{position:relative;display:inline-block;vertical-align:middle}
.ref-switcher-btn{cursor:pointer;font:inherit;background:#f6f6f6;border:1px solid #d0d0d0;border-radius:5px;padding:2px 10px}
.ref-switcher-btn:hover{background:#efefef}
.ref-switcher-name{font-weight:600}
.ref-switcher-menu{position:absolute;left:0;z-index:30;margin-top:4px;min-width:260px;max-height:360px;overflow:auto;background:#fff;border:1px solid #d0d0d0;border-radius:6px;box-shadow:0 6px 20px rgba(0,0,0,.14)}
.ref-filter{display:block;width:calc(100% - 20px);margin:8px 10px;padding:5px 8px;border:1px solid #d0d0d0;border-radius:5px;font:inherit}
.ref-switcher-tabs{display:flex;border-bottom:1px solid #eee}
.ref-kind-tab{flex:1;cursor:pointer;background:none;border:none;padding:7px;font:inherit;color:#555}
.ref-kind-tab.active{color:#c2410c;box-shadow:inset 0 -2px 0 #c2410c}
.ref-list{list-style:none;margin:0;padding:4px 0}
.ref-list li a{display:block;padding:5px 12px;color:inherit;text-decoration:none}
.ref-list li a:hover{background:#f4f4f4}
.ref-list li a.current{font-weight:700}
.ref-empty{padding:6px 12px;color:#888}
.ref-create{padding:8px;border-top:1px solid #eee}
.ref-create button{width:100%;cursor:pointer;font:inherit;text-align:left;background:#fff;border:1px solid #d0d0d0;border-radius:5px;padding:6px 8px}
.ref-create button:hover{background:#f4f4f4}"))
    (:script (:raw "
function caveToggleRefMenu(btn){var m=btn.parentNode.querySelector('.ref-switcher-menu');var open=m.hasAttribute('hidden');document.querySelectorAll('.ref-switcher-menu').forEach(function(x){x.setAttribute('hidden','')});if(open){m.removeAttribute('hidden');var f=m.querySelector('.ref-filter');if(f)f.focus();}}
function caveShowRefKind(btn,kind){var m=btn.closest('.ref-switcher-menu');m.querySelectorAll('.ref-kind-tab').forEach(function(t){t.classList.toggle('active',t.dataset.kind===kind);});m.querySelectorAll('.ref-list').forEach(function(l){l.hidden=(l.dataset.kind!==kind);});caveFilterRefs(m.querySelector('.ref-filter'));}
function caveFilterRefs(input){if(!input)return;var m=input.closest('.ref-switcher-menu');var q=input.value.trim().toLowerCase();var kind=m.querySelector('.ref-kind-tab.active').dataset.kind;var list=m.querySelector('.ref-list[data-kind=\"'+kind+'\"]');var exact=false;list.querySelectorAll('li').forEach(function(li){var t=li.textContent.trim().toLowerCase();var match=t.indexOf(q)!==-1;li.hidden=!match;if(t===q)exact=true;});var form=m.querySelector('.ref-create');if(form){var show=(kind==='branches'&&q.length>0&&!exact);form.hidden=!show;if(show){form.querySelector('.ref-create-name').value=input.value.trim();form.querySelector('.ref-create-label').textContent=input.value.trim();}}}
document.addEventListener('click',function(e){if(!e.target.closest('.ref-switcher'))document.querySelectorAll('.ref-switcher-menu').forEach(function(x){x.setAttribute('hidden','')});});")))))

(defun view-code (&key owner-name repo branches tags default-branch current-ref
                       commit-count recent-commits file-tree signatures last-commits
                       language-stats)
  "Render the repo code/file browser page."
  (let* ((org-name owner-name)
         (repo-name (getf repo :name))
         (current-ref (or current-ref default-branch)))
    (page (:title (format nil "Code — ~A/~A" org-name repo-name))
      (render-repo-tabs org-name repo-name :code :repo repo
                        :ref current-ref :default-branch default-branch)
      (render-clone-widget org-name repo-name)

      ;; Branch/tag bar + last commit
      (:div.repo-info-bar
       (:div.repo-info-left
        (render-ref-switcher org-name repo-name current-ref branches tags
                             :can-write (and *current-user-id*
                                             (repo-member-role (getf repo :id)
                                                               *current-user-id*))
                             :href-fn (lambda (r)
                                        (if (equal r default-branch)
                                            (format nil "/~A/~A/code" org-name repo-name)
                                            (format nil "/~A/~A/code?ref=~A" org-name repo-name
                                                    (hunchentoot:url-encode r)))))
        (:span.repo-info-stat
         (format nil "~A ~:[branches~;branch~]" (length branches) (= (length branches) 1)))
        (when tags
          (:span.repo-info-stat
           (format nil "~A ~:[tags~;tag~]" (length tags) (= (length tags) 1)))))
       (when commit-count
         (:span.repo-info-stat
          (format nil "~A ~:[commits~;commit~]" commit-count (= commit-count 1)))))
      ;; Language breakdown bar
      (render-language-bar language-stats)
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
        (render-file-tree file-tree org-name repo-name current-ref nil
                          :last-commits last-commits))

      ;; Recent commits
      (when recent-commits
        (:section
         (:h2 "Recent commits")
         (:ul.issue-list
          (dolist (c recent-commits)
            (let ((sig (when signatures (gethash (getf c :hash) signatures))))
              (:li
               (:a :href (format nil "/~A/~A/commit/~A" org-name repo-name (getf c :hash))
                (:code :style "color:var(--link);font-size:.8rem" (getf c :short-hash)))
               (:span (getf c :subject))
               (render-verified-badge sig)
               (:span :style "margin-left:auto;color:var(--text-muted);font-size:.8rem"
                (getf c :author)))))))))))

;;; ========================== TREE & BLOB PAGES ==========================

(defun view-tree (&key owner-name repo ref path file-tree
                       branches tags default-branch last-commits)
  "Render a directory listing at a path."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" path owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo
                        :ref ref :default-branch default-branch)
      (render-breadcrumbs
       (append (list (list (format nil "/~A" owner-name) owner-name)
                     (list (format nil "/~A/~A" owner-name repo-name) repo-name))
               (when (and path (not (uiop:emptyp path)))
                 (let ((parts (uiop:split-string path :separator '(#\/)))
                       (crumbs nil)
                       (built ""))
                   (dolist (part parts)
                     (setf built (if (uiop:emptyp built) part (format nil "~A/~A" built part)))
                     (push (list (tree-url owner-name repo-name ref built)
                                 part)
                           crumbs))
                   ;; Last one is just text, not a link
                   (let ((reversed (nreverse crumbs)))
                     (append (butlast reversed)
                             (list (second (car (last reversed))))))))))
      (:div.repo-info-bar
       (:div.repo-info-left
        (render-ref-switcher owner-name repo-name ref branches tags
                             :can-write (and *current-user-id*
                                             (repo-member-role (getf repo :id)
                                                               *current-user-id*)))))
      (if file-tree
          (render-file-tree file-tree owner-name repo-name ref path
                            :last-commits last-commits)
          (:p.empty "Empty directory.")))))

(defun json-for-script (s)
  "JSON-stringify a string for safe inlining inside a <script> tag.
   jzon emits a literal </script> when the input contains one (e.g. a
   source file with embedded <script> templates), and the browser's HTML
   parser closes the surrounding <script> early. Replace </ with <\\/ —
   a JSON-legal escape that JavaScript treats identically to / but that
   the HTML parser won't match as a script-close."
  (let* ((json (com.inuoe.jzon:stringify s))
         (out (make-string-output-stream))
         (i 0))
    (loop while (< i (length json)) do
      (cond ((and (char= (char json i) #\<)
                  (< (1+ i) (length json))
                  (char= (char json (1+ i)) #\/))
             (write-string "<\\/" out)
             (incf i 2))
            (t (write-char (char json i) out)
               (incf i))))
    (get-output-stream-string out)))

(defun view-blob (&key owner-name repo ref path content is-binary file-size language
                       branches tags default-branch
                       is-markdown (view-mode :source) rendered-html)
  "Render a file content page.
Markdown files render to HTML by default (VIEW-MODE :rendered, RENDERED-HTML
supplied); VIEW-MODE :source shows the Monaco source viewer. A Raw link always
serves the unrendered bytes."
  (let ((repo-name (getf repo :name))
        (filename (let ((slash (position #\/ path :from-end t)))
                    (if slash (subseq path (1+ slash)) path))))
    (page (:title (format nil "~A — ~A/~A" path owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo
                        :ref ref :default-branch default-branch)
      ;; Branch/tag switcher — switching keeps the current file path, so you can
      ;; view the same file across refs (as GitHub/GitLab/Gitea do).
      (when (or branches tags)
        (:div.repo-info-bar
         (:div.repo-info-left
          (render-ref-switcher owner-name repo-name ref branches tags
                               :can-write (and *current-user-id*
                                               (repo-member-role (getf repo :id)
                                                                 *current-user-id*))
                               :href-fn (lambda (r)
                                          (blob-url owner-name repo-name r path))))))
      (render-breadcrumbs
       (append (list (list (format nil "/~A" owner-name) owner-name)
                     (list (format nil "/~A/~A" owner-name repo-name) repo-name))
               ;; Every intermediate path segment is a directory, so use /tree/.
               ;; The final segment is the file itself and is rendered text-only
               ;; (no link), so its URL doesn't matter.
               (let ((parts (uiop:split-string path :separator '(#\/)))
                     (crumbs nil)
                     (built ""))
                 (dolist (part parts)
                   (setf built (if (uiop:emptyp built) part (format nil "~A/~A" built part)))
                   (push (list (tree-url owner-name repo-name ref built)
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
       (:div.blob-view-toggle :style "margin-left:auto;display:flex;gap:0"
        (when is-markdown
          (if (eq view-mode :rendered)
              (:span.btn.btn-sm.btn-active "Rendered")
              (:a.btn.btn-sm :href (blob-url owner-name repo-name ref path)
               "Rendered")))
        (when is-markdown
          (if (eq view-mode :source)
              (:span.btn.btn-sm.btn-active "Source")
              (:a.btn.btn-sm :href (format nil "/~A/~A/blob/~A?path=~A&view=source"
                                           owner-name repo-name ref path)
               "Source")))
        (:a.btn.btn-sm :href (format nil "/~A/~A/raw/~A?path=~A"
                                     owner-name repo-name ref path)
         "Raw")))
      (cond
        ((and (eq view-mode :rendered) rendered-html)
         (:div.readme-content
          :style "background:var(--surface);border:1px solid var(--border);border-top:none;padding:var(--sp-6)"
          (:raw rendered-html)))
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
  var search = params.get('search');
  // Line target: prefer #L42 fragment (the shape we emit on click and the one
  // people are used to from GitHub); fall back to legacy ?line=42.
  var hashMatch = (window.location.hash || '').match(/^#L(\\d+)/);
  var line = hashMatch ? parseInt(hashMatch[1]) : parseInt(params.get('line'));
  var deco = [];
  function highlightLine(n) {
    if (!n || n < 1) { deco = ed.deltaDecorations(deco, []); return; }
    deco = ed.deltaDecorations(deco, [{
      range: new monaco.Range(n, 1, n, 1),
      options: { isWholeLine: true, className: 'cave-line-link',
                 linesDecorationsClassName: 'cave-line-link-gutter' }
    }]);
  }
  if (line) {
    ed.revealLineInCenter(line);
    ed.setPosition({ lineNumber: line, column: 1 });
    highlightLine(line);
  }
  // Per-line action menu, hung off a Monaco ContentWidget so the editor
  // owns the positioning — it stays anchored to the line through scroll
  // and viewport flips above/below when there's no room.
  var menuDom = document.createElement('div');
  menuDom.className = 'cave-line-menu';
  var menuLine = null;
  var menuWidget = {
    allowEditorOverflow: true,
    getId: function() { return 'cave.line-menu'; },
    getDomNode: function() { return menuDom; },
    getPosition: function() {
      if (menuLine === null) return null;
      return {
        position: { lineNumber: menuLine, column: 1 },
        preference: [
          monaco.editor.ContentWidgetPositionPreference.BELOW,
          monaco.editor.ContentWidgetPositionPreference.ABOVE
        ]
      };
    }
  };
  ed.addContentWidget(menuWidget);

  function buildMenu(n) {
    var permalink = location.origin + location.pathname + location.search + '#L' + n;
    var lineText = ed.getModel().getLineContent(n);
    var refBody = '#' + permalink + '\\n\\n```\\n' + lineText + '\\n```';
    var issueHref = '~A/issues/new?body=' + encodeURIComponent(refBody);
    menuDom.innerHTML =
        '<button type=\"button\" class=\"cave-line-menu-item\" data-act=\"copy-line\">Copy line</button>' +
        '<button type=\"button\" class=\"cave-line-menu-item\" data-act=\"copy-link\">Copy permalink</button>' +
        '<a class=\"cave-line-menu-item\" href=\"' + issueHref + '\">Reference in new issue</a>';
    menuDom.querySelector('[data-act=\"copy-line\"]').onclick = function() {
      navigator.clipboard && navigator.clipboard.writeText(lineText);
      hideMenu();
    };
    menuDom.querySelector('[data-act=\"copy-link\"]').onclick = function() {
      navigator.clipboard && navigator.clipboard.writeText(permalink);
      hideMenu();
    };
  }
  function showMenu(n) {
    buildMenu(n);
    menuLine = n;
    ed.layoutContentWidget(menuWidget);
  }
  function hideMenu() {
    menuLine = null;
    ed.layoutContentWidget(menuWidget);
  }
  ed.onMouseDown(function(e) {
    if (e.target && e.target.type === monaco.editor.MouseTargetType.GUTTER_LINE_NUMBERS) {
      var n = e.target.position && e.target.position.lineNumber;
      if (!n) return;
      history.replaceState(null, '', '#L' + n);
      highlightLine(n);
      showMenu(n);
    }
  });
  // Click anywhere outside the menu or line-numbers gutter dismisses.
  document.addEventListener('click', function(e) {
    if (e.target.closest('.cave-line-menu')) return;
    if (e.target.closest('.line-numbers')) return;
    if (menuLine !== null) hideMenu();
  });
  if (search) {
    var fc = ed.getContribution('editor.contrib.findController');
    fc.setSearchString(search);
    fc.start({ forceRevealReplace: false, seedSearchStringFromSelection: 'none',
               shouldFocus: 0, shouldAnimate: true, loop: true });
  }
  }
});"
                  (json-for-script content)
                  (json-for-script content)
                  (json-for-script (or language "plaintext"))
                  (format nil "/~A/~A" owner-name repo-name)))))))))

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
" (json-for-script (or raw-diff "")))))))

(defun view-commit (&key owner-name repo commit diff-raw diff-stat
                         signature trailers)
  "Render a commit detail page with diff."
  (declare (ignore diff-stat))
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "~A — ~A/~A" (getf commit :short-hash) owner-name repo-name))
      (render-repo-tabs owner-name repo-name :code :repo repo)
      (render-breadcrumbs
       (list (list (format nil "/~A" owner-name) owner-name)
             (list (format nil "/~A/~A" owner-name repo-name) repo-name)
             (getf commit :short-hash)))
      (:div.commit-header
       (:h1.commit-subject
        (getf commit :subject)
        (render-verified-badge signature))
       (when (and (getf commit :body) (not (uiop:emptyp (getf commit :body))))
         (:pre.commit-body (getf commit :body))))
      (:div.commit-meta
       (:strong (getf commit :author))
       (:span :style "margin-left:var(--sp-2);color:var(--text-muted)"
        (getf commit :date))
       (:code :style "margin-left:auto" (getf commit :hash)))
      ;; Trailer chips — Co-Authored-By / Signed-off-by etc.
      (when trailers
        (:div :style "display:flex;gap:.5rem;flex-wrap:wrap;margin-top:.5rem"
         (dolist (tr trailers)
           (:span :style "padding:.1rem .45rem;border:1px solid var(--border);border-radius:3px;font-family:var(--font-mono);font-size:.75rem;color:var(--text-muted)"
            (format nil "~A: ~A" (car tr) (cdr tr))))))
      (when diff-raw
        (render-diff2html diff-raw)))))

