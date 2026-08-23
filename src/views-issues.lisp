(in-package #:cave)

;;; ========================== ISSUE PAGES ==========================

(defun %label-hue (s)
  "Deterministic hue 0-359 for a label string, so each label gets a stable,
distinct color without a stored color column."
  (let ((h 0))
    (loop for ch across (string s)
          do (setf h (mod (+ (* h 31) (char-code ch)) 360)))
    h))

(defun %label-style (l)
  "Inline pill style (pastel bg + dark text) for label L, in both light/dark themes."
  (let ((hue (%label-hue l)))
    (format nil "background:hsl(~D 70% 90%);color:hsl(~D 60% 26%);border-color:hsl(~D 45% 78%)"
            hue hue hue)))

(defun view-issues (&key owner-name repo issues current-status
                         labels-by-issue current-label all-labels
                         comment-counts authors)
  "Render the issues list — a triage surface: status glyph, title, colored
labels, and a metadata line (number, author, age, comment count)."
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
       (:div :style "display:flex;gap:var(--sp-2)"
        (when *current-user*
          (:a.btn.btn-sm :href (format nil "/~A/~A/milestones" org-name repo-name)
           "Milestones"))
        (when *current-user*
          (:a.btn.btn-primary :href (format nil "/~A/~A/issues/new" org-name repo-name)
           "New issue"))))
      ;; Label filter bar
      (when all-labels
        (:div.label-filters :style "margin:.5rem 0;display:flex;gap:.35rem;flex-wrap:wrap;align-items:center"
         (:span :style "font-size:.8rem;color:var(--text-muted)" "Labels:")
         (when current-label
           (:a.btn.btn-sm :href (format nil "?status=~A" (or current-status "open")) "✕ clear"))
         (dolist (l all-labels)
           (:a :class (if (equal l current-label) "issue-label issue-label-active" "issue-label")
            :style (%label-style l)
            :href (format nil "?status=~A&label=~A" (or current-status "open")
                          (hunchentoot:url-encode l))
            l))))
      (if issues
          (:ul.issues
           (dolist (iss issues)
             (let* ((open (equal (getf iss :status) "open"))
                    (num (getf iss :number))
                    (iid (getf iss :id))
                    (pinned (let ((p (getf iss :pin-order))) (and p (not (eq p :null)))))
                    (author (and authors (gethash (getf iss :author-id) authors)))
                    (ago (format-relative-time (getf iss :created-at)))
                    (ncomments (or (and comment-counts (gethash iid comment-counts)) 0))
                    (labels (and labels-by-issue (gethash iid labels-by-issue))))
               (:li.issue-row
                (:span.issue-icon
                 :title (getf iss :status)
                 :style (format nil "background:~A" (if open "#3fb950" "#a371f7")))
                (:div.issue-main
                 (:div.issue-titleline
                  (when pinned (:span.issue-pin "📌"))
                  (:a.issue-title
                   :href (format nil "/~A/~A/issues/~A" org-name repo-name num)
                   (getf iss :title))
                  (dolist (l labels)
                    (:a.issue-label
                     :style (%label-style l)
                     :href (format nil "?status=~A&label=~A" (or current-status "open")
                                   (hunchentoot:url-encode l))
                     l)))
                 (:div.issue-meta
                  (format nil "#~A" num)
                  (when author (format nil " · opened by ~A" author))
                  (when ago (format nil " · ~A" ago))
                  (when (plusp ncomments) (format nil " · 💬 ~A" ncomments))))))))
          (:p.empty "No issues found.")))))

(defun view-milestones (&key owner-name repo milestones counts can-edit)
  "Render the milestones list with open/closed issue counts and a create form."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Milestones — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :issues :repo repo)
      (:div :style "display:flex;justify-content:space-between;align-items:center"
       (:h1 "Milestones")
       (:a.btn.btn-sm :href (format nil "/~A/~A/issues" owner-name repo-name) "← Issues"))
      (if milestones
          (:ul.data-list
           (dolist (m milestones)
             (let ((c (and counts (gethash (getf m :id) counts))))
               (:li
                (:strong (getf m :title))
                (:span.badge (getf m :state))
                (when c
                  (:span :style "color:var(--text-muted);font-size:.85rem;margin-left:.5rem"
                   (format nil "~A open / ~A closed" (car c) (cdr c))))
                (let ((d (getf m :description)))
                  (when (and d (not (eq d :null)) (plusp (length d)))
                    (:div :style "color:var(--text-muted);font-size:.85rem" d)))
                (when can-edit
                  (:div :style "display:inline-flex;gap:.35rem;margin-left:.5rem"
                   (when (equal (getf m :state) "open")
                     (:form :method "post" :style "display:inline"
                      :action (format nil "/~A/~A/milestones/~A/close" owner-name repo-name (getf m :id))
                      (:button.btn.btn-sm :type "submit" "Close")))
                   (:form :method "post" :style "display:inline"
                    :action (format nil "/~A/~A/milestones/~A/delete" owner-name repo-name (getf m :id))
                    (:button.btn.btn-sm :type "submit" "Delete"))))))))
          (:p.empty "No milestones yet."))
      (when can-edit
        (:section
         (:h2 "New milestone")
         (:form :method "post" :action (format nil "/~A/~A/milestones" owner-name repo-name)
          (:div.field
           (:label :for "title" "Title")
           (:input :type "text" :id "title" :name "title" :required t))
          (:div.field
           (:label :for "description" "Description (optional)")
           (:input :type "text" :id "description" :name "description"))
          (:button.btn.btn-primary :type "submit" "Create milestone")))))))

(defun %dep-severity-rank (sev)
  "Sort key: critical highest. SEV may be a string or :null."
  (cond ((or (null sev) (eq sev :null)) 0)
        ((string-equal sev "critical") 4)
        ((string-equal sev "high") 3)
        ((string-equal sev "moderate") 2)
        ((string-equal sev "low") 1)
        (t 0)))

(defun view-dependencies (&key owner-name repo alerts deps)
  "Render the repo Security tab: open alerts (severity-sorted) + the graph."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Security — ~A/~A" owner-name repo-name))
      (render-repo-tabs owner-name repo-name :security :repo repo)
      (:h2 (format nil "Security alerts (~A)" (length alerts)))
      (if alerts
          (:table.dep-alerts
           (:thead (:tr (:th "Severity") (:th "Package") (:th "Version")
                        (:th "Advisory") (:th "Fix")))
           (:tbody
            (dolist (a (sort (copy-list alerts) #'>
                             :key (lambda (a) (%dep-severity-rank (getf a :severity)))))
              (let ((sev (getf a :severity)) (fix (getf a :fix-version)))
                (:tr
                 (:td (:span.badge (if (or (null sev) (eq sev :null))
                                       "unknown" (string-downcase sev))))
                 (:td (format nil "~A (~A)" (getf a :package-name) (getf a :ecosystem)))
                 (:td (:code (getf a :version)))
                 (:td (:a :href (advisory-url (getf a :osv-id))
                          :target "_blank" :rel "noopener noreferrer"
                          (getf a :osv-id)))
                 (:td (if (or (null fix) (eq fix :null)) "—" (:code fix))))))))
          (:p.empty "✓ No open security alerts."))
      (:h2 (format nil "Dependencies (~A)" (length deps)))
      (if deps
          (:table.dep-list
           (:thead (:tr (:th "Ecosystem") (:th "Package") (:th "Version") (:th "Scope")))
           (:tbody
            (dolist (d deps)
              (:tr
               (:td (getf d :ecosystem))
               (:td (getf d :package-name))
               (:td (:code (getf d :version)))
               (:td (if (getf d :is-direct) "direct" "transitive"))))))
          (:p.empty "No dependencies recorded. Push to the default branch to scan.")))))

(defun view-new-issue (&key owner-name repo body)
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
        (:textarea :id "body" :name "body" :rows "12" (when body (princ body))))
       (:button.btn.btn-primary :type "submit" "Create issue")))))

(defun render-reactions (reactions owner-name repo-name issue-num &key comment-id)
  "Render a row of toggle reaction buttons (the 8 standard emoji), highlighting
the viewer's own and showing counts. Logged-in only; posts to the react route."
  (when *current-user*
    (let ((by-emoji (let ((h (make-hash-table :test 'equal)))
                      (dolist (r reactions) (setf (gethash (getf r :emoji) h) r))
                      h)))
      (spinneret:with-html
        (:form :method "post"
         :style "display:flex;gap:.25rem;flex-wrap:wrap;margin-top:.4rem"
         :action (format nil "/~A/~A/issues/~A/react" owner-name repo-name issue-num)
         (when comment-id
           (:input :type "hidden" :name "comment_id" :value (princ-to-string comment-id)))
         (dolist (e '("👍" "👎" "😄" "🎉" "❤️" "🚀" "😕" "👀"))
           (let* ((r (gethash e by-emoji))
                  (n (and r (getf r :count)))
                  (mine (and r (getf r :mine))))
             (:button.btn.btn-sm :type "submit" :name "emoji" :value e
              :style (format nil "padding:.05rem .4rem;font-size:.85rem~@[;border-color:var(--primary,#7c9a5e);color:var(--primary,#7c9a5e)~]" mine)
              (format nil "~A~@[ ~A~]" e (and n (plusp n) n))))))))))

(defun view-issue (&key owner-name repo issue author comments
                        labels assignees milestone milestones can-edit
                        reactions comment-reactions pinned)
  "Render an issue detail page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name))
        (issue-num (getf issue :number)))
    (page (:title (format nil "#~A ~A — Cave" issue-num (getf issue :title)))
      (render-repo-tabs org-name repo-name :issues :repo repo)
      (:div.issue-header
       (:h1 (format nil "~@[📌 ~]#~A ~A" pinned issue-num (getf issue :title)))
       (:span.badge (getf issue :status))
       (when can-edit
         (:form :method "post" :style "display:inline;margin-left:.5rem"
          :action (format nil "/~A/~A/issues/~A/pin" org-name repo-name issue-num)
          (:button.btn.btn-sm :type "submit" (if pinned "Unpin" "Pin")))))
      (:div.issue-meta
       (render-avatar (getf author :email) :size 16)
       (format nil " Opened by ~A" (getf author :username)))
      ;; Labels / assignees / milestone summary
      (when (or labels assignees (and milestone (not (eq milestone :null))))
        (:div.issue-attrs :style "display:flex;gap:1rem;flex-wrap:wrap;margin:.5rem 0;font-size:.85rem"
         (when labels
           (:div "Labels: "
            (dolist (l labels)
              (:span.badge :style "margin-left:.25rem" l))))
         (when assignees
           (:div (format nil "Assignees: ~{~A~^, ~}"
                         (mapcar (lambda (a) (getf a :username)) assignees))))
         (when (and milestone (not (eq milestone :null)))
           (:div "Milestone: " (:strong (getf milestone :title))))))
      ;; Member edit form for labels / assignees / milestone
      (when can-edit
        (:details :style "margin:.5rem 0"
         (:summary :style "cursor:pointer;font-size:.85rem;color:var(--text-muted)"
          "Edit labels / assignees / milestone")
         (:form :method "post" :style "margin-top:.5rem"
          :action (format nil "/~A/~A/issues/~A/meta" org-name repo-name issue-num)
          (:div.field
           (:label :for "labels" "Labels (comma-separated)")
           (:input :type "text" :id "labels" :name "labels"
                   :value (format nil "~{~A~^, ~}" labels)))
          (:div.field
           (:label :for "assignees" "Assignees (comma-separated usernames)")
           (:input :type "text" :id "assignees" :name "assignees"
                   :value (format nil "~{~A~^, ~}"
                                  (mapcar (lambda (a) (getf a :username)) assignees))))
          (:div.field
           (:label :for "milestone_id" "Milestone")
           (:select :id "milestone_id" :name "milestone_id"
            (:option :value "" "— none —")
            (dolist (m milestones)
              (:option :value (princ-to-string (getf m :id))
               :selected (and milestone (not (eq milestone :null))
                              (eql (getf m :id) (getf milestone :id)))
               (getf m :title)))))
          (:button.btn.btn-sm :type "submit" "Save"))))
      (let ((ib (getf issue :body)))
        (when (and ib (not (eq ib :null)))
          (:div.issue-body (:raw (render-markdown ib)))))
      (render-reactions reactions org-name repo-name issue-num)

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
              (:div.comment-body (:raw (render-markdown (getf c :body))))
              (render-reactions (and comment-reactions
                                     (gethash (getf c :id) comment-reactions))
                                org-name repo-name issue-num
                                :comment-id (getf c :id))))
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
          (:ul.issues
           (dolist (cs pulls)
             (let* ((num (getf cs :number))
                    (merged (getf cs :is-merged))
                    (closed (getf cs :is-closed))
                    (state (cond (merged "merged") (closed "closed") (t "open")))
                    (author (let ((u (ignore-errors (find-user-by-id (getf cs :author-id)))))
                              (and u (getf u :username))))
                    (ago (format-relative-time (getf cs :created-at)))
                    (ver (getf cs :version)))
               (:li.issue-row
                (:span.issue-icon
                 :title state
                 :style (format nil "background:~A"
                                (cond (merged "#a371f7") (closed "#c25450") (t "#3fb950"))))
                (:div.issue-main
                 (:div.issue-titleline
                  (:a.issue-title
                   :href (format nil "/~A/~A/pulls/~A" org-name repo-name num)
                   (format nil "~A → ~A" (getf cs :source-branch) (getf cs :target-branch)))
                  (:span.badge state))
                 (:div.issue-meta
                  (format nil "#~A" num)
                  (when author (format nil " · opened by ~A" author))
                  (when ago (format nil " · ~A" ago))
                  (when (and (numberp ver) (> ver 1)) (format nil " · v~A" ver))))))))
          (:p.empty "No pull requests found.")))))

(defun view-new-pull-request (&key owner-name repo branches default-branch)
  "Render the new pull request form."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "New pull request — ~A/~A" owner-name repo-name))
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

(defparameter +checks-panel-css+
  ".checks-rollup{display:flex;align-items:center;gap:.5rem;margin:.25rem 0 .5rem;font-weight:600}
.checks-list{list-style:none;padding:0;margin:0}
.check{display:flex;align-items:center;gap:.4rem;padding:.3rem 0;border-top:1px solid var(--border,#333)}
.check-name{font-family:var(--font-mono,monospace)}
.check-desc{color:var(--text-muted,#888);font-size:.85rem}
.check-icon{display:inline-block;width:1em;height:1em;flex:0 0 1em;text-align:center;box-sizing:border-box}
.check-success .check-icon::before{content:'\\2713';color:var(--green,#7c9a5e)}
.check-failure .check-icon::before{content:'\\2717';color:var(--red,#b04a4a)}
.check-pending .check-icon,.check-running .check-icon{border:2px solid var(--yellow,#c9a03e);border-top-color:transparent;border-radius:50%;width:.85em;height:.85em;animation:cave-spin .8s linear infinite}
.checks-dot{display:inline-block;width:.7em;height:.7em;border-radius:50%}
.checks-success{background:var(--green,#7c9a5e)}
.checks-failure{background:var(--red,#b04a4a)}
.checks-pending{background:var(--yellow,#c9a03e)}
@keyframes cave-spin{to{transform:rotate(360deg)}}"
  "Styles for the live PR checks panel (icons, spinners, rollup dot).")

(defparameter +checks-panel-js+
  "(function(){
  var panel=document.getElementById('checks-panel');
  if(!panel)return;
  var url=panel.getAttribute('data-url');
  function render(data){
    var roll=document.getElementById('checks-rollup');roll.textContent='';
    if(!data.rollup||data.rollup.total===0){
      var s=document.createElement('span');s.className='checks-summary';
      s.textContent='No checks have reported yet.';roll.appendChild(s);
    }else{
      var dot=document.createElement('span');dot.className='checks-dot checks-'+data.rollup.overall;roll.appendChild(dot);
      var parts=[];
      if(data.rollup.failure>0)parts.push(data.rollup.failure+' failing');
      if(data.rollup.pending>0)parts.push(data.rollup.pending+' in progress');
      if(data.rollup.success>0)parts.push(data.rollup.success+' successful');
      var sm=document.createElement('span');sm.className='checks-summary';sm.textContent=parts.join(', ');roll.appendChild(sm);
    }
    var ul=document.getElementById('checks-rows');ul.textContent='';
    (data.checks||[]).forEach(function(c){
      var li=document.createElement('li');li.className='check check-'+c.state;
      var ic=document.createElement('span');ic.className='check-icon';li.appendChild(ic);
      var nm=document.createElement('span');nm.className='check-name';nm.textContent=' '+c.name;li.appendChild(nm);
      var ds=document.createElement('span');ds.className='check-desc';ds.textContent=' \\u2014 '+(c.description||c.state);li.appendChild(ds);
      if(c.url){li.appendChild(document.createTextNode(' '));var a=document.createElement('a');a.href=c.url;a.style.marginLeft='.5rem';a.style.fontSize='.8rem';a.textContent='details';li.appendChild(a);}
      ul.appendChild(li);
    });
  }
  var wasPending=false;
  function poll(){
    fetch(url,{headers:{'Accept':'application/json'}}).then(function(r){return r.json();}).then(function(data){
      render(data);
      if(data.rollup&&data.rollup.overall==='pending'){wasPending=true;setTimeout(poll,3000);}
      else if(wasPending){setTimeout(function(){location.reload();},800);}
    }).catch(function(){setTimeout(poll,5000);});
  }
  if(panel.querySelector('.check-pending,.check-running')){wasPending=true;poll();}
})();"
  "Polls the checks JSON endpoint while anything is in progress, live-updating
rows + rollup, and reloads once everything settles so the merge box refreshes.")

(defun render-checks-rollup (rollup)
  "Render the rollup summary line (status dot + 'N failing, M in progress, …')."
  (spinneret:with-html
    (if (zerop (getf rollup :total))
        (:span.checks-summary "No checks have reported yet.")
        (progn
          (:span :class (format nil "checks-dot checks-~A" (getf rollup :overall)))
          (:span.checks-summary
           (let ((parts nil)
                 (fail (getf rollup :failure))
                 (pend (getf rollup :pending))
                 (succ (getf rollup :success)))
             (when (plusp fail) (push (format nil "~A failing" fail) parts))
             (when (plusp pend) (push (format nil "~A in progress" pend) parts))
             (when (plusp succ) (push (format nil "~A successful" succ) parts))
             (format nil "~{~A~^, ~}" (nreverse parts))))))))

(defun render-check-row (c)
  "Render one check row; icon is driven by the check-STATE class (CSS)."
  (spinneret:with-html
    (:li :class (format nil "check check-~A" (getf c :state))
     (:span.check-icon)
     (:span.check-name (format nil " ~A" (getf c :name)))
     (:span.check-desc (format nil " — ~A" (or (getf c :description) (getf c :state))))
     (let ((u (getf c :url)))
       (when u (:a :href u :style "margin-left:.5rem;font-size:.8rem" "details"))))))

(defun render-checks-panel (owner-name repo-name pr-num checks rollup)
  "Render the live CI checks panel: rollup summary + per-check rows with spinners
for in-progress checks, polling a JSON endpoint while anything runs."
  (spinneret:with-html
    (:section
     (:style (:raw +checks-panel-css+))
     (:h2 "Checks")
     (:div#checks-panel
      :data-url (format nil "/~A/~A/pulls/~A/checks.json" owner-name repo-name pr-num)
      (:div#checks-rollup.checks-rollup (render-checks-rollup rollup))
      (:ul#checks-rows.checks-list (dolist (c checks) (render-check-row c))))
     (:script (:raw +checks-panel-js+)))))

(defun view-interdiff (&key owner-name repo pr from-version to-version text)
  "Render the interdiff (range-diff) between two PR rounds."
  (let ((repo-name (getf repo :name)))
    (page (:title (format nil "Interdiff #~A — Cave" (getf pr :number)))
      (render-repo-tabs owner-name repo-name :pulls :repo repo)
      (:h1 (format nil "PR #~A — interdiff: round ~A → round ~A"
                   (getf pr :number) from-version to-version))
      (:p (:a :href (format nil "/~A/~A/pulls/~A" owner-name repo-name (getf pr :number))
           "← back to pull request"))
      (if (and text (plusp (length text)))
          (:pre :style "background:var(--surface);padding:1rem;border-radius:var(--radius);border:1px solid var(--border);overflow-x:auto;font-size:.85rem;white-space:pre"
           text)
          (:p.empty "No differences between these rounds (or the commits are unavailable).")))))

(defun view-pull-request (&key owner-name repo pr author reviews eligibility
                             can-merge can-override conflict-files stack stack-items diff-raw
                             diff-comments-json comment-action
                             checks checks-rollup source-missing can-close code-owners
                             versions)
  "Render a pull request detail page."
  (let ((org-name owner-name)
        (repo-name (getf repo :name))
        (cs-num (getf pr :number)))
    (page (:title (format nil "#~A ~A — Cave" cs-num (getf pr :source-branch)))
      (render-repo-tabs org-name repo-name :pulls :repo repo)

      (:div.issue-header
       (:h1 (format nil "#~A ~A" cs-num (getf pr :source-branch)))
       (when (getf pr :is-draft)
         (:span.badge :style "background:var(--surface);color:var(--text-muted)" "draft"))
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

      ;; Code owners for the changed files (from CODEOWNERS).
      (when code-owners
        (:div :style "color:var(--text-muted);font-size:.85rem;margin:.25rem 0"
         (format nil "Code owners: ~{~A~^ ~}" code-owners)))

      ;; State controls (author or repo member); merged PRs are frozen.
      (when (and can-close (not (getf pr :is-merged)))
        (let ((open-p (not (getf pr :is-closed)))
              (am (getf pr :auto-merge-strategy)))
          (:div :style "display:flex;gap:.5rem;flex-wrap:wrap;margin:.5rem 0;align-items:center"
           ;; Close / reopen
           (:form :method "post" :style "display:inline"
            :action (format nil "/~A/~A/pulls/~A/state" org-name repo-name cs-num)
            (if open-p
                (:button.btn.btn-sm :type "submit" :name "action" :value "close"
                 :style "border-color:var(--red,#b04a4a);color:var(--red,#b04a4a)"
                 "Close pull request")
                (:button.btn.btn-sm :type "submit" :name "action" :value "reopen"
                 "Reopen pull request")))
           ;; Draft / ready toggle (open PRs only)
           (when open-p
             (:form :method "post" :style "display:inline"
              :action (format nil "/~A/~A/pulls/~A/state" org-name repo-name cs-num)
              (if (getf pr :is-draft)
                  (:button.btn.btn-sm :type "submit" :name "action" :value "ready"
                   "Mark ready for review")
                  (:button.btn.btn-sm :type "submit" :name "action" :value "draft"
                   "Convert to draft"))))
           ;; Auto-merge arm/disarm (open, non-draft PRs)
           (when (and open-p (not (getf pr :is-draft)))
             (if (and am (not (eq am :null)))
                 (:form :method "post" :style "display:inline"
                  :action (format nil "/~A/~A/pulls/~A/state" org-name repo-name cs-num)
                  (:span :style "color:var(--text-muted);font-size:.85rem;margin-right:.35rem"
                   (format nil "Auto-merge armed (~A)" am))
                  (:button.btn.btn-sm :type "submit" :name "action" :value "disable-auto-merge"
                   "Cancel auto-merge"))
                 (:form :method "post" :style "display:inline"
                  :action (format nil "/~A/~A/pulls/~A/state" org-name repo-name cs-num)
                  (:input :type "hidden" :name "action" :value "auto-merge")
                  (:button.btn.btn-sm :type "submit" :name "strategy" :value "merge"
                   "Enable auto-merge")))))))

      ;; Source branch gone (e.g. pruned by a mirror sync): the diff can't be
      ;; computed and the PR can't be merged. Say so instead of "0 files".
      (when (and source-missing (not (getf pr :is-merged)) (not (getf pr :is-closed)))
        (:div.flash-error
         :style "margin:var(--sp-3) 0;padding:.6rem .8rem;border:1px solid var(--red,#b04a4a);border-radius:var(--radius);background:var(--red-bg,rgba(176,74,74,.1))"
         (format nil "Source branch ~A no longer exists — its commits were removed (most likely pruned by a mirror sync). This PR has nothing to merge and should be closed."
                 (getf pr :source-branch))))

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

      ;; Rounds: each push is a numbered, immutable round; reviews anchor to a
      ;; round and consecutive rounds can be interdiffed (git range-diff).
      (when (and versions (> (length versions) 1))
        (:section
         (:h2 :style "font-size:1rem" "Rounds")
         (:ul.data-list
          (loop for (v . rest) on versions
                do (:li
                    (:strong (format nil "Round ~A" (getf v :version)))
                    (:code :style "margin-left:.4rem"
                     (let ((h (getf v :head-commit)))
                       (if (and h (>= (length h) 8)) (subseq h 0 8) h)))
                    (let ((rel (format-relative-time (getf v :created-at))))
                      (when rel
                        (:span :style "color:var(--text-muted);font-size:.8rem;margin-left:.4rem" rel)))
                    ;; Interdiff vs the previous round.
                    (when rest
                      (:a :style "margin-left:.5rem;font-size:.85rem"
                       :href (format nil "/~A/~A/pulls/~A/interdiff?from=~A&to=~A"
                                     org-name repo-name cs-num
                                     (getf (first rest) :version) (getf v :version))
                       (format nil "interdiff vs round ~A" (getf (first rest) :version)))))))))

      ;; Live CI checks panel (commit statuses + cave workflow jobs)
      (when (and checks-rollup (plusp (getf checks-rollup :total)))
        (render-checks-panel org-name repo-name cs-num checks checks-rollup))

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
              "Squash and merge")
             (:button.btn :type "submit" :name "strategy" :value "fast-forward-only"
              "Fast-forward only"))))
         ;; Conflicts: list the conflicting files and how to resolve them. A
         ;; "merge anyway" override can't succeed on a real conflict, so that is
         ;; suppressed below; instead admins get a "mark as manually merged"
         ;; escape hatch for when they resolve + push out of band.
         (when conflict-files
           (let ((source (getf pr :source-branch))
                 (target (getf pr :target-branch))
                 (ssh-url (ssh-clone-url org-name repo-name)))
             (:div.merge-conflicts
              (:h3 (format nil "Conflicts with ~A" target))
              (:p "These files conflict and must be resolved before this pull request can be merged:")
              (:ul (dolist (f conflict-files) (:li (:code f))))
              (:p "Resolve locally, then push the updated branch:")
              (:pre
               (format nil "git fetch ~A~%git checkout ~A~%git merge origin/~A~%# fix the conflicting files above, then:~%git add -A && git commit~%git push origin ~A"
                       ssh-url target source target))
              (when can-override
                (:details.merge-manual
                 (:summary "Mark as manually merged")
                 (:form :method "post"
                  :action (format nil "/~A/~A/pulls/~A/merge" org-name repo-name cs-num)
                  (:p :style "margin:var(--sp-2) 0;color:var(--fg-muted)"
                   (format nil "If you already merged ~A into ~A another way, paste the resulting commit on ~A to close this out:"
                           source target target))
                  (:input :type "text" :name "manual_merge_commit"
                   :placeholder "commit sha on target"
                   :style "width:100%;margin-bottom:var(--sp-2)")
                  (:button.btn :type "submit" "Mark as manually merged")))))))
         ;; Admin escape hatch for OTHER unmet requirements (failing checks,
         ;; missing approvals). Suppressed for conflicts, where merge-anyway
         ;; can't win — those get the resolution UI above instead.
         (when (and can-override (not conflict-files))
           (:details.merge-override
            (:summary "⚠ Admin override — merge anyway")
            (:form :method "post"
             :action (format nil "/~A/~A/pulls/~A/merge" org-name repo-name cs-num)
             (:input :type "hidden" :name "override" :value "t")
             (:p :style "margin:var(--sp-2) 0;color:var(--fg-muted)"
              "This pull request does not meet all merge requirements. As a repo admin you can override the checks and merge anyway.")
             (:div :style "display:flex;gap:var(--sp-2)"
              (:button.btn.btn-primary :type "submit" :name "strategy" :value "merge"
               "Merge anyway")
              (:button.btn :type "submit" :name "strategy" :value "squash"
               "Squash and merge")
              (:button.btn :type "submit" :name "strategy" :value "fast-forward-only"
               "Fast-forward only")))))))

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
              (let ((rb (getf r :body)))
                (when (and rb (not (eq rb :null)))
                  (:div.review-body rb)))
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

