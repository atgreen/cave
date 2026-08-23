(in-package #:cave)

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

(defun view-workflow-run (&key owner-name repo run jobs artifacts)
  "Render a workflow run detail page with jobs, steps, and artifacts."
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
         (princ-to-string (getf run :created-at)))
        ;; Re-run a finished run — recovers from a zombie/failed build.
        (when (and *current-user-id*
                   (repo-member-role (getf repo :id) *current-user-id*)
                   (member (getf run :status) '("success" "failure" "cancelled")
                           :test #'equal))
          (:form :method "post" :style "margin-left:auto"
           :action (format nil "/~A/~A/runs/w/~A/rerun"
                           owner-name repo-name (getf run :id))
           (:button.btn.btn-sm :type "submit" "Re-run")))))

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

      ;; Artifacts
      (when artifacts
        (:section :style "border:1px solid var(--border);border-radius:var(--radius);padding:var(--sp-3);margin-bottom:var(--sp-3)"
         (:h3 :style "margin:0 0 var(--sp-2) 0" "Artifacts")
         (:div :style "display:flex;flex-direction:column;gap:2px"
          (dolist (a artifacts)
            (:a :href (format nil "/~A/~A/runs/w/~A/artifacts/~A"
                              owner-name repo-name (getf run :id) (getf a :id))
             :style "display:flex;gap:var(--sp-2);align-items:center;padding:var(--sp-1) var(--sp-2);font-size:.85rem"
             (:span "📦")
             (:span (getf a :name))
             (:span :style "margin-left:auto;color:var(--text-muted);font-size:.75rem"
              (format-bytes (getf a :size-bytes))))))))

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
        "Run: " (:code (format nil "cave-server runner --url ~A --token ~A"
                               (runner-grpc-url)
                               (getf registration-token :token))))))
    (:form :method "post" :action token-action
     (:button.btn.btn-primary :type "submit" "Generate registration token"))))

