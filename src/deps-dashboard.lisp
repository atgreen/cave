;;; deps-dashboard.lisp — the per-repo dependency dashboard issue.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Maintains one pinned "Dependency Dashboard" issue per repo (authored by the
;;; cave-bot user), listing open security alerts grouped by severity. Reuses the
;;; existing issue + markdown machinery — no new UI. Refreshed after a scan
;;; (sbom.lisp) and after an advisory sync (osv.lisp).
;;; See docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *dashboard-marker* "cave-dependency-dashboard"
  "Hidden marker in the issue body used to locate the dashboard idempotently.")

(defparameter *dashboard-title* "Dependency Dashboard")

(defparameter *dashboard-severities* '("critical" "high" "moderate" "low" "unknown")
  "Severity buckets, most severe first.")

(defun %severity-label (sev)
  "Normalize a stored severity (string or :null) to a bucket label."
  (if (or (null sev) (eq sev :null)) "unknown" (string-downcase sev)))

(defun render-dependency-dashboard-body (repo-id)
  "Markdown body for REPO-ID's dependency dashboard, grouped by severity."
  (let ((alerts (list-dep-alerts-detailed repo-id :state "open"))
        (groups (make-hash-table :test 'equal)))
    (dolist (a alerts)
      (push a (gethash (%severity-label (getf a :severity)) groups)))
    (with-output-to-string (out)
      (format out "<!-- ~A -->~%" *dashboard-marker*)
      (format out "_Maintained automatically by Cave. Dismiss false positives with_ `cave deps dismiss`.~%~%")
      (if (null alerts)
          (format out "✓ No open security alerts.~%")
          (progn
            (format out "## ~A open security alert~:P~%~%" (length alerts))
            (dolist (label *dashboard-severities*)
              (let ((rows (sort (copy-list (gethash label groups))
                                #'string< :key (lambda (a) (getf a :package-name)))))
                (when rows
                  (format out "### ~:(~A~) (~A)~%~%" label (length rows))
                  (dolist (a rows)
                    (let ((osv (getf a :osv-id))
                          (summary (let ((s (getf a :summary)))
                                     (unless (or (null s) (eq s :null)) s)))
                          (fix (let ((f (getf a :fix-version)))
                                 (unless (or (null f) (eq f :null)) f))))
                      (format out "- **~A** `~A` (~A) — [~A](~A)~@[: ~A~]~@[ — fix: `~A`~]~%"
                              (getf a :package-name) (getf a :version)
                              (getf a :ecosystem) osv (advisory-url osv) summary fix)))
                  (format out "~%")))))))))

(defun update-dependency-dashboard (repo-id)
  "Create or refresh REPO-ID's dependency-dashboard issue. Creates one only when
   there is something to report; refreshes an existing one regardless."
  (let ((existing (find-dashboard-issue repo-id *dashboard-marker*)))
    (cond
      (existing
       (update-issue (getf existing :id)
                     :body (render-dependency-dashboard-body repo-id)
                     :status "open"))
      ((list-dep-alerts-detailed repo-id :state "open")
       (create-issue :repo-id repo-id
                     :author-id (ensure-dependency-bot-user)
                     :title *dashboard-title*
                     :body (render-dependency-dashboard-body repo-id))))))

(defun refresh-dependency-dashboards ()
  "Refresh dashboards for every repo with open alerts or an existing dashboard."
  (dolist (rid (repos-needing-dashboard-refresh *dashboard-marker*))
    (handler-case (update-dependency-dashboard rid)
      (error (e)
        (llog:warn "Dashboard refresh failed" :repo-id rid
                                              :error (princ-to-string e))))))
