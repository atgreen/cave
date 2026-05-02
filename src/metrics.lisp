;;; metrics.lisp — Prometheus metrics for Cave
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; Thread-safe counters and histograms for HTTP request instrumentation.
;;; Emits Prometheus text exposition format at /metrics.

(defvar *metrics-lock* (bt:make-lock "metrics"))

;; Request counters: hash of "method:status" -> count
(defvar *request-counts* (make-hash-table :test 'equal))

;; Request duration buckets (seconds)
(defparameter *duration-buckets* '(0.005 0.01 0.025 0.05 0.1 0.25 0.5 1.0 2.5 5.0 10.0))

;; Duration tracking: hash of method -> (list of durations for histogram)
;; We store bucket counts + sum + count, not raw durations
(defvar *request-duration-sum* (make-hash-table :test 'equal))
(defvar *request-duration-count* (make-hash-table :test 'equal))
(defvar *request-duration-buckets* (make-hash-table :test 'equal))

;; Application-level gauges
(defvar *active-requests* 0)

(defun reset-metrics ()
  "Reset all metrics (for testing)."
  (bt:with-lock-held (*metrics-lock*)
    (clrhash *request-counts*)
    (clrhash *request-duration-sum*)
    (clrhash *request-duration-count*)
    (clrhash *request-duration-buckets*)
    (setf *active-requests* 0)))

(defun record-request (method status duration)
  "Record a completed HTTP request."
  (let ((count-key (format nil "~A:~A" method status))
        (dur-key (string method)))
    (bt:with-lock-held (*metrics-lock*)
      ;; Increment request counter
      (incf (gethash count-key *request-counts* 0))
      ;; Update duration histogram
      (incf (gethash dur-key *request-duration-count* 0))
      (incf (gethash dur-key *request-duration-sum* 0.0d0) duration)
      ;; Update bucket counts
      (let ((buckets (or (gethash dur-key *request-duration-buckets*)
                         (setf (gethash dur-key *request-duration-buckets*)
                               (make-array (1+ (length *duration-buckets*))
                                           :initial-element 0)))))
        ;; Increment all buckets where duration <= threshold (cumulative)
        (loop for threshold in *duration-buckets*
              for i from 0
              when (<= duration threshold)
                do (incf (aref buckets i)))
        ;; +Inf bucket always gets incremented
        (incf (aref buckets (length *duration-buckets*)))))))

(defun format-prometheus-metric (stream name help type lines)
  "Write a metric block in Prometheus text format."
  (format stream "# HELP ~A ~A~%" name help)
  (format stream "# TYPE ~A ~A~%" name type)
  (dolist (line lines)
    (write-string line stream)
    (terpri stream))
  (terpri stream))

(defun collect-metrics ()
  "Collect all metrics and return Prometheus text format string."
  (with-output-to-string (s)
    (bt:with-lock-held (*metrics-lock*)
      ;; -- Request count --
      (let ((lines nil))
        (maphash (lambda (key count)
                   (destructuring-bind (method status)
                       (uiop:split-string key :separator ":")
                     (push (format nil "cave_http_requests_total{method=~S,status=~S} ~A"
                                   method status count)
                           lines)))
                 *request-counts*)
        (when lines
          (format-prometheus-metric s "cave_http_requests_total"
                                    "Total HTTP requests" "counter" (nreverse lines))))

      ;; -- Request duration histogram --
      (maphash (lambda (method count)
                 (declare (ignore count))
                 (let ((lines nil)
                       (buckets (gethash method *request-duration-buckets*))
                       (sum (gethash method *request-duration-sum* 0.0d0))
                       (cnt (gethash method *request-duration-count* 0)))
                   (when buckets
                     (loop for threshold in *duration-buckets*
                           for i from 0
                           do (push (format nil "cave_http_request_duration_seconds_bucket{method=~S,le=~S} ~A"
                                            method (format nil "~F" threshold) (aref buckets i))
                                    lines))
                     (push (format nil "cave_http_request_duration_seconds_bucket{method=~S,le=\"+Inf\"} ~A"
                                   method (aref buckets (length *duration-buckets*)))
                           lines)
                     (push (format nil "cave_http_request_duration_seconds_sum{method=~S} ~F"
                                   method sum)
                           lines)
                     (push (format nil "cave_http_request_duration_seconds_count{method=~S} ~A"
                                   method cnt)
                           lines))
                   (when lines
                     (format-prometheus-metric s "cave_http_request_duration_seconds"
                                               "HTTP request duration in seconds"
                                               "histogram" (nreverse lines)))))
               *request-duration-count*))

    ;; -- Active requests gauge --
    (format-prometheus-metric s "cave_active_requests"
                              "Currently in-flight HTTP requests" "gauge"
                              (list (format nil "cave_active_requests ~A" *active-requests*)))

    ;; -- Application gauges (query DB) --
    (handler-case
        (postmodern:with-connection *db-spec*
          (let ((user-count (postmodern:query
                             (:select (:count '*) :from 'cave-users
                              :where (:= 'is-active t))
                             :single))
                (repo-count (postmodern:query
                             (:select (:count '*) :from 'cave-repos)
                             :single))
                (org-count (postmodern:query
                            (:select (:count '*) :from 'cave-orgs)
                            :single))
                (open-issues (postmodern:query
                              (:select (:count '*) :from 'cave-issues
                               :where (:= 'status "open"))
                              :single))
                (open-changesets (postmodern:query
                                  (:select (:count '*) :from 'cave-changesets
                                   :where (:and (:= 'is-merged nil)
                                                (:= 'is-closed nil)))
                                  :single))
                (active-sessions (postmodern:query
                                   (:select (:count '*) :from 'cave-sessions
                                    :where (:> 'expires-at (:now)))
                                   :single)))
            (format-prometheus-metric s "cave_users_total"
                                      "Total active users" "gauge"
                                      (list (format nil "cave_users_total ~A" user-count)))
            (format-prometheus-metric s "cave_repos_total"
                                      "Total repositories" "gauge"
                                      (list (format nil "cave_repos_total ~A" repo-count)))
            (format-prometheus-metric s "cave_orgs_total"
                                      "Total organizations" "gauge"
                                      (list (format nil "cave_orgs_total ~A" org-count)))
            (format-prometheus-metric s "cave_issues_open"
                                      "Open issues" "gauge"
                                      (list (format nil "cave_issues_open ~A" open-issues)))
            (format-prometheus-metric s "cave_changesets_open"
                                      "Open changesets" "gauge"
                                      (list (format nil "cave_changesets_open ~A" open-changesets)))
            (format-prometheus-metric s "cave_sessions_active"
                                      "Active sessions" "gauge"
                                      (list (format nil "cave_sessions_active ~A" active-sessions)))))
      (error (e)
        (declare (ignore e))
        ;; If DB is unavailable, skip app metrics
        nil))

    ;; -- SBCL runtime metrics --
    (format-prometheus-metric s "sbcl_gc_run_time_seconds_total"
                              "Cumulative time spent in GC" "counter"
                              (list (format nil "sbcl_gc_run_time_seconds_total ~F"
                                           (/ sb-ext:*gc-run-time*
                                              (float internal-time-units-per-second 1.0d0)))))
    (format-prometheus-metric s "sbcl_bytes_consed_total"
                              "Total bytes allocated since startup" "counter"
                              (list (format nil "sbcl_bytes_consed_total ~A"
                                           (sb-ext:get-bytes-consed))))
    (format-prometheus-metric s "sbcl_dynamic_space_size_bytes"
                              "Maximum dynamic space (heap) size" "gauge"
                              (list (format nil "sbcl_dynamic_space_size_bytes ~A"
                                           (sb-ext:dynamic-space-size))))
    (format-prometheus-metric s "sbcl_dynamic_usage_bytes"
                              "Current dynamic space (heap) usage" "gauge"
                              (list (format nil "sbcl_dynamic_usage_bytes ~A"
                                           (sb-kernel:dynamic-usage))))
    (format-prometheus-metric s "sbcl_thread_count"
                              "Number of active SBCL threads" "gauge"
                              (list (format nil "sbcl_thread_count ~A"
                                           (length (sb-thread:list-all-threads)))))))
