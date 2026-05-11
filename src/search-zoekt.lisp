;;; search-zoekt.lisp — Zoekt code search integration
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Provides:
;;; - Background indexing triggered by post-receive hooks
;;; - Search API client for the Zoekt webserver sidecar
;;; - Visibility filtering (private repos hidden from non-members)

(in-package #:cave)

;;; --- Indexing ---

(defun zoekt-index-repo (owner repo-name)
  "Trigger zoekt indexing for a repository in a background thread.
   Runs zoekt-git-index with -name owner/repo-name so the index uses
   the same naming convention as Cave URLs."
  (when (config-value :zoekt-enabled)
    (let ((repo-path (namestring (repo-disk-path owner repo-name)))
          (index-dir (config-value :zoekt-index-dir "/data/zoekt-index"))
          (index-name (format nil "~A/~A" owner repo-name)))
      (bt:make-thread
       (lambda ()
         (handler-case
             (multiple-value-bind (_out err exit)
                 (uiop:run-program
                  (list "zoekt-git-index"
                        "-index" index-dir
                        "-branches" "HEAD"
                        "-name" index-name
                        repo-path)
                  :output '(:string :stripped t)
                  :error-output '(:string :stripped t)
                  :ignore-error-status t)
               (declare (ignore _out))
               (if (zerop exit)
                   (llog:info "Zoekt indexed repo" :repo index-name)
                   (llog:warn "Zoekt indexing failed" :repo index-name :error err)))
           (error (e)
             (llog:warn "Zoekt indexing error" :repo index-name
                                               :error (princ-to-string e)))))
       :name (format nil "zoekt-index-~A/~A" owner repo-name)))))

;;; --- Search API Client ---

(defun zoekt-search (query &key (limit 50) repo-scope)
  "Search via the Zoekt webserver JSON API.
   Returns a plist with :files (list of file-match plists) and :stats.
   REPO-SCOPE when non-nil restricts search to \"owner/repo-name\"."
  (unless (config-value :zoekt-enabled)
    (return-from zoekt-search (list :files nil :error "Search not configured")))
  (let* ((effective-query (if repo-scope
                              (format nil "repo:~A ~A" repo-scope query)
                              query))
         (opts (make-hash-table :test 'equal))
         (payload (make-hash-table :test 'equal)))
    (setf (gethash "MaxMatchDisplayCount" opts) limit)
    (setf (gethash "NumContextLines" opts) 1)
    (setf (gethash "Q" payload) effective-query)
    (setf (gethash "Opts" payload) opts)
    (let ((url (format nil "~A/api/search"
                       (config-value :zoekt-web-url "http://cave-prod-zoekt-web:6070"))))
      (handler-case
          (multiple-value-bind (body status)
              (dex:post url
                        :content (com.inuoe.jzon:stringify payload)
                        :headers '(("Content-Type" . "application/json"))
                        :connect-timeout 5
                        :read-timeout 10)
            (if (= status 200)
                (parse-zoekt-response body)
                (list :files nil :error (format nil "Search returned HTTP ~A" status))))
        (error (e)
          (llog:warn "Zoekt search failed" :error (princ-to-string e))
          (list :files nil :error "Search temporarily unavailable"))))))

(defun parse-zoekt-response (json-string)
  "Parse Zoekt JSON search response into a plist structure."
  (let* ((data (com.inuoe.jzon:parse json-string))
         (file-matches (gethash "FileMatches" data))
         (result nil))
    (when file-matches
      (loop for fm across file-matches
            do (let* ((repo-obj (gethash "Repository" fm))
                      (repo-name (when repo-obj (gethash "Name" repo-obj)))
                      (file-name (gethash "FileName" fm))
                      (chunk-matches (gethash "ChunkMatches" fm)))
                 (when repo-name
                   (push (list :repository repo-name
                               :file-name file-name
                               :lines (parse-chunk-matches chunk-matches))
                         result)))))
    (list :files (nreverse result)
          :stats (list :file-count (length (or file-matches #()))))))

(defun parse-chunk-matches (chunks)
  "Parse Zoekt ChunkMatch array into line plists.
   Each ChunkMatch has Content (base64 bytes), ContentStart (line/col),
   and Ranges for match highlighting."
  (when chunks
    (let ((lines nil))
      (loop for chunk across chunks
            do (let* ((content-raw (gethash "Content" chunk))
                      (content (if (stringp content-raw)
                                   content-raw
                                   (cl-base64:base64-string-to-string
                                    (if (vectorp content-raw)
                                        (flexi-streams:octets-to-string content-raw)
                                        (princ-to-string content-raw)))))
                      (start-obj (gethash "ContentStart" chunk))
                      (start-line (when start-obj
                                    (gethash "LineNumber" start-obj)))
                      (ranges (gethash "Ranges" chunk))
                      (content-lines (uiop:split-string content :separator '(#\Newline))))
                 (loop for line-text in content-lines
                       for line-num from (or start-line 1)
                       unless (and (= line-num (+ (or start-line 1) (length content-lines) -1))
                                   (string= line-text ""))
                       do (push (list :line-number line-num
                                      :line line-text
                                      :fragments (collect-fragments-for-line
                                                  line-num (or start-line 1) ranges))
                                lines))))
      (nreverse lines))))

(defun collect-fragments-for-line (line-num start-line ranges)
  "Extract match fragments that overlap with LINE-NUM from Zoekt Ranges."
  (when ranges
    (let ((frags nil))
      (loop for range across ranges
            do (let* ((range-start (gethash "Start" range))
                      (range-end (gethash "End" range))
                      (start-ln (when range-start (gethash "LineNumber" range-start)))
                      (end-ln (when range-end (gethash "LineNumber" range-end)))
                      (start-col (when range-start (gethash "Column" range-start)))
                      (end-col (when range-end (gethash "Column" range-end))))
                 (when (and start-ln end-ln
                            (<= start-ln line-num)
                            (>= end-ln line-num))
                   (let ((offset (if (= start-ln line-num) (1- start-col) 0))
                         (len (cond
                                ((= start-ln end-ln) (- end-col start-col))
                                ((= line-num start-ln) 200) ; extends past line
                                (t 200))))
                     (push (list :offset (max 0 offset)
                                 :length (max 1 len))
                           frags)))))
      (nreverse frags))))

;;; --- Visibility Filter ---

(defun zoekt-search-visible (query &key (limit 50) repo-scope)
  "Search via Zoekt, filtering results to repos visible to the current user.
   Uses a cache to avoid repeated DB lookups for the same repo."
  (let ((results (zoekt-search query :limit limit :repo-scope repo-scope)))
    (when (getf results :error)
      (return-from zoekt-search-visible results))
    (let ((repo-cache (make-hash-table :test 'equal))
          (visible-files nil))
      (dolist (fm (getf results :files))
        (let* ((repo-path (getf fm :repository))
               (cached (gethash repo-path repo-cache :miss)))
          (when (eq cached :miss)
            ;; Lookup and cache
            (let* ((parts (uiop:split-string repo-path :separator '(#\/)))
                   (owner (first parts))
                   (name (second parts))
                   (repo (when (and owner name) (find-repo owner name))))
              (setf cached (and repo (repo-visible-p repo)))
              (setf (gethash repo-path repo-cache) cached)))
          (when cached
            (push fm visible-files))))
      (list :files (nreverse visible-files)
            :stats (list :file-count (length visible-files))))))
