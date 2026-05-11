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
   Uses -repo_cache to derive owner/repo naming from the directory structure."
  (when (config-value :zoekt-enabled)
    (let ((repo-path (namestring (repo-disk-path owner repo-name)))
          (index-dir (config-value :zoekt-index-dir "/data/zoekt-index"))
          (repos-base (namestring (repos-dir)))
          (index-name (format nil "~A/~A" owner repo-name)))
      (bt:make-thread
       (lambda ()
         (handler-case
             (multiple-value-bind (_out err exit)
                 (uiop:run-program
                  (list "zoekt-git-index"
                        "-index" index-dir
                        "-branches" "HEAD"
                        "-repo_cache" repos-base
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
  "Search via the Zoekt webserver JSON API (GET /search?format=json).
   Returns a plist with :files (list of file-match plists) and :stats.
   REPO-SCOPE when non-nil restricts search to \"owner/repo-name\"."
  (unless (config-value :zoekt-enabled)
    (return-from zoekt-search (list :files nil :error "Search not configured")))
  (let* ((effective-query (if repo-scope
                              (format nil "r:~A ~A" repo-scope query)
                              query))
         (base-url (config-value :zoekt-web-url "http://cave-prod-zoekt-web:6070"))
         (url (format nil "~A/search?q=~A&format=json&num=~A&ctx=1"
                      base-url
                      (hunchentoot:url-encode effective-query)
                      limit)))
    (handler-case
        (multiple-value-bind (body status)
            (dex:get url :connect-timeout 5 :read-timeout 10)
          (if (= status 200)
              (parse-zoekt-response body)
              (list :files nil :error (format nil "Search returned HTTP ~A" status))))
      (error (e)
        (llog:warn "Zoekt search failed" :error (princ-to-string e))
        (list :files nil :error "Search temporarily unavailable")))))

(defun parse-zoekt-response (json-string)
  "Parse Zoekt JSON search response into a plist structure.
   The response is wrapped in a 'result' key with FileMatches array.
   Each FileMatch has Matches with Before/After context and Fragments."
  (let* ((data (com.inuoe.jzon:parse json-string))
         (result-obj (gethash "result" data))
         (file-matches (when result-obj (gethash "FileMatches" result-obj)))
         (result nil))
    (when file-matches
      (loop for fm across file-matches
            do (let* ((repo-name (gethash "Repo" fm))
                      (file-name (gethash "FileName" fm))
                      (language (gethash "Language" fm))
                      (matches (gethash "Matches" fm)))
                 (when repo-name
                   (push (list :repository repo-name
                               :file-name file-name
                               :language (or language "")
                               :matches (parse-zoekt-matches matches))
                         result)))))
    (list :files (nreverse result)
          :stats (list :file-count (length (or file-matches #()))))))

(defun parse-zoekt-matches (matches)
  "Parse Zoekt Match array into plists with context.
   Each match has LineNum, Before (context), After (context),
   and Fragments with Pre/Match/Post for the matched line."
  (when matches
    (loop for m across matches
          collect (let ((line-num (gethash "LineNum" m))
                        (before (gethash "Before" m))
                        (after (gethash "After" m))
                        (fragments (gethash "Fragments" m)))
                   (list :line-num (or line-num 0)
                         :before-lines (when (and before (not (string= before "")))
                                         (remove "" (uiop:split-string before :separator '(#\Newline))
                                                 :test #'string=))
                         :after-lines (when (and after (not (string= after "")))
                                        (remove "" (uiop:split-string after :separator '(#\Newline))
                                                :test #'string=))
                         :fragments (when fragments
                                      (loop for f across fragments
                                            collect (list :pre (or (gethash "Pre" f) "")
                                                         :match (or (gethash "Match" f) "")
                                                         :post (or (gethash "Post" f) "")))))))))

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
