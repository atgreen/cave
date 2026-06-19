;;; deps-fix.lisp — propose dependency-fix pull requests.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Given a security alert with a fix version, classify how it can be fixed and,
;;; for the unambiguous manifest case, open a real bump PR. Anything that needs
;;; package-manager/lockfile tooling (ranged versions, transitive deps) is
;;; classified and reported :manual rather than guessed at — we never corrupt a
;;; manifest. CI-gated auto-merge is a later layer.
;;; See docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *fix-branch-prefix* "cave/deps"
  "Branch namespace for dependency-fix PRs.")

(defun classify-fix-kind (alert)
  "Classify how ALERT (a find-dep-alert-detailed plist) could be fixed:
   'none' (no fix version), 'manifest' (direct dep), or 'transitive_parent'."
  (let ((fix (getf alert :fix-version))
        (direct (getf alert :is-direct)))
    (cond ((or (null fix) (eq fix :null)) "none")
          (direct "manifest")
          (t "transitive_parent"))))

(defun safe-bump-manifest (content old-version new-version)
  "Return CONTENT with OLD-VERSION replaced by NEW-VERSION, but only when
   OLD-VERSION occurs exactly once — avoiding ambiguous or corrupting edits.
   Returns NIL when the version is absent (ranged/lockfile-only) or ambiguous."
  (when (and (stringp content) (plusp (length old-version)))
    (let ((count 0) (pos nil) (start 0))
      (loop for p = (search old-version content :start2 start)
            while p
            do (incf count) (setf pos p start (+ p (length old-version))))
      (when (= count 1)
        (concatenate 'string
                     (subseq content 0 pos)
                     new-version
                     (subseq content (+ pos (length old-version))))))))

(defun %sanitize-branch-component (s)
  "Make S safe for a git ref component."
  (substitute-if #\- (lambda (c) (not (or (alphanumericp c)
                                          (member c '(#\. #\_ #\-)))))
                 s))

(defun open-dependency-fix-pr (alert-id &key actor-id)
  "Attempt to open a fix PR for ALERT-ID. Caches the classified fix kind on the
   alert. Opens a real PR only for an unambiguous manifest version bump; returns
   :manual (without touching files) otherwise. Plist results:
     (:status :opened :pr P :branch B :commit SHA)
     (:status :manual :fix-kind K :reason R)
     (:status :no-fix)
     (:status :error :reason R)"
  (let ((alert (find-dep-alert-detailed alert-id)))
    (unless alert
      (return-from open-dependency-fix-pr (list :status :error :reason "alert not found")))
    (let ((fix-kind (classify-fix-kind alert)))
      (set-alert-fix-kind alert-id fix-kind)
      (when (string= fix-kind "none")
        (return-from open-dependency-fix-pr (list :status :no-fix)))
      (unless (string= fix-kind "manifest")
        (return-from open-dependency-fix-pr
          (list :status :manual :fix-kind fix-kind
                :reason "transitive/lockfile fix needs package-manager tooling")))
      (let* ((repo-id (getf alert :repo-id))
             (repo (find-repo-by-id repo-id))
             (owner (and repo (repo-owner-name repo)))
             (name (and repo (getf repo :name)))
             (manifest (getf alert :manifest-path)))
        (when (or (null repo) (null manifest) (string= manifest "sbom"))
          (return-from open-dependency-fix-pr
            (list :status :manual :fix-kind fix-kind
                  :reason "no manifest path recorded for this dependency")))
        (let* ((disk (repo-disk-path owner name))
               (base (or (chamber-get-default-branch owner name) "main"))
               (pkg (getf alert :package-name))
               (from (getf alert :version))
               (to (getf alert :fix-version))
               (content (handler-case (git-blob disk base manifest) (error () nil)))
               (new-content (safe-bump-manifest content from to)))
          (unless new-content
            (return-from open-dependency-fix-pr
              (list :status :manual :fix-kind fix-kind
                    :reason "version not found unambiguously in manifest (ranged/lockfile)")))
          (let* ((branch (format nil "~A/~A-~A-~A" *fix-branch-prefix*
                                 (%sanitize-branch-component (getf alert :ecosystem))
                                 (%sanitize-branch-component pkg)
                                 (%sanitize-branch-component to)))
                 (message (format nil "deps: bump ~A from ~A to ~A (~A)~%~%Fixes ~A."
                                  pkg from to (getf alert :ecosystem)
                                  (getf alert :osv-id)))
                 (sha (git-commit-file-on-branch disk base branch manifest
                                                 new-content message)))
            (unless sha
              (return-from open-dependency-fix-pr
                (list :status :error :reason "failed to create fix commit")))
            (let ((pr (create-pull-request
                       :repo-id repo-id
                       :author-id (or actor-id (ensure-dependency-bot-user))
                       :source-branch branch :target-branch base :head-commit sha)))
              (set-alert-fix-pr alert-id (getf pr :id))
              (list :status :opened :pr pr :branch branch :commit sha))))))))
