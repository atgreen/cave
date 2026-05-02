;;; git.lisp — Git CLI integration
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; Shell out to git for all operations. Simple and correct.

(defun git-run (repo-path &rest args)
  "Run a git command in REPO-PATH. Returns (VALUES output error-output exit-code)."
  (let ((cmd (append (list "git" "-C" (namestring repo-path)) args)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program cmd
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (values output error-output exit-code))))

(defun git-branches (repo-path)
  "List branches in a bare repo. Returns list of branch name strings."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "branch" "--format=%(refname:short)")
    (declare (ignore _err))
    (when (zerop exit-code)
      (remove-if #'uiop:emptyp
                 (uiop:split-string output :separator '(#\Newline))))))

(defun git-default-branch (repo-path)
  "Get the default branch (HEAD target) of a bare repo."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "symbolic-ref" "--short" "HEAD")
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-log (repo-path &key (branch nil) (limit 20))
  "Get recent commits. Returns list of plists (:hash :short-hash :author :date :subject)."
  (let* ((format-str "%H%n%h%n%an%n%ai%n%s%n---")
         (args (list "log" (format nil "--format=~A" format-str)
                     (format nil "-~A" limit)))
         (args (if branch (append args (list branch)) args)))
    (multiple-value-bind (output _err exit-code)
        (apply #'git-run repo-path args)
      (declare (ignore _err))
      (when (zerop exit-code)
        (parse-git-log output)))))

(defun parse-git-log (output)
  "Parse git log output in our custom format."
  (let ((entries nil)
        (lines (uiop:split-string output :separator '(#\Newline))))
    (loop while (>= (length lines) 5)
          do (let ((hash (pop lines))
                   (short-hash (pop lines))
                   (author (pop lines))
                   (date (pop lines))
                   (subject (pop lines)))
               ;; Skip separator
               (when (and lines (equal (first lines) "---"))
                 (pop lines))
               (push (list :hash hash
                           :short-hash short-hash
                           :author author
                           :date date
                           :subject subject)
                     entries)))
    (nreverse entries)))

(defun git-file-tree (repo-path &key (ref "HEAD"))
  "Get the file tree at REF. Returns list of (:mode :type :hash :name)."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "ls-tree" "-r" "--name-only" ref)
    (declare (ignore _err))
    (when (zerop exit-code)
      (remove-if #'uiop:emptyp
                 (uiop:split-string output :separator '(#\Newline))))))

(defun git-diff (repo-path base-ref head-ref)
  "Get diff between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-diff-stat (repo-path base-ref head-ref)
  "Get diff stat between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" "--stat" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-commit-count (repo-path &key (branch nil))
  "Count commits on a branch (or all if nil)."
  (let ((args (if branch
                  (list "rev-list" "--count" branch)
                  (list "rev-list" "--count" "--all"))))
    (multiple-value-bind (output _err exit-code)
        (apply #'git-run repo-path args)
      (declare (ignore _err))
      (when (zerop exit-code)
        (parse-integer output :junk-allowed t)))))

(defun git-repo-empty-p (repo-path)
  "Check if a repo has any commits."
  (multiple-value-bind (_out _err exit-code)
      (git-run repo-path "rev-parse" "HEAD")
    (declare (ignore _out _err))
    (/= exit-code 0)))
