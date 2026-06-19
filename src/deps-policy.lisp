;;; deps-policy.lisp — dependency-update governance.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Resolves the effective auto-merge policy for a repo: the org's
;;; cave_org_dep_policy ceiling *caps* the repo's own .cave/deps.yml request
;;; (a repo can only narrow, never widen). Plus the eligibility predicate the
;;; auto-merge processor uses. See docs/design/DESIGN_DEPENDENCY_UPDATES.md.

(in-package #:cave)

(defparameter *automerge-levels* '("none" "patch" "minor" "major")
  "Auto-merge ceilings, increasing permissiveness.")

(defun %ceiling-rank (ceiling)
  "Numeric rank of a ceiling string (unknown -> 0 = none)."
  (or (position ceiling *automerge-levels* :test #'string-equal) 0))

(defun %level-rank (level)
  "Numeric rank of a bump level keyword (:patch/:minor/:major)."
  (case level (:patch 1) (:minor 2) (:major 3) (t 4)))

(defun ceiling-allows-level-p (ceiling level)
  "True when a CEILING string permits auto-merging a LEVEL bump."
  (and (> (%ceiling-rank ceiling) 0)
       (<= (%level-rank level) (%ceiling-rank ceiling))))

(defun narrower-ceiling (a b)
  "The more restrictive of two ceiling strings."
  (if (<= (%ceiling-rank a) (%ceiling-rank b)) a b))

(defun parse-deps-config (yaml-string)
  "Parse a .cave/deps.yml into a plist (:automerge CEILING-or-NIL :ignore LIST).
   :automerge is NIL when unspecified (so the caller can inherit). Tolerant of
   NIL/blank/garbage input."
  (let ((automerge nil) (ignore '()))
    (handler-case
        (let ((alist (and yaml-string (plusp (length yaml-string))
                          (yaml-parse yaml-string))))
          (when (listp alist)
            (let ((am (cdr (assoc "automerge" alist :test #'string-equal)))
                  (ig (cdr (assoc "ignore" alist :test #'string-equal))))
              (when (and (stringp am)
                         (member am *automerge-levels* :test #'string-equal))
                (setf automerge (string-downcase am)))
              (when (listp ig) (setf ignore (remove-if-not #'stringp ig))))))
      (error () nil))
    (list :automerge automerge :ignore ignore)))

(defun read-repo-deps-config (owner repo-name &optional (ref "HEAD"))
  "Read and parse .cave/deps.yml from the repo, or defaults if absent."
  (parse-deps-config
   (handler-case (git-blob (repo-disk-path owner repo-name) ref ".cave/deps.yml")
     (error () nil))))

(defun effective-automerge-ceiling (repo &key config)
  "Effective auto-merge ceiling for REPO. The org policy ceiling is the cap; the
   repo's .cave/deps.yml can only narrow it. An org repo with no config inherits
   the org cap; a user repo with no config is off ('none'). CONFIG (a
   parse-deps-config plist) may be supplied to avoid disk reads.

   Off by default: org cap defaults to 'none' until an org policy is set."
  (let* ((repo-cfg (or config
                       (read-repo-deps-config (repo-owner-name repo) (getf repo :name))))
         (explicit (getf repo-cfg :automerge))      ; string, or NIL when absent
         (org-id (getf repo :org-id))
         (org-repo (and org-id (not (eq org-id :null))))
         (cap (if org-repo
                  (let ((p (get-org-dep-policy org-id)))
                    (if p (getf p :automerge-ceiling "none") "none"))
                  "major"))                          ; user repos: no org cap
         (request (or explicit (if org-repo cap "none"))))
    (narrower-ceiling cap request)))

(defun auto-merge-eligible-p (from to ceiling)
  "True when bumping FROM -> TO is within CEILING."
  (ceiling-allows-level-p ceiling (bump-level from to)))

(defun process-dependency-automerge (&key (verbose t))
  "Merge eligible, CI-green dependency-fix PRs. Off by default: each repo's
   effective ceiling is 'none' until an org/repo opts in. Only acts on bot-opened
   fix PRs whose head commit is green. Returns the count merged."
  (let ((merged 0))
    (dolist (cand (dep-automerge-candidates))
      (handler-case
          (let* ((repo (find-repo-by-id (getf cand :repo-id)))
                 (owner (and repo (repo-owner-name repo)))
                 (name (and repo (getf repo :name))))
            (when (and repo
                       (auto-merge-eligible-p (getf cand :version)
                                              (getf cand :fix-version)
                                              (effective-automerge-ceiling repo))
                       (eq (combined-commit-status (getf cand :repo-id)
                                                   (getf cand :head-commit))
                           :success))
              ;; Same call shape as the web merge route (server.lisp).
              (multiple-value-bind (ok err)
                  (chamber-merge-branch owner name
                                        (getf cand :source-branch)
                                        (getf cand :target-branch))
                (cond
                  (ok
                   (merge-pull-request (getf cand :pr-id))
                   (set-dep-alert-state (getf cand :alert-id) "auto_fixed")
                   (log-event "pr.merged"
                              :user-id (ensure-dependency-bot-user)
                              :repo-id (getf cand :repo-id)
                              :entity-type "pull_request"
                              :entity-id (getf cand :pr-id))
                   (let ((pr (find-pull-request-by-id (getf cand :pr-id))))
                     (when pr (handler-case (notify-pr-merged repo owner name pr)
                                (error () nil))))
                   (incf merged)
                   (when verbose
                     (format t "  merged #~A: ~A ~A -> ~A~%"
                             (getf cand :pr-number) (getf cand :package-name)
                             (getf cand :version) (getf cand :fix-version))))
                  (verbose
                   (format t "  skip #~A: merge failed (~A)~%"
                           (getf cand :pr-number) err))))))
        (error (e)
          (llog:warn "Auto-merge failed" :alert (getf cand :alert-id)
                                         :error (princ-to-string e)))))
    (when verbose (format t "~&Auto-merged ~A dependency PR(s).~%" merged))
    merged))
