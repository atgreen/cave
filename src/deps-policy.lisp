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
  "Parse a .cave/deps.yml into a plist (:automerge CEILING :ignore LIST).
   Defaults to automerge 'none'. Tolerant of NIL/blank/garbage input."
  (let ((automerge "none") (ignore '()))
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
  "Effective auto-merge ceiling for REPO: the org policy ceiling caps the repo's
   .cave/deps.yml request (repo can only narrow). User repos have no org cap.
   CONFIG (a parse-deps-config plist) may be supplied to avoid disk reads."
  (let* ((repo-cfg (or config
                       (read-repo-deps-config (repo-owner-name repo) (getf repo :name))))
         (repo-ceiling (getf repo-cfg :automerge "none"))
         (org-id (getf repo :org-id))
         (org-ceiling (if (and org-id (not (eq org-id :null)))
                          (let ((p (get-org-dep-policy org-id)))
                            (if p (getf p :automerge-ceiling "none") "none"))
                          "major")))            ; user repos: no org cap
    (narrower-ceiling org-ceiling repo-ceiling)))

(defun auto-merge-eligible-p (from to ceiling)
  "True when bumping FROM -> TO is within CEILING."
  (ceiling-allows-level-p ceiling (bump-level from to)))
