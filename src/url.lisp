(in-package #:cave)

;;;; Internal URL construction helpers.
;;;;
;;;; One place to build in-app links so path shape and query-string encoding
;;;; stay consistent across views, notifications, and API responses. Deeper
;;;; paths (e.g. settings/protect/…) stay as format strings at their call
;;;; sites.

(defun repo-url (owner repo &rest segments)
  "Site path for a repo: (repo-url \"o\" \"r\" \"settings\") => \"/o/r/settings\".
SEGMENTS are inserted verbatim; use TREE-URL / BLOB-URL for ?path= links."
  (format nil "/~A/~A~{/~A~}" owner repo segments))

(defun issue-url (owner repo num)
  "Permalink to issue NUM in OWNER/REPO."
  (format nil "/~A/~A/issues/~A" owner repo num))

(defun pr-url (owner repo num)
  "Permalink to pull request NUM in OWNER/REPO."
  (format nil "/~A/~A/pulls/~A" owner repo num))

(defun tree-url (owner repo ref path)
  "Directory-listing URL. REF and PATH are url-encoded so branch names with
slashes and paths with spaces/# survive the round-trip."
  (format nil "/~A/~A/tree/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))

(defun blob-url (owner repo ref path)
  "File-view URL. REF and PATH are url-encoded (see TREE-URL)."
  (format nil "/~A/~A/blob/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))

(defun raw-url (owner repo ref path)
  "Raw-file URL. REF and PATH are url-encoded (see TREE-URL)."
  (format nil "/~A/~A/raw/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))

(defun blame-url (owner repo ref path)
  "Blame-view URL. REF and PATH are url-encoded (see TREE-URL)."
  (format nil "/~A/~A/blame/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))

(defun commits-url (owner repo ref &optional path)
  "Commit-list URL for REF, optionally filtered to commits touching PATH."
  (if (and path (plusp (length path)))
      (format nil "/~A/~A/commits/~A?path=~A"
              owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path))
      (format nil "/~A/~A/commits/~A" owner repo (hunchentoot:url-encode ref))))
