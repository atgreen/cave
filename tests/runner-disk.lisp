;;; runner-disk.lisp — a full runner host should say so (cave-pav)
;;;
;;; h3 filled its root filesystem and every job then failed at "Failed to pull
;;; image ghcr.io/...", which reads as a registry or network problem. The actual
;;; error, buried in podman's output, was "no space left on device". A runner
;;; that cannot possibly succeed should say why before it starts, in words that
;;; name the host and the disk.
;;;
;;;   make test-runner-disk
;;;
;;; SPDX-License-Identifier: MIT

(asdf:load-system :cave)

(in-package #:cl-user)

;; --- Reading df output ---------------------------------------------------
;;
;; POSIX `df -Pk` guarantees one header line then one line per filesystem, with
;; available blocks in the fourth field. Long device names are NOT wrapped under
;; -P, which is the whole reason for that flag.

(let ((out "Filesystem     1024-blocks      Used Available Capacity Mounted on
/dev/mapper/rhel-root     51175040  47000000   4175040      92% /"))
  (assert (= 4175040 (cave::%df-available-kb out)) ()
          "should read the Available column, got ~S" (cave::%df-available-kb out)))

(let ((out "Filesystem     1024-blocks      Used Available Capacity Mounted on
/dev/mapper/rhel-root     51175040  51175000        40     100% /"))
  (assert (= 40 (cave::%df-available-kb out)) ()
          "a nearly full filesystem should read back tiny, not zero-by-accident"))

;; Garbage in, NIL out: an unreadable df must not be mistaken for "no space",
;; which would fail every job on every runner.
(dolist (bad '("" "df: /nope: No such file or directory" "header only"))
  (assert (null (cave::%df-available-kb bad)) ()
          "unparseable df output should yield NIL, got ~S for ~S"
          (cave::%df-available-kb bad) bad))

;; --- The decision --------------------------------------------------------

(assert (null (cave::insufficient-disk-message "/tmp" 100 :available-kb (* 500 1024)))
        () "plenty of room should produce no complaint")

(let ((msg (cave::insufficient-disk-message "/var/lib/cave-runner" 2048
                                            :available-kb (* 40 1024))))
  (assert msg () "40MB free against a 2048MB floor should complain")
  (assert (search "40" msg) () "the message should say how much is left: ~S" msg)
  (assert (search "2048" msg) () "and how much is needed: ~S" msg)
  (assert (search "/var/lib/cave-runner" msg) ()
          "and which path ran out, since a runner host has several: ~S" msg)
  (assert (search "disk" (string-downcase msg)) ()
          "and the word disk, so nobody reads it as a registry failure: ~S" msg))

;; Exactly at the floor is fine; below it is not.
(assert (null (cave::insufficient-disk-message "/tmp" 100 :available-kb (* 100 1024)))
        () "at the floor exactly should pass")
(assert (cave::insufficient-disk-message "/tmp" 100 :available-kb (- (* 100 1024) 1))
        () "one kilobyte under the floor should fail")

;; An unreadable filesystem is not evidence of a full one.
(assert (null (cave::insufficient-disk-message "/tmp" 2048 :available-kb nil))
        () "unknown free space must not block jobs - a broken df would otherwise ~
            take every runner offline")

(format t "~&Runner disk tests passed.~%")
