
(defpackage #:weblocks-test
  (:use :cl :weblocks :rt :c2mop)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(defun test-weblocks ()
  (do-tests))
