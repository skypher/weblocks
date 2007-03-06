
(defpackage #:weblocks-test
  (:use :cl :weblocks :rt)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(defun test-weblocks ()
  (do-tests))
