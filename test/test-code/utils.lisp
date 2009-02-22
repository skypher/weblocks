
(in-package :weblocks-test)

;; unfortunate that `ensure-same' doesn't accept lambda expr
(defun set-equal-equal (a b)
  (set-equal a b :test #'equal))

(defun set-equal-uri= (a b)
  (set-equal a b :test #'puri:uri=))

(defun remove-all-methods (gf)
  (mapc (f_ (remove-method gf _)) (copy-list (generic-function-methods gf))))
