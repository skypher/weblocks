
(in-package :weblocks-test)

;;; testing root-composite
(deftest root-composite-1
    (with-request :get nil
      (root-composite))
  nil nil)

(deftest root-composite-2
    (with-request :get nil
      (setf (root-composite) 'foobar)
      (multiple-value-bind (res present-p)
	  (root-composite)
	(values res (not (null present-p)))))
  foobar t)

