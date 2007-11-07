
(in-package :weblocks-test)

;;; test yield->do-place
(deftest yield->do-place-1
    (weblocks::yield->do-place '(1 two (three 4) (5 (yield 6) 7) 8) 'foo)
  (1 two (three 4) (5 (do-place foo 6) 7) 8))

(deftest yield->do-place-2
    (weblocks::yield->do-place nil 'foo)
  nil)

;;; test with-flow
(deftest with-flow-1
    (let ((c1 (make-instance 'composite))
	  (c2 (make-instance 'composite)))
      (with-flow (composite-widgets c1)
	(yield c2))
      (values (eq (car (composite-widgets c1)) c2)
	      (progn (answer c2)
		     (null (composite-widgets c1)))))
  t t)

