
(in-package :weblocks-test)

;;; test with-flow
(deftest with-flow-1
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (w2 (make-instance 'composite))
	     (c1 (make-instance 'composite :widgets w1)))
	(with-flow w1
	  (yield w2))
	(values (eq (car (composite-widgets c1)) w2)
		(progn (answer w2)
		       (null (composite-widgets w1))))))
  t t)

