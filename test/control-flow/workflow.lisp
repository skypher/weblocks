
(in-package :weblocks-test)

;;; test yield->do-widget
(deftest yield->do-widget-1
    (weblocks::yield->do-widget '(1 two (three 4) (5 (yield 6) 7) 8) 'foo)
  (1 two (three 4) (5 (do-widget foo 6) 7) 8))

(deftest yield->do-widget-2
    (weblocks::yield->do-widget nil 'foo)
  nil)

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

