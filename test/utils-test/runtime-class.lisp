
(in-package :weblocks-test)

(deftest make-class-1
    (let ((obj (make-instance (make-class '(foo bar baz)))))
      (values (slot-value obj 'foo)
	      (slot-value obj 'bar)
	      (slot-value obj 'baz)))
  nil nil nil)

(deftest make-class-2
    (let ((obj (make-instance (make-class '(foo bar baz)))))
      (setf (slot-value obj 'foo) 1
	    (slot-value obj 'bar) 2
	    (slot-value obj 'baz) 3)
      (values (slot-value obj 'foo)
	      (slot-value obj 'bar)
	      (slot-value obj 'baz)))
  1 2 3)

