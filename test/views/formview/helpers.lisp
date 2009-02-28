
(in-package :weblocks-test)

;;; Test make-slot-writer
(deftest make-slot-writer-1
    (let ((obj (copy-template *joe*)))
      (funcall 
       (make-slot-writer 'name (lambda (value)
				 (declare (ignore value))
				 "foo"))
       "bak" obj)
      (first-name obj))
  "foo")

