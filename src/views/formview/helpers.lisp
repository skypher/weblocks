
(in-package :weblocks)

(export '(make-slot-writer))

(defun make-slot-writer (slot-name slot-reader)
  "Returns a function that accepts a value and object and stores the
value transformed by 'slot-reader' in the object's 'slot-name'. This
is useful when declaring writers in the UI that should record the
object ID instead of the object itself."
  (lambda (value obj)
    (setf (slot-value obj slot-name)
	  (when value
	    (funcall slot-reader value)))))

