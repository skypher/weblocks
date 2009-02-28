
(in-package :weblocks)

(wexport '(make-class) '(t util))

(defun make-class (slots &optional (class-name (gensym)))
  "Makes an anonymous class with a number of slots.
'slots' - a list of symbols to be used as slot names."
  (ensure-class class-name
		:direct-superclasses (list (find-class 'standard-object))
		:direct-slots (mapcar (lambda (slot)
					(list :name slot
					      :readers nil
					      :writers nil
					      :initargs nil
					      :initform nil
					      :initfunction (constantly nil)))
				      slots)))

