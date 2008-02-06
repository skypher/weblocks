
(in-package :weblocks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod view-argument-quoting-strategy ((arg-name (eql :present-as)))
    :list)
    
  (setf (gethash :present-as *custom-view-field-argument-compilers*)
	(lambda (slot-name presentation)
	  (let ((presentation (ensure-list presentation)))
	    `(setf (view-field-presentation ,slot-name)
		   (funcall #'make-instance (presentation-class-name ',(car presentation))
			    ,@(quote-property-list-arguments
			       (cdr presentation))))))))

