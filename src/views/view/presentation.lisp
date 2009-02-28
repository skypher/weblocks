
(in-package :weblocks)

(export '(presentation))

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

(defclass presentation ()
  ()
  (:documentation "Base class for all presentations. Exists in order
to help manage CSS and JavaScript dependencies for presentations."))

;; by default presentations have no dependencies, so the primary method
;; returns nil
(defmethod dependencies append ((obj presentation))
  ())
