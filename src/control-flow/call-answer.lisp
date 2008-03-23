
(in-package :weblocks)

(export '(do-widget do-page do-modal answer))

;;; Specialize widget-continuation
(defmethod widget-continuation ((widget function))
  widget)

(defmethod (setf widget-continuation) (value obj)
  ; Nothing we can do here, but we need this to be silent
  value)

(defmethod widget-continuation ((widget symbol))
  (get widget 'continuation))

(defmethod (setf widget-continuation) (value (widget symbol))
  (setf (get widget 'continuation) value))

(defun/cc call (callee op)
  "Saves the current continuation to the appropriate place and
interrupts the computation. Before the computation is interrupted,
calls 'op' with the value of the widget where the computation is
saved (may be different from 'callee' because of objects like
functions). If 'callee' is of class 'widget', the continuation is
saved in the 'current-continuation' slot of 'callee'. If 'callee' is a
function, continuation is curried as a first parameter and the result
is returned. Otherwise, continuation isn't saved."
  (let/cc k
    (setf (widget-continuation callee) k)
    (funcall op (if (functionp callee)
		    (curry callee k)
		    callee))
    t))

(defun answer (continuation &optional result)
  "Returns control to location saved in 'continuation', which may be a
callee widget or the continuation object explicitly. Continuation is
called with 'result', or nil. If the widget doesn't have a
continuation, recursively tries its parents."
  (if (widget-continuation continuation)
      (safe-funcall (widget-continuation continuation) result)
      (when (widget-parent continuation)
	(answer (widget-parent continuation) result))))

;; Places 'callee' in the place of 'widget', saves the continuation,
;; and returns from the delimited computation. When 'callee' answers,
;; restores the original widget and reactivates the computation. If
;; 'wrapper-fn' is present, passes it the new callee and sets the return
;; value as the value of a place. By default 'wrapper-fn' is simply an
;; identity function.
(defun/cc do-widget (widget callee &optional (wrapper-fn #'identity))
  (if (or (null widget)
	  (eq widget (root-composite)))
      (do-root-widget callee wrapper-fn)
      (do-widget-aux widget callee wrapper-fn)))

(defun/cc do-root-widget (callee  &optional (wrapper-fn #'identity))
  (let* ((old-value (composite-widgets (root-composite))))
    (prog1
	(call callee
	      (lambda (new-callee)
		(setf (composite-widgets (root-composite))
		      (funcall wrapper-fn new-callee))))
      (setf (composite-widgets (root-composite))
	    old-value))))

(defun/cc do-widget-aux (widget callee &optional (wrapper-fn #'identity))
  (let* ((parent (widget-parent widget))
	 (place (member widget (composite-widgets parent))))
    (unless parent
      (error "Attempted to replace widget ~S which has no parent!" widget))
    (unless place
      (error "Widget ~S was not added to parent ~S properly - it cannot be found in parent."
	     widget parent))
    (flet ((place-widget (value)
	     (rplaca place value)
	     (setf (composite-widgets parent)
		   (composite-widgets parent))))
      (prog1
	  (call callee
		(lambda (new-callee)
		  (place-widget (funcall wrapper-fn new-callee))))
	(place-widget widget)))))

;; Sets 'callee' as the only widget in the root composite, saves the
;; continuation, and returns from the delimited computation. When
;; 'callee' answers, restores the original widgets in the root
;; composite and reactivates the computation.
(defun/cc do-page (callee)
  (do-widget nil callee))

(defun/cc do-modal (title callee &key css-class)
  "Same as 'do-page', but wraps the callee in a div for styling purposes."
  (do-widget nil callee
	     (lambda (new-callee)
	       (lambda (&rest args)
		 (declare (ignore args))
		 (with-html
		   (:div :class "modal"
			 (:h1 (:span (str title)))
			 (:div :class css-class
			       (render-widget new-callee))))))))

