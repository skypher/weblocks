
(in-package :weblocks)

(export '(do-place do-page answer))

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
called with 'result', or nil."
  (safe-funcall (widget-continuation continuation) result))

(defmacro do-place (place callee)
  "Expands to code that sets 'callee' as the widget in 'place', saves
the continuation, and returns from the delimited computation. When
'callee' answers, restores the original widget in 'place' and
reactivates the computation."
  (let ((original-value (gensym))
	(new-callee (gensym)))
    `(let ((,original-value ,place))
       (prog1
	   (call ,callee
		 (lambda (,new-callee)
		   (setf ,place ,new-callee)))
	 (setf ,place ,original-value)))))

(defun/cc do-page (callee)
  "Sets 'callee' as the only widget in the root composite, saves the
continuation, and returns from the delimited computation. When
'callee' answers, restores the original widgets in the root
composite and reactivates the computation."
  (do-place (composite-widgets (root-composite)) callee))

