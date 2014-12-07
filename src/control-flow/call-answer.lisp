
(in-package :weblocks)

(export '(do-widget do-page do-modal answer make-widget-place-writer 
          adopt-widget widget-not-in-parent))


(define-condition widget-not-in-parent (webapp-style-warning)
  ((widget :accessor widget :initarg :widget)
   (parent :accessor parent :initarg :parent))
  (:report report-widget-not-in-parent)
  (:documentation "This style warning serves as a makeshift
                  until we find a proper way to have an idempotent
                  DO-WIDGET."))

(defun report-widget-not-in-parent (c stream)
  "Display human-readably that a widget could not be found in its parent."
  (format stream "Widget ~S cannot be found in parent ~S."
          (widget c) (parent c)))

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
calls OP with the value of the widget where the computation is
saved (may be different from CALLEE because of objects like
functions).

If CALLEE is of class WIDGET, the continuation is
saved in the 'continuation symbol of CALLEE.

If CALLEE is a function, continuation is curried as a first parameter
and the result is returned. Otherwise the continuation isn't passed."
  (let/cc k
    (setf (widget-continuation callee) k)
    (safe-funcall op (if (functionp callee)
                         (curry callee k)
                         callee))
    t))

(defun answer (continuation &optional result)
  "Returns control to location saved in CONTINUATION, which may be a
callee widget or the continuation object explicitly. Continuation is
called with RESULT (defaulting to NIL). If the widget doesn't have a
continuation, recursively tries its parents."
  (mark-dirty (root-widget))
  (if (widget-continuation continuation)
      (safe-funcall (widget-continuation continuation) result)
      (when (widget-parent continuation)
        (answer (widget-parent continuation) result))))

(defun adopt-widget (parent widget)
  "Like (setf (widget-parent WIDGET) PARENT), but signal an error when
WIDGET already has a parent (even if it's PARENT)."
  (let ((old-parent (widget-parent widget)))
    (if old-parent
        (error "Widget ~A already has parent ~A; cannot write parent" 
               widget old-parent)
        (setf (widget-parent widget) parent)))
  (values))

(defun/cc do-widget (widget callee &optional (wrapper-fn #'identity))
  "Places CALLEE in the place of WIDGET, saves the continuation,
and returns from the delimited computation. When CALLEE answers,
restores the original widget and resumes the computation.

If WRAPPER-FN is present, passes it the new callee and sets the return
value as the value of a place. By default WRAPPER-FN is simply the
identity function."
  (if (or (null widget)
          (eq widget (root-widget)))
      (do-root-widget callee wrapper-fn)
      (do-widget-aux widget callee wrapper-fn)))

(defun/cc do-widget-aux (widget callee &optional (wrapper-fn #'identity))
  (let* ((parent (or (widget-parent widget)
                     (error "Attempted to replace widget ~S which has no parent!"
                            widget)))
         (place-writer (make-widget-place-writer parent widget)))
    (prog1
        (call callee
              (lambda (new-callee)
                (funcall place-writer (funcall wrapper-fn new-callee))))
      ;; the following is the rest of the computation 
      (funcall place-writer widget))))

;; This function is aware of the internal structure of the root widget;
;; this should be OK as it's a special case; it does violate the contract.
(defun/cc do-root-widget (callee  &optional (wrapper-fn #'identity))
  "Replace the children of the root widget with CALLEE,
the latter one being optionally transformed by WRAPPER-FN."
  (let* ((old-value (slot-value (root-widget) 'children)))
    (prog1
      (call callee
            (lambda (new-callee)
              (setf (widget-children (root-widget))
                    (funcall wrapper-fn new-callee))))
      (setf (slot-value (root-widget) 'children) old-value)
      (update-parent-for-children (widget-children (root-widget))))))

(defun/cc do-page (callee)
  "Sets CALLEE as the only child in the root widget, saves the
continuation, and returns from the delimited computation. When
CALLEE answers, restores the original children in the root
widget and reactivates the computation."
  (mark-dirty (root-widget))
  (do-widget nil callee))

(defun modal-wt (&key title content css-class)
  (with-html-to-string
    (:div :class "modal"
     (:h1 (:span (str title)))
     (:div :class css-class
      (str content)))))

(deftemplate :modal-wt 'modal-wt)

(defun/cc do-modal (title callee &key css-class)
  "Same as DO-PAGE, but wraps CALLEE in a div container
for styling purposes."
  (do-widget nil callee
             (lambda (new-callee)
               (lambda (&rest args)
                 (declare (ignore args))
                 (let ((weblocks-stream *weblocks-output-stream*))
                   (write-string 
                     (render-template-to-string 
                       :modal-wt 
                       (list :css-class css-class :callee callee)
                       :title title 
                       :content (capture-weblocks-output (render-widget new-callee))
                       :css-class css-class)
                     weblocks-stream))))))

