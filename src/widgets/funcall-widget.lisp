(defpackage #:weblocks.widgets.funcall
  (:use #:cl)
  (:export
   #:make-funcall-widget))
(in-package weblocks.widgets.funcall)


(weblocks.widget:defwidget funcall-widget ()
  ((function :type (or symbol function)
             :accessor get-function
             :initarg :function)))


(defmethod weblocks.widget:render ((widget funcall-widget))
  (let (args)
    ;; TODO: refactor widget class and move continuations into
    ;;       a separate class.
    (when (weblocks.widget::widget-continuation widget)
      (setf args (cons (weblocks.widget::widget-continuation widget) args)))
    
    (let ((func (get-function widget)))
      (etypecase func
        (symbol
         (if (fboundp func)
             (apply func args)
             (error "Cannot render ~A as widget. Symbol not bound to a function."
                    func)))
        (function
         (apply func args))))))


(defun make-funcall-widget (func)
  "Create a widget from a symbol denoting a function."
  (check-type func (or symbol function))
  (make-instance 'funcall-widget :function func))


(defmethod create-widget-from ((object function))
  (make-funcall-widget object))
