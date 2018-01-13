(defpackage #:weblocks.widgets.string-widget
  (:use #:cl)
  (:export
   #:make-string-widget
   #:string-widget
   #:get-content
   #:escape-p))
(in-package weblocks.widgets.string-widget)


(weblocks.widget:defwidget string-widget ()
  ((content :type string
            :accessor get-content
            :initarg :content)
   (escape-p :type boolean
             :accessor escape-p
             :initarg :escape-p
             :initform t
             :documentation "Whether to escape the output
             for HTML.")))

(defmethod weblocks.widget:render ((widget string-widget))
  (let ((content (get-content widget)))
    (if (escape-p widget)
        (weblocks.html:with-html
          (:p content))
        (weblocks.html:with-html
          (:p (:raw content))))))


(defun make-string-widget (string &key (escape-p t))
  "Create a widget from a string."
  (make-instance 'string-widget
                 :content string
                 :escape-p escape-p))

