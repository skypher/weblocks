
(in-package :weblocks)

(export '(string-widget string-widget-content string-widget-escape-p))

(defwidget string-widget ()
  ((content :type string
            :accessor string-widget-content
            :initarg :content)
   (escape-p :type boolean
             :accessor string-widget-escape-p
             :initarg :escape-p
             :initform t
             :documentation "Whether to escape the output
             for HTML.")))

(defmethod render-widget-body ((widget string-widget) &rest args &key id class &allow-other-keys)
  (declare (ignore args))
  (let ((content (if (string-widget-escape-p widget)
                   (escape-string (string-widget-content widget))
                   (string-widget-content widget))))
    (with-html
      (:p :id id :class class (str content)))))
                   
(defmethod make-widget ((obj string))
  "Create a widget from a string."
  (make-instance 'string-widget :content obj))

