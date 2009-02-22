
(in-package :weblocks)

(export '(gauge gauge-value gauge-maximum))


(defwidget gauge (widget)
  ((value :type real :accessor gauge-value :initarg :value :initform 0)
   (maximum :type real :accessor gauge-maximum :initarg :maximum :initform 1)))


(defmethod with-widget-header ((widget gauge) body-fn &rest args &key
                               prewidget-body-fn postwidget-body-fn &allow-other-keys)
    (apply body-fn widget args))


(defmethod render-widget-body ((gauge gauge) &rest args)
  (declare (ignore args))
  (let ((percentage (round (* (gauge-value gauge) 100)
                           (gauge-maximum gauge)) ))
    (with-html
      (:div :class "widget gauge"
            :style (format nil "width:~,0D%" percentage)))))

