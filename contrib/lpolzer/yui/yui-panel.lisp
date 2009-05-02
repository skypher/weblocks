
(in-package :weblocks)

(export '(yui-panel panel-on-close panel-on-end-drag))

(defwidget yui-panel (yui-widget)
  ((on-close :type function :accessor panel-on-close :initarg :on-close :initform (constantly t))
   (on-end-drag :type function :accessor panel-on-end-drag :initarg :on-end-drag :initform (constantly t)))
  (:default-initargs :modules '("dragdrop" "container") :class-name |:YAHOO.widget.:Panel|))

(defmethod render-widget-body ((widget yui-panel) &rest args)
  (declare (ignore args))
  (send-script
    (ps* `(with-lazy-loaded-modules (,(yui-modules widget) ,@(yui-loader-args widget))
            (setf (global-variable ,(yui-widget-variable widget))
                  (new (,(yui-class-name widget) ,(yui-target-id widget)
                                                 (keywords-to-object ,(yui-component-config widget)))))
            ((@ (global-variable ,(yui-widget-variable widget)) render))
            ;(console.log ,(format nil "rendered yui widget ~A." (yui-widget-variable widget)))
            ))))

(defmethod with-widget-header ((widget yui-panel) body-fn &rest args)
  (apply body-fn widget args))

