
(in-package :weblocks)

(export '(yui-resize resize-proxy-resize-p resize-panel))

(defwidget yui-resize (yui-widget)
  ((panel :type (or symbol nil) :accessor resize-panel :initarg :panel :initform nil))
  (:default-initargs :modules '("resize") :class-name |:YAHOO.util.:Resize|))

(defmethod with-widget-header ((widget yui-resize) body-fn &rest args)
  (apply body-fn widget args))

(defmethod render-widget-body ((widget yui-resize) &rest args)
  (declare (ignore args))
  (if (resize-panel widget) 
    (progn 
      (send-script
        (ps* `(with-lazy-loaded-modules (,(yui-modules widget) ,@(yui-loader-args widget))
                ;(|:YAHOO.util.:Event.:addListener| (global-variable ,(resize-panel widget)) "init" 
                (|:YAHOO.util.:Event.:onAvailable| ,(format nil "~A_c" (yui-target-id widget)) ; fragile hack
                   (lambda (obj)
                     (setf ,(yui-widget-variable widget)
                           (new (,(yui-class-name widget) ,(yui-target-id widget)
                                                          (keywords-to-object ,(yui-component-config widget)))))

                     (init-yui-resize ,(yui-widget-variable widget) ,(resize-panel widget))
                     #+OFF(console.log ,(format nil "rendered yui widget ~A." (yui-widget-variable widget)))))))))
    (call-next-method)))

