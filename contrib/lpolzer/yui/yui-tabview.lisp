(in-package :weblocks)

(export '(yui-tabview yui-tabview-tabs yui-tabview-selected))

(defwidget yui-tabview (composite yui-widget)
  ((tabs :accessor yui-tabview-tabs
         :initarg :tabs
         :initform nil
         :documentation "Association list of tab names and widgets.")
   (selected :accessor yui-tabview-selected
             :initarg :selected 
             :type integer
             :initform 1
             :documentation "Tab that will be selected by default. An
             integer number, starting from 1 (e.g. the first tab
             corresponds to 1)."))
  (:default-initargs :modules '("tabview"))
  (:documentation "YUI TabView widget"))

(defmethod initialize-instance :after ((obj yui-tabview) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (composite-widgets obj) (mapcar #'cdr (yui-tabview-tabs obj)))
  (setf (yui-target-id obj) (dom-id obj)))

(defmethod widget-css-classes ((obj yui-tabview))
  (declare (ignore obj))
  "yui-navset")


(defmethod render-widget-body ((widget yui-tabview) &rest args)
  (flet ((render-tab (i) (let ((child (cdr (nth i (yui-tabview-tabs widget)))))
                           (render-widget child))))
    (with-html
      (:ul :class "yui-nav"
        (loop for name in (mapcar #'car (yui-tabview-tabs widget))
              for i from 1
              do (htm (:li :class (concatenate 'string
                                               (format nil "tab~A tab-~A" i (attributize-name name))
                                               (when (eql (yui-tabview-selected widget) i)
                                                 " selected"))
                           (:a :href (format nil "#tab~D" i)
                               (:em (esc (humanize-name name))))))))
      (:div :class "yui-content"
        (loop for child in (mapcar #'cdr (yui-tabview-tabs widget))
              do (htm (:div (render-widget child))))))
    #+OLD(send-script
      (ps:ps* `(new (|:YAHOO.widget.:TabView| ,(widget-dom-id widget)))))
    (send-script
      (ps* `(with-lazy-loaded-modules (,(yui-modules widget) 
				       ,@(yui-loader-args widget))
         (setf ,(yui-widget-variable widget)
               (new (|:YAHOO.widget.:TabView| ,(yui-target-id widget)
                                              (keywords-to-object ,(yui-component-config widget))))))))
    ))


#| Jan's version of rendering
;; this should probably render differently if javascript is not available
(defmethod render-widget-body ((obj yui-tabview) &rest args)
  (declare (ignore args))
  (let ((counter 0))
    (with-html
      (:ul :class "yui-nav"
           (mapcar (lambda (pane)
                     (incf counter)
                     (htm (:li :class (if (= counter (yui-tabview-selected obj))
                                          "selected"
                                          "")
                               (:a :href (format nil "#~A-pane~D"
                                                 (attributize-name (widget-name obj))
                                                 counter)
                                   (str (car pane))))))
                   (yui-tabview-tabs obj)))
      (setf counter 0)
      (:div :class "yui-content"
            (mapcar (lambda (pane)
                      (incf counter)
                      (htm (:div :id (format nil "~A-pane~D"
                                             (attributize-name (widget-name obj))
                                             counter)
                                 (render-widget (cdr pane)))))
                    (yui-tabview-tabs obj))))
    ))
|#

