(in-package :weblocks)

(export '(template-form-view))

(defclass template-form-view (form-view)
  ((template :initform :template-form-view-body-wt :initarg :template)
   (additional-variables-fn :initarg :additional-variables-fn :initform nil :accessor template-form-view-additional-variables-fn))
  (:documentation "Similar to form-view but every field is passed to template "))

(defclass template-form-view-field (form-view-field)
  ())

(defclass template-form-scaffold (form-scaffold)
  ())

(defun template-form-view-body-wt (&rest args &key caption class-name validation-summary form-view-buttons content method action form-id header-class enctype extra-submit-code use-ajax-p fields-data &allow-other-keys)
  (with-html-to-string
    (with-html-form (method action
                            :id form-id
                            :class header-class
                            :enctype enctype
                            :extra-submit-code extra-submit-code
                            :use-ajax-p use-ajax-p)
      (when caption
        (htm (:h1 (fmt caption class-name))))
      (str validation-summary)
      (:ul (loop for (key value) on fields-data  :by #'cddr
                 do 
                 (cl-who:htm (str value))))
      (str form-view-buttons))))

(deftemplate :template-form-view-body-wt 'template-form-view-body-wt)

(defmethod with-view-header ((view template-form-view) 
                             obj widget body-fn &rest args &key
                             (method (form-view-default-method view))
                             (action (form-view-default-action view))
                             (fields-prefix-fn (view-fields-default-prefix-fn view))
                             (fields-suffix-fn (view-fields-default-suffix-fn view))
                             validation-errors
                             &allow-other-keys)
  (declare (special *on-ajax-complete-scripts* *form-submit-dependencies*))

  (with-slots (template additional-variables-fn) view
    (let ((form-id (gen-id))
          (fields-data)
          (field-values-data)
          (labels-data))
      (apply #'map-view-fields 
             (lambda (field-info)
               (let ((*presentation-dom-id* (gen-id)))
                 (setf field-values-data 
                       (append field-values-data 
                               (list (alexandria:make-keyword (view-field-slot-name (field-info-field field-info))) 
                                     (capture-weblocks-output 
                                       (let ((field (field-info-field field-info))
                                             (obj (field-info-object field-info)))
                                         (apply #'render-view-field-value
                                                (obtain-view-field-value field obj)
                                                (view-field-presentation field)
                                                field view widget 
                                                obj args)))))))
               (setf fields-data 
                     (append fields-data 
                             (list 
                               (alexandria:make-keyword (view-field-slot-name (field-info-field field-info))) 
                               (capture-weblocks-output 
                                 (let ((field (field-info-field field-info))
                                       (obj (field-info-object field-info)))
                                   (safe-apply (view-field-prefix-fn field) view field obj args)
                                   (apply #'render-view-field
                                          field view widget (view-field-presentation field)
                                          (obtain-view-field-value field obj) obj 
                                          :field-info field-info
                                          args)
                                   (safe-apply (view-field-suffix-fn field) view field obj args))))))
               (setf labels-data 
                     (append labels-data 
                             (list (alexandria:make-keyword (view-field-slot-name (field-info-field field-info)))
                                   (translate (view-field-label (field-info-field field-info)))))))
             view obj args)

      (apply 
        #'render-wt 
        (list* 
          template
          (list :view view :object obj :method method)
          :method method 
          :action action
          :header-class (attributize-name (object-class-name obj))
          :enctype (form-view-default-enctype view)
          :extra-submit-code (capture-weblocks-output (render-form-submit-dependencies *form-submit-dependencies*))
          :caption (view-caption view) 
          :form-id (when (form-view-focus-p view) form-id)
          :use-ajax-p (form-view-use-ajax-p view)
          :class-name (humanize-name (object-class-name obj))
          :validation-summary (capture-weblocks-output 
                                (render-validation-summary view obj widget validation-errors))
          :form-view-buttons (capture-weblocks-output (apply #'render-form-view-buttons view obj widget args))
          :fields-data fields-data
          :field-values-data field-values-data
          :labels-data labels-data
          :content (capture-weblocks-output (apply body-fn view obj args))
          (append 
            (loop for (key value) on fields-data  :by #'cddr
                  append (list 
                           (alexandria:make-keyword (format nil "~A-FIELD" key))
                           value))
            (loop for (key value) on field-values-data  :by #'cddr
                  append (list 
                           (alexandria:make-keyword (format nil "~A-FIELD-VALUE" key))
                           value))
            (loop for (key value) on labels-data  :by #'cddr
                  append (list 
                           (alexandria:make-keyword (format nil "~A-LABEL" key))
                           value))
            (safe-apply additional-variables-fn (list :view view :object obj :widget widget)))))))
  (when (form-view-focus-p view)
    (send-script (ps* `((@ ($ ,form-id) focus-first-element))))))
