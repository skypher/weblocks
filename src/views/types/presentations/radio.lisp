
(in-package :weblocks)

(export '(radio radio-presentation))

;;; Radio buttons
(defclass radio-presentation (form-presentation choices-presentation-mixin)
  ())

(defun radio-view-field-wt (&key field-class content field-label show-required-indicator required-indicator-label validation-error)
  (with-html-to-string
    (:li :class field-class
     (:span :class "label"
      (:span :class "slot-name"
       (:span :class "extra"
        (str field-label) ":&nbsp;"
        (when show-required-indicator 
          (htm 
            (:em :class "required-slot"
             (str required-indicator-label)
             (str "&nbsp;")))))))
     (str content)
     (when validation-error
       (htm (:p :class "validation-error"
             (:em
               (:span :class "validation-error-heading" "Error:&nbsp;")
               (str validation-error))))))))

(deftemplate :radio-view-field-wt 'radio-view-field-wt)

(defmethod render-view-field ((field form-view-field) (view form-view)
                                                      widget (presentation radio-presentation) value obj
                                                      &rest args &key validation-errors field-info &allow-other-keys)
  (let* ((attribute-slot-name (if field-info
                                (attributize-view-field-name field-info)
                                (attributize-name (view-field-slot-name field))))
         (validation-error (assoc attribute-slot-name (remove-if (lambda (item) (null (car item))) validation-errors)
                                  :test #'string-equal
                                  :key (lambda (f) (and f (view-field-slot-name f)))))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated")))
         (required-indicator (form-view-field-required-indicator field))
         (show-required-indicator (and (form-view-field-required-p field)
                                       (not (form-view-field-disabled-p field obj))
                                       required-indicator)))
      (render-wt 
        :radio-view-field-wt 
        (list :field field :view view :widget widget :presentation presentation :object obj :value value)
        :field-class field-class 
        :field-label (translate (view-field-label field))
        :show-required-indicator show-required-indicator
        :required-indicator-label (when show-required-indicator 
                                    (if (eq t required-indicator)
                                      *default-required-indicator*
                                      required-indicator))
        :validation-error (and validation-error (format nil "~A" (cdr validation-error)))
        :content (capture-weblocks-output 
                   (apply #'render-view-field-value
                          value presentation
                          field view widget obj
                          args)))))

(defmethod render-view-field-value (value (presentation radio-presentation)
                                    (field form-view-field) (view form-view) widget obj
                                    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args)
           (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-radio-buttons (if field-info
                            (attributize-view-field-name field-info)
                            (attributize-name (view-field-slot-name field)))
                          (obtain-presentation-choices presentation obj)
                          :selected-value (if intermediate-value-p
                                              intermediate-value
                                              (when value
                                                (attributize-name value)))
                          :disabledp (form-view-field-disabled-p field obj)
                          :id *presentation-dom-id*)))

