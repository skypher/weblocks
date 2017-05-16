(in-package :weblocks)

(export '(checkboxes checkboxes-presentation checkboxes-parser render-checkboxes))

;;;; PRESENTATION
(defclass checkboxes-presentation (form-presentation choices-presentation-mixin)
  ())

(defmethod obtain-presentation-choices ((choices-mixin checkboxes-presentation) obj)
  (mapcar (lambda (cons) (cons (car cons)
                               (intern (string-upcase (cdr cons)) :keyword)))
          (call-next-method)))

(defun checkboxes-view-field-wt (&key field-class content field-label show-required-indicator required-indicator-label validation-error)
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

(deftemplate :checkboxes-view-field-wt 'checkboxes-view-field-wt)

(defmethod render-view-field ((field form-view-field) 
                              (view form-view)
                              widget (presentation checkboxes-presentation) value obj
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((required-indicator (form-view-field-required-indicator field))
         (attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc attribute-slot-name validation-errors
                                  :test #'string-equal
                                  :key (lambda (f) (and f (view-field-slot-name f)))))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated")))
         (show-required-indicator (and (form-view-field-required-p field)
                                       (not (form-view-field-disabled-p field obj)))))

    (render-wt 
      :checkboxes-view-field-wt 
      (list :field field :view view :widget widget :presentation presentation :object obj)
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

(defmethod render-view-field-value (value (presentation checkboxes-presentation)
                                    (field form-view-field) (view form-view) widget obj
                                    &rest args &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-checkboxes (view-field-slot-name field)
                       (obtain-presentation-choices presentation obj)
                       :selected-values (when (or value intermediate-value)
                                          (mapcar (compose (curry-after #'intern :keyword) #'string-upcase)
                                                  (if intermediate-value-p
                                                      intermediate-value
                                                      value)))
                       :disabledp (form-view-field-disabled-p field obj))))

(defmethod render-view-field-value (value (presentation checkboxes-presentation)
                                    field view widget obj &rest args
                                    &key highlight &allow-other-keys)
  (declare (ignore highlight args))
  (if (null value)
      nil
      (mapcar #'(lambda (value)
                  (with-html
                    (:span value))) value)))

(defun render-checkbox-wt (&key name id class checked-p value content disabled-p)
  (with-html-to-string
    (:input :name name :type "checkbox" :id id :class class
     :checked checked-p :disabled (if disabled-p "disabled")
     :value value)
    (str content)))

(deftemplate :checkbox-view-field-value-wt 'render-checkbox-wt)

(defun render-checkboxes (name selections &key id (class "checkbox") selected-values disabledp)
  "If `disabledp' is true, all the checkboxes are disabled."
  (dolist (val selections)
    (let ((checked-p (if (find (cdr val) selected-values :test #'equal) "checked" nil))
          (label (if (consp val) (car val) val))
          (value (if (consp val) (cdr val) val)))
      (render-wt 
        :checkbox-view-field-value-wt 
        (list :name name :id id :disabledp disabledp)
        :name (attributize-name name)
        :id id
        :class class
        :checked-p checked-p
        :value value
        :content (humanize-name label)
        :disabled-p disabledp))))

(defmethod request-parameter-for-presentation (name (presentation checkboxes-presentation))
  (declare (ignore presentation))
  (post-parameter->list name))


;;;; PARSER
(defclass checkboxes-parser (parser)
  ()
  (:documentation "A parser for checkboxes"))

(defun post-parameter->list (param)
  (loop for x in (hunchentoot:post-parameters*)
        when (or 
               (equalp (car x) param)
               (equalp (car x) (format nil "~A[]" param)))
        collect (cdr x)))

(defmethod parse-view-field-value ((parser checkboxes-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let ((result (mapcar (compose (curry-after #'intern "KEYWORD") #'string-upcase)
                        (post-parameter->list
                          (symbol-name (view-field-slot-name field))))))
      (values t result result)))


;; ; usage
;; 
;;   (dressings :present-as (checkboxes :choices (f_ '(mustard onions cheese)))
;;              :parse-as checkboxes)

