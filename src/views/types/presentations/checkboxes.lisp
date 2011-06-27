(in-package :weblocks)

(export '(checkboxes checkboxes-presentation checkboxes-parser render-checkboxes))

;;;; PRESENTATION
(defclass checkboxes-presentation (form-presentation choices-presentation-mixin)
  ())

(defmethod obtain-presentation-choices ((choices-mixin checkboxes-presentation) obj)
  (mapcar (lambda (cons) (cons (car cons)
                               (intern (string-upcase (cdr cons)) :keyword)))
          (call-next-method)))

(defmethod render-view-field ((field form-view-field) (view form-view)
                              widget (presentation checkboxes-presentation) value obj
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc attribute-slot-name validation-errors
                                  :test #'string-equal
                                  :key #'view-field-slot-name))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
           (:span :class "label"
                  (:span :class "slot-name"
                         (:span :class "extra"
                                (str (view-field-label field)) ":&nbsp;"
                                (when (form-view-field-required-p field)
                                  (htm (:em :class "required-slot" "(required)&nbsp;"))))))
           (apply #'render-view-field-value
                  value presentation
                  field view widget obj
                  args)
           (when validation-error
             (htm (:p :class "validation-error"
                      (:em
                       (:span :class "validation-error-heading" "Error:&nbsp;")
                       (str (format nil "~A" (cdr validation-error)))))))))))

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
                                                      value))))))

(defmethod render-view-field-value (value (presentation checkboxes-presentation)
                                    field view widget obj &rest args
                                    &key highlight &allow-other-keys)
  (declare (ignore highlight args))
  (if (null value)
      nil
      (mapcar #'(lambda (value)
                  (with-html
                    (:span value))) value)))

(defun render-checkboxes (name selections &key id (class "checkbox") selected-values)
  (dolist (val selections)
    (let ((checked-p (if (find (cdr val) selected-values :test #'equal) "checked" nil))
          (label (if (consp val) (car val) val))
          (value (if (consp val) (cdr val) val)))
    (with-html
      (:input :name (attributize-name name) :type "checkbox" :id id :class class
	      :checked checked-p :value value (str (humanize-name label)))))))

(defmethod request-parameter-for-presentation (name (presentation checkboxes-presentation))
  (declare (ignore presentation))
  (post-parameter->list name))


;;;; PARSER
(defclass checkboxes-parser (parser)
  ()
  (:documentation "A parser for checkboxes"))

(defun post-parameter->list (param)
  (loop for x in (hunchentoot:post-parameters*)
        when (equalp (car x) param)
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

