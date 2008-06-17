
(in-package :weblocks)

(export '(checkboxes checkboxes-presentation checkboxes-parser))

;;; Checkboxes
(defclass checkboxes-presentation (form-presentation choices-presentation-mixin)
  ())

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
                                (str (view-field-label field)) ":&nbsp;")))
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
                          :selected-values (if intermediate-value-p
                                             intermediate-value
                                             (when value
                                               (mapcar #'attributize-name value))))))

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
          (value (if (consp val) (car val) val)))
    (with-html
      (:input :name name :type "checkbox" :id id :class class
             :checked checked-p :value value (str (humanize-name value)))))))

(defun render-checkboxes2 (name selections &key id (class "checkbox") selected-values)
  "Renders a group of checkboxes.

'name' - name of checkboxes.
'selections' - a list of selections. May be an association list, in
which case its car is used to dispaly selection text, and cdr is used
for the value.
'id' - id of a label that holds the checkboxes.
'class' - class of the label and of checkboxes.
'selected-value' - list of selected checkbox values."
  (loop for i in (list->assoc selections)
        for j from 1
        with count = (length selections)
        for label-class = (cond
                            ((eq j 1) (concatenate 'string class " first"))
                            ((eq j count) (concatenate 'string class " last"))
                            (t class))
        do (progn
             (with-html
               (:label :id id :class label-class
                       (let ((cb-name (concatenate 'string (attributize-name name) "-" (attributize-name j))))
                         (htm (:input :name cb-name :type "checkbox" :class "checkbox"
                                      :value (cdr i)
                                      (if (find (cdr i) selected-values :test 'equal) :checked "checked"))))
                       (:span (str (format nil "~A&nbsp;" (car i))))))))
    (with-html (:input :name (attributize-name name) :type "hidden")))

(defclass checkboxes-parser (parser)
  ()
  (:documentation "A parser for checkboxes."))

(defun post-parameter->list (param)
  (let ((result nil))
    (mapcar (lambda (x)
              ;(format t "~S~%" x)
              (when (equalp (car x) param)
                (push (cdr x) result)))
            (hunchentoot:post-parameters))
    result))

(defmethod parse-view-field-value ((parser checkboxes-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let ((result (mapcar (lambda (s)
                          (intern (string-upcase s) "KEYWORD"))
                        (post-parameter->list (symbol-name (view-field-slot-name field))))))
      (values t result result)))

