
(in-package :weblocks)

(export '(validate-object-form-view validate-form-view-field))

(let ((keyword-package (find-package :keyword)))
  (defun symbol-to-keyword (symbol)
    (intern (symbol-name symbol) keyword-package)))

(defun run-view-validators (validators object fields-values)
  "Applies all of the functions in the 'validators' list to
the 'fields-values' arglist. Returns either t if all functions
returned t, or nil and an association list of nils and error
messages if any of the functions failed."
  (let ((validates t)
        errors)
    (dolist (validator-function validators)
      (multiple-value-bind (validatesp error)
          (funcall validator-function object fields-values)
        (unless validatesp
          (setf validates nil)
          (push-end (cons nil error) errors))))
    (if validates
        t
        (values nil errors))))

(defgeneric validate-object-form-view (object view parsed-values)
  (:documentation "Called by the framework during form deserialization
to validate a form view. Default implementation validates each field
by calling 'validate-form-view-field', then if individual field validation
succeeds, applies view-global validators.

If this function succeeds validating the form it returns
true. Otherwise returns nil as the first value, and an association
list of either fields and errors or nils and errors (for non-field-related
validation errors) as the second value.

'object' - the object the form is being deserialized into.
'view' - form view object being deserialized.
'parsed-values' - an association list of field-info structures and
parsed values.")
  (:method (object (view form-view) parsed-values)
    (let ((validates t)
          errors
          fields-values)
      (dolist (info-value-pair parsed-values)
        (destructuring-bind (field-info . parsed-value)
            info-value-pair
          (multiple-value-bind (validatesp error)
              (let ((field (field-info-field field-info))
                    (object (field-info-object field-info)))
                (push (cons (view-field-slot-name field) parsed-value) fields-values)
                (validate-form-view-field (view-field-slot-name field)
                                          object field view parsed-value))
            (unless validatesp
              (setf validates nil)
              (push-end (cons (field-info-field field-info) error) errors)))))
      ;; We proceed to view-level validation only if individual fields were
      ;; successfully validated.
      (if validates
          (if (form-view-satisfies view)
              (run-view-validators (form-view-satisfies view) object fields-values)
              t)
          (values nil errors)))))

(defgeneric validate-form-view-field (slot-name object field view parsed-value)
  (:documentation "Called by 'validate-object-form-view' during form
deserialization to validate a form view field. Default implementation
ensures that the parsed value satisfies any type requirements declared
on the slot (unless the field has a custom writer, in which case this
condition is waived), and functions declared on the 'satisfies' slot
of the form-view-field.

If the field is validated, the function returns true. Otherwise,
returns nil as the first value and an error message as the second
value. Default implementation obtains the error message by calling
'parser-error-message' on the field's parser.

'slot-name' - the name of the slot being validated.
'object' - object being validated.
'field' - form-view-field being validated.
'view' - form-view being validated.
'parsed-value' - the value parsed from the request.")
  (:method (slot-name object (field form-view-field) (view form-view) parsed-value)
    (let* ((slot-esd (find-slot-esd (class-of object) slot-name))
           (slot-type (if slot-esd
                          (slot-definition-type slot-esd)
                          t))
           (validators (remove nil
                               (cons (unless (slot-boundp field 'writer)
                                       (lambda (value)
                                         (or 
                                           (typep value slot-type)
                                           (null value))))
                                     (ensure-list (form-view-field-satisfies field))))))
      (loop
         for validator in validators
         do (multiple-value-bind (result error) (funcall validator parsed-value)
              (unless result
                (return (values nil (or error
                                        (parser-error-message (form-view-field-parser field)))))))
         finally (return t)))))
