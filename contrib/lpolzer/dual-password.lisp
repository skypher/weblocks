;;; NB:
;;; needs request-parameter-for-presentation.diff applied to Weblocks

(in-package :weblocks)

(export '(dual-password-presentation dual-password-parser))

(defclass dual-password-presentation (text-presentation input-presentation)
  ((max-length :initform *max-password-length*)
   (clear-text-p :accessor dual-password-presentation-clear-text-p
                 :initarg :clear-text-p
                 :initform nil))
  (:documentation "A presentation for passwords."))

(defun render-dual-password-fields (name value maxlength &optional clear-text-p)
  (let* ((basename (format nil "~A-weblocks" (attributize-name name)))
         (name1 (format nil "~A-1" basename))
         (name2 (format nil "~A-2" basename))
         (status-name (format nil "~A-status" basename))
         (render-fn (if clear-text-p (lambda (name value &rest kwargs)
                                       (apply #'render-input-field "text" name value kwargs))
                      #'render-password)))
    (declare (ignorable status-name))
    (with-html
      (:div
        (funcall render-fn name1 (or value "")
                 :id name1 :maxlength maxlength
                 :style (when value "border:1px solid green"))))
    (with-html 
      (:div
        (funcall render-fn name2 (or value "")
                 :id name2 :maxlength maxlength
                 :class "password confirm"
                 :style (when value "border:1px solid green"))))
    (send-script
      (concatenate 'string
        "function checkPasswordFields() {
          var f1 = $('" name1 "'),
              f2 = $('" name2 "'),
              color;

          if (f1.value == f2.value
              && f1.value != '')
            color = 'green';
          else
            color = 'red';

          f1.style.borderColor = color;
          f2.style.borderColor = color;
        }"
        "
         $('" name1 "').observe('keyup', checkPasswordFields);
         $('" name2 "').observe('keyup', checkPasswordFields);
        "
    ))))

(defmethod render-view-field-value (value (presentation dual-password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignorable args widget obj))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-dual-password-fields (view-field-slot-name field) (if intermediate-value-p
                                                                intermediate-value
                                                                value)
                                 (input-presentation-max-length presentation)
                                 (dual-password-presentation-clear-text-p presentation))))

(defmethod render-view-field-value ((value null) (presentation dual-password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignorable args widget obj))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-dual-password-fields (view-field-slot-name field) (if intermediate-value-p
                                                                intermediate-value
                                                                value)
                                 (input-presentation-max-length presentation)
                                 (dual-password-presentation-clear-text-p presentation))))

(defmethod request-parameter-for-presentation (name (presentation dual-password-presentation))
  (if (equal (call-next-method (format nil "~A-weblocks-1" name) presentation)
             (call-next-method (format nil "~A-weblocks-2" name) presentation))
    (call-next-method (format nil "~A-weblocks-1" name) presentation)
    ""))

(defclass dual-password-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser dual-password-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignorable args obj value))
  (let* ((val1 (hunchentoot:parameter (format nil "~A-weblocks-1" (attributize-name (view-field-slot-name field)))))
         (val2 (hunchentoot:parameter (format nil "~A-weblocks-2" (attributize-name (view-field-slot-name field)))))
         (present-p (and val1 val2 (text-input-present-p val1) (text-input-present-p val2)))
         (valid-p (and present-p (equal val1 val2)))
         ; XXX min-length < length < max-length
         )
    (values valid-p present-p val1)))

