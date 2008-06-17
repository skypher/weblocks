;;; NB:
;;; needs request-parameter-for-presentation.diff applied to Weblocks

(in-package :weblocks)

(export '(dual-password-presentation dual-password-parser))

(defclass dual-password-presentation (text-presentation input-presentation)
  ((max-length :initform *max-password-length*))
  (:documentation "A presentation for passwords."))

(defun render-dual-password-fields (name value maxlength)
  (render-password (format nil "~A-weblocks-1" name) (or value "") :maxlength maxlength)
  (with-html (:br))
  (render-password (format nil "~A-weblocks-2" name) (or value "") :maxlength maxlength
                   :class "password confirm"))

(defmethod render-view-field-value (value (presentation dual-password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-dual-password-fields (view-field-slot-name field) (if intermediate-value-p
                                                                intermediate-value
                                                                value)
                                 (input-presentation-max-length presentation))))

(defmethod render-view-field-value ((value null) (presentation dual-password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-dual-password-fields (view-field-slot-name field) (if intermediate-value-p
                                                                intermediate-value
                                                                value)
                                 (input-presentation-max-length presentation))))

(defmethod request-parameter-for-presentation (name (presentation dual-password-presentation))
  (if (equal (call-next-method (format nil "~A-weblocks-1" name) presentation)
             (call-next-method (format nil "~A-weblocks-2" name) presentation))
    (call-next-method (format nil "~A-weblocks-1" name) presentation)
    ""))

(defclass dual-password-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser dual-password-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let* ((val1 (hunchentoot:parameter (format nil "~A-1" (string-downcase (view-field-slot-name field)))))
         (val2 (hunchentoot:parameter (format nil "~A-2" (string-downcase (view-field-slot-name field)))))
         (present-p (and val1 val2 (text-input-present-p val1) (text-input-present-p val2)))
         (valid-p (and present-p (equal val1 val2)))
         ; XXX min-length < length < max-length
         )
      (values valid-p present-p val1)))

