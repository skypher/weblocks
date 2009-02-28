;; (in-package :app)
 
;; (defclass init-message-presentation (input-presentation)
;;   ((message :accessor get-init-message :initarg :message :initform "Value" )))

;; (defmethod render-view-field-value  (value (presentation init-message-presentation)
;;                                     (field form-view-field) (view form-view) widget obj
;;                                     &rest args &key intermediate-values &allow-other-keys)
;;   (let (
;; 	(a-slot-name           (attributize-name (view-field-slot-name field))))
;;     (unless value (setf value (get-init-message presentation)))
;;     (multiple-value-bind (intermediate-value intermediate-value-p)
;;       (form-field-intermediate-value field intermediate-values)
;;       (with-html
;; 	(:input :type "text" :name a-slot-name :id a-slot-name ;:onfocus "if(value='AC' {value=''}"
;; 							    :value (if intermediate-value-p
;; 								       intermediate-value
;; 								       (apply #'print-view-field-value value presentation field view widget obj args))
;; 							    :maxlength (input-presentation-max-length presentation)))


;;       (send-script (format nil "
;; 			document.getElementById('~A').onfocus = function () { document.getElementById('~A').value = ''; };"
;; 			   a-slot-name a-slot-name ;(get-init-message presentation)

;; 			   )))))

;; ;; <input name="TEXTBOXNAME"  value="YOURTEXTBOXVALUE" size="20" maxlength="50" onfocus="if (value='YOURTEXTBOXVALUE') {value=''}" onblur="if (value=='') {value='YOURTEXTBOXVALUE'}" />

;; (defmethod render-view-field-value (value (presentation init-message-presentation)
;;                                     field view widget obj &rest args)
;;   (declare (ignore args))
;;   (with-html
;;     (:span value)))

 
;; (defclass init-message-parser (text-parser)
;;   ())

;; ;removed this.
;; (defmethod xs-parse-view-field-value ((parser init-message-parser) value obj
;; 				   (view form-view) (field form-view-field) &rest args)
;;   (declare (ignore args))
;;   (break (format nil "InitParser: Called with ~A and ~A" (get-init-message (view-field-presentation field)) value ))
;;   (when (get-init-message (view-field-presentation field))
;;     ;(break "inside")
;;     (if (ppcre:all-matches value (get-init-message (view-field-presentation field)))
;; 	(values t nil "") ;what is wrong with this bloody thing: code reaches here, but how do I signal an invalid parsed value?
;; 	(values t (for-initmessage-text-input-present-p value (view-field-presentation field)) "")))
;;   (values t (text-input-present-p value) value)      ) ;code's a bit screwed up, don't need this 'coz of when, but it should work.

;; (defun for-initmessage-text-input-present-p (value presentn)
;;   "Returns true if the text input is to be considered non-empty."
;;   (and (not (equal value (get-init-message presentn)))
;;        (not (string-whitespace-p (or value "")))))

;; ;; For template form, just remove the label that we usually show.
;; (defmethod render-view-field ((field form-view-field) (view templform-view)
;;                               widget (presentation init-message-presentation) value obj
;;                               &rest args &key validation-errors &allow-other-keys)
;;   (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
;;          (validation-error (assoc attribute-slot-name validation-errors
;;                                   :test #'string-equal
;;                                   :key #'view-field-slot-name))
;;          (field-class (concatenate 'string attribute-slot-name
;;                                    (when validation-error " item-not-validated"))))
;;     (with-html
;;       (:li :class field-class
;;            (:span :class "label"
;;                   (:span :class "slot-name"
;;                          (:span :class "extra" 
;; 				(str (view-field-label field)) ":&nbsp;"
;;                                  (when (form-view-field-required-p field)
;;                                    (htm (:em :class "required-slot" "(required)&nbsp;")))
;;                                 )))
;;            (apply #'render-view-field-value
;;                   value presentation
;;                   field view widget obj
;;                   args)
;;            (when validation-error
;;              (htm (:p :class "validation-error"
;;                       (:em
;;                        (:span :class "validation-error-heading" "Error:&nbsp;")
;;                        (str (format nil "~A" (cdr validation-error)))))))))
;; ))
