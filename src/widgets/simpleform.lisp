
(in-package :weblocks)

;; TODO: automated tests for this widget

(export '(simpleform simpleform-success-widget simpleform-submit-label
          make-simpleform render-dataform-summary))

(defwidget simpleform (quickform)
  ((success-widget :accessor simpleform-success-widget
                   :initarg :success-widget
                   :initform (make-widget "Your data was correct.")
                   :documentation "An object that will be rendered on success.")
   (submit-label :type string
                 :accessor simpleform-submit-label
                 :initarg :submit-label
                 :initform "Submit"
                 :documentation "Label for the submit button.")
   (summary-view :accessor simpleform-summary-view
                 :initform nil
                 :initarg :summary-view
                 :documentation "View for confirmation step.
                 If null, skip summary step."))
  (:default-initargs :allow-close-p nil :ui-state :form :buttons '(:submit))
  (:documentation "A form widget to handle very simple forms without frills.
It sports a plain style and displays a custom widget when
validation passes."))

(defmethod dataform-on-success ((widget simpleform))
  (when (eq (slot-value widget 'ui-state) :success)
    (slot-value widget 'on-success)))

(defwidget simpleform-success-widget ()
  ((data :accessor data :initarg :data)))

(defmethod initialize-instance :after ((widget simpleform) &rest args)
  (declare (ignore args))
  (unless (dataform-data widget)
    (setf (dataform-data widget)
          (make-instance (class-from-view (dataform-form-view widget))))))

(defun make-simpleform (view &rest args)
  "Create a simple form.

Example:

(defview password-reminder-view (:type form :persistp nil)
  (username :requiredp t))

(defun make-password-reminder ()
  (make-simpleform 'password-reminder-view
                   :submit-label \"Request password\"
                   :satisfies (lambda (widget data)
                                (if (find-user (slot-value data 'username))
                                  t
                                  (values nil `((username . \"User not found.\")))))
                   :on-success (lambda (widget) (let ((user (find-user
                                                                (slot-value (dataform-data widget) 'username))))
                                                  (mail (email user)
                                                        \"Your password\"
                                                        (format nil \"Your password is ~S\"
                                                                (password user)))))
                   :success-widget \"Your password has been sent to your email address.\"))
"
  (apply #'make-instance 'simpleform
                         :form-view view
                         args))

(defmethod render-form-view-buttons ((view form-view) obj (widget simpleform) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "submit"
      (weblocks:render-button *submit-control-name*
                              :value (simpleform-submit-label widget)))))

(defmethod with-view-header ((view form-view) obj (widget simpleform) body-fn &rest args &key
                             (method (form-view-default-method view))
                             (action (form-view-default-action view))
                             (fields-prefix-fn (view-fields-default-prefix-fn view))
                             (fields-suffix-fn (view-fields-default-suffix-fn view))
                             validation-errors
                             &allow-other-keys)
  (let ((header-class (format nil "view form ~A"
                              (attributize-name (object-class-name obj)))))
    (when (>= (count-view-fields view)
              (form-view-error-summary-threshold view))
      (setf header-class (concatenate 'string header-class " long-form")))
    (with-html-form (method action :class header-class)
      (render-validation-summary view obj widget validation-errors)
      (safe-apply fields-prefix-fn view obj args)
      (:ul (apply body-fn view obj args))
      (safe-apply fields-suffix-fn view obj args)
      (apply #'render-form-view-buttons view obj widget args))))

(defmethod render-validation-summary ((view form-view) obj (widget simpleform) errors)
    (declare (ignore view obj))
    (call-next-method)#+off
    (when errors
      (with-html
        (:div :class "validation-errors-summary"
           (:h2 :class "error-count"
             "Some fields are not filled in correctly."
             (:br)
             "Please correct them and try again.")))))

(defmethod render-dataform ((widget simpleform) data &rest args)
  (flet ((render-success-widget ()
           (let ((widget (simpleform-success-widget widget)))
             (if (functionp widget)
               (funcall widget data)
               (render-widget (etypecase widget
                                (string widget)
                                (widget widget)
                                (symbol (make-instance widget :data data))))))))
    (ecase (slot-value widget 'ui-state)
      (:form
        (apply #'render-dataform-form widget data (dataform-form-view widget) args))
      ((:summary :data)
       (if (simpleform-summary-view widget) 
         (apply #'render-dataform-summary widget data (simpleform-summary-view widget) args)
	 (progn
	   (setf (slot-value widget 'ui-state) :success)
	   (safe-funcall (dataform-on-success widget) widget)
	   (render-success-widget))))
      (:success
        (render-success-widget)))))

(defmethod render-dataform-summary ((widget simpleform) data view &rest args)
  (apply #'render-object-view data view
         :fields-suffix-fn (lambda (&rest args)
                             (declare (ignore args))
                             (with-html
                               (:div :class "back"
                                     (render-link
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         (setf (slot-value widget 'ui-state) :form))
                                       "Back"
                                       :class "button modify"))
                               (:div :class "next"
                                     (render-link
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         (setf (slot-value widget 'ui-state) :success)
                                         (safe-funcall (dataform-on-success widget) widget))
                                       "Confirm"
                                       :class "button modify"))))
         :widget widget
         args))

(defmethod render-view-field ((field form-view-field) (view form-view)
                              (widget simpleform) presentation value obj 
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc field validation-errors))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
           (:label :class (attributize-presentation
                           (view-field-presentation field))
                   (:span :class "slot-name"
                          (:span :class "extra" :style (if validation-error "color:red" nil)
                                 (str (view-field-label field))
                                 (if (form-view-field-required-p field)
                                   (htm (:em :class "required-slot" (:sup "*")))
                                   ":&nbsp;")))
                   (:div :class "maybe-error" :style (if validation-error "border:1px solid red" nil)
                     (apply #'render-view-field-value
                            value presentation
                            field view widget obj
                            args))
                   #+(or)
                   (when validation-error
                     (htm (:p :class "validation-error"
                              (:em
                               (:span :class "validation-error-heading" "Error:&nbsp;")
                               (str (format nil "~A" (cdr validation-error))))))))))))

