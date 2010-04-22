
(in-package :weblocks)

(export '(wizard

          wizard-data
          wizard-cursor
          wizard-status-type
          wizard-current-widget
          wizard-on-complete

          wizard-current-step
          wizard-total-steps
          wizard-processed-data
          wizard-current-datum
          wizard-remaining-data
          wizard-proceed
          wizard-recede
          wizard-render-no-data
          wizard-render-status
          wizard-render-summary-page
          wizard-render-step

          wizard-dataform
          wizard-dataform-wizard

          wizard-create-widget
          wizard-update-current-widget

          wizard-form-view))

;;;
;;; TODO
;;; * decouple the wizard from its data's model!
;;; * tests
;;;

(defwidget wizard ()
  ((data :type list
         :accessor wizard-data
         :initarg :data
         :initform nil
         :documentation "The list of data held by this wizard. Elements will be
         presented in an ordered fashion with one element per step.")
   (cursor :type integer
           :accessor wizard-cursor
           :initarg :cursor
           :initform 0
           :documentation "Offset of the current position in the data list.
           Not to be used directly; use the WIZARD-CURRENT-STEP function instead.")
   (status-type :type symbol
                :accessor wizard-status-type
                :initarg :status-type
                :initform :simple
                :documentation "The type of status display rendered before the form.
                Currently only :SIMPLE is supported.")
   (current-widget :type widget
                   :accessor wizard-current-widget
                   :initarg :current-widget
                   :documentation "The widget currently displayed by the wizard.")
   (on-complete :type (or function symbol null)
                :accessor wizard-on-complete
                :initarg :on-complete
                :initform nil
                :documentation "A function designator (or NIL) holding the function
                that is to be called after the completion of the wizard."))
  (:documentation "A widget that displays a series of data objects in separate steps.
                  The default implementation renders a dataform for each step.

                  Specialize WIZARD-FORM-VIEW to specify views for the data."))

(defmethod initialize-instance :after ((inst wizard) &rest initargs)
  "Initialize the wizard's first page."
  (wizard-update-current-widget inst))

(defmethod (setf wizard-cursor) :after (value (wizard wizard))
  "Updates the current widget when the data item changes."
  (declare (ignore value))
  (wizard-update-current-widget wizard))

(defmethod wizard-current-step ((wizard wizard))
  "Returns the current step of the wizard, starting from 1."
  (1+ (wizard-cursor wizard)))

(defmethod wizard-total-steps ((wizard wizard))
  "Returns the total steps of the wizard, which is the number
  of data items plus one for the final confirmation page by
  default."
  (1+ (length (wizard-data wizard))))

(defmethod wizard-processed-data ((wizard wizard))
  "Returns the data already processed so far by the wizard.

  This is not a guarantee that the other items haven't been shown
  to the user but rather an indicator of which data contents can currently
  be considered valid."
  (safe-subseq (wizard-data wizard) 0 (wizard-cursor wizard)))

(defmethod wizard-current-datum ((wizard wizard))
  "Returns the current data object shows to the user."
  (car (wizard-remaining-data wizard)))

(defmethod wizard-remaining-data ((wizard wizard))
  "Returns the data objects still remaining to be processed.

  The notes from WIZARD-PROCESSED-DATA hold here, too."
  (nthcdr (wizard-cursor wizard) (wizard-data wizard)))

(defmethod wizard-proceed ((wizard wizard))
  "Proceed to the next step. You mustn't go beyond the final step."
  (prog1
    (incf (wizard-cursor wizard))
    (assert (<= (wizard-current-step wizard) (wizard-total-steps wizard)))))

(defmethod wizard-recede ((wizard wizard))
  "Recede to the previous step. You mustn't go beyond the first step
  (i.e. to step zero)."
  (prog1
    (decf (wizard-cursor wizard))
    (assert (> (wizard-current-step wizard) 0))))

(defmethod wizard-render-no-data ((wizard wizard))
  "Called to render a message indicating that this wizard does
  not have any data to operate on."
  (with-html
    (:p "This Wizard does not have any data to manipulate")))

(defmethod wizard-render-status ((wizard wizard) (type (eql :simple)))
  "Render the current status of the wizard as a simple text string
  displaying the current step and the number of total steps."
  (with-html
    (:div :class "state"
      (esc (format nil "Step ~D/~D" (wizard-current-step wizard)
                                    (wizard-total-steps wizard))))))

(defmethod wizard-form-view ((wizard wizard) data (step integer))
  "Return the appropriate form view for a combination of wizard, data and step.
  The default implementation passes this form view to the dataform."
  nil)

(defmethod wizard-render-summary-page ((wizard wizard))
  "Render the final summary/confirmation page of the wizard."
  (with-html
    (:p "You have completed all steps.")
    (:p :class "wizard-summary-back"
      (render-link (f_% (wizard-recede wizard)) "Back"))
    (:p :class "wizard-summary-confirm"
      (render-link (f_% (safe-funcall (wizard-on-complete wizard))) "Confirm"))))

(defmethod wizard-render-step ((wizard wizard) (step integer) data)
  "Render a specific step. The default implementation either
  renders the final confirmation page (if the current step is the last step)
  or the current widget."
  (if (eql step (wizard-total-steps wizard))
    (wizard-render-summary-page wizard)
    (render-widget (wizard-current-widget wizard))))

(defwidget wizard-dataform (dataform)
  ((wizard :type wizard
           :accessor wizard-dataform-wizard
           :initarg :wizard
           :documentation "The wizard owning the dataform. You better set this
           at initialization time so the dataform can decide what buttons to show."))
  (:default-initargs :ui-state :form
                     :allow-close-p nil)
  (:documentation "A dataform slightly customized for the wizard's purposes."))

(defmethod initialize-instance :after ((form wizard-dataform) &rest initargs)
  (let ((wizard (wizard-dataform-wizard form)))
    (setf (dataform-on-cancel form) (f_% (wizard-recede wizard)))
    (setf (dataform-on-success form) (f_% (wizard-proceed wizard)))))

(defmethod render-form-view-buttons ((view form-view) obj (widget wizard-dataform) &rest args)
  "Render the buttons to recede and proceed with the wizard."
  (declare (ignore obj args))
  (flet ((find-button (name)
           (ensure-list
             (find name (form-view-buttons view)
                   :key (lambda (item)
                          (car (ensure-list item)))))))
    (with-html
      (:div :class "submit"
            (unless (eql (wizard-current-step (wizard-dataform-wizard widget)) 1)
              (let ((cancel (or (find-button :back) '(:back))))
                (render-button *cancel-control-name*
                               :class "submit cancel"
                               :value (or (cdr cancel)
                                          (humanize-name (car cancel))))))
            (when (wizard-remaining-data (wizard-dataform-wizard widget))
              (let ((submit (or (find-button :next) '(:next))))
                (render-button *submit-control-name*
                               :value (or (cdr submit)
                                          (humanize-name (car submit))))))))))

(defmethod wizard-create-widget ((wizard wizard) (step integer) data)
  "Create a fresh widget for the specified STEP and DATA.
  Default implementation creates a WIZARD-DATAFORM."
  (make-instance 'wizard-dataform :wizard wizard :data data
                 :form-view (wizard-form-view wizard data step)))

(defmethod wizard-update-current-widget ((wizard wizard))
  "Instantiate the widget belonging to the current step
  and assign it to the wizard."
  (when (wizard-current-datum wizard) ;; this is just a kludge as we don't need the
                                      ;; widget in the final step anyway
    (setf (wizard-current-widget wizard)
          (wizard-create-widget wizard (wizard-current-step wizard) (wizard-current-datum wizard)))))

(defmethod render-widget-body ((wizard wizard) &rest args)
  "Render the wizard."
  (cond
    ((null (wizard-data wizard))
     (wizard-render-no-data wizard))
    (t
     (wizard-render-status wizard (wizard-status-type wizard))
     (wizard-render-step wizard (wizard-current-step wizard) (wizard-current-datum wizard)))))

