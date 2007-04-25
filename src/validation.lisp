
(in-package :weblocks)

(export '(slot-in-request-empty-p declare-validators decl-validate))

(defparameter *slot-validators-indicator* 'validators
  "An indicator that stores validators in a slot's plist.")

;;; Check if slot is empty
(defgeneric slot-in-request-empty-p (slot-type request-slot-value)
  (:documentation
   "Returns true if the slot value in request is to be considered
'empty', i.e. all whitespace, 'Select State' dropdown value, etc.

Default implementation returns true when all characters of the request
value are a whitespace."))

(defmethod slot-in-request-empty-p (slot-type request-slot-value)
  (string-whitespace-p request-slot-value))

;;; Check if slot is required
(defun slot-value-required-p (slot)
  "Returns true if 'slot' is declared to have an existance validator,
nil otherwise."
  (find :required
	(get (slot-definition-name slot) *slot-validators-indicator*)))

;;; Validate slot
(defun validate-slot-from-request (obj slot parsed-request-slot-value)
  "Call all validators recorded on the symbol that names the slot."
  (mapc (lambda (validator)
	  (apply-validator validator obj (cdr slot) parsed-request-slot-value))
	(get (slot-definition-name (car slot)) *slot-validators-indicator*)))

;;; Validation condition
(define-condition form-validation-error (error)
  ((slot-name :accessor validation-error-slot :initarg :slot-name)))

;;; Applying validators
(defgeneric apply-validator (validator obj slot-name parsed-value)
  (:documentation
   "Applies an appropriate validator."))

;;; Required validator
(define-condition required-validation-error (form-validation-error) ()
  (:report (lambda (condition stream)
	     (format stream "~S is a required field." (humanize-name (validation-error-slot condition))))))

(defmethod apply-validator ((validator (eql :required)) obj slot-name parsed-value)
  (unless parsed-value
    (error (make-condition 'required-validation-error
			   :slot-name slot-name))))

;;; Not twenty-four validator
(define-condition not-twenty-four-validation-error (form-validation-error) ()
  (:report (lambda (condition stream)
	     (format stream "~S must not equal 24." (humanize-name (validation-error-slot condition))))))

(defmethod apply-validator ((validator (eql :not-twenty-four)) obj slot-name parsed-value)
  (unless (/= parsed-value 24)
    (error (make-condition 'not-twenty-four-validation-error
			   :slot-name slot-name))))

;;; Declaring validators
(defmacro decl-validate (class-name &rest args)
  "Provides a more comfortable interface to 'declare-validators'.

Ex:
\(decl-validate employee
   ssn (:unique :required)
   first-name (:required))"
  `(declare-validators ',class-name ',args))

(defun declare-validators (class-name args)
  "Specifies validators for slots of a particular class.

Ex:
\(decl-validate 'employee
   '(ssn (:unique :required)
     first-name (:required))

'args' is a list that contains pairs of elements. Each odd element is
expected to be the name of the slot, while the element that follows
is expected to be a list of validators.

See macro 'decl-validate' for a more comfortable syntax."
  (mapc (lambda (slot-name)	
	  (let* ((validation-slot (member slot-name args))
		 (validators (when validation-slot
			       (cadr validation-slot))))
	    (when validators
	      (setf (get slot-name *slot-validators-indicator*) validators))))
    (slot-names class-name)))
