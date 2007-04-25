
(in-package :weblocks)

(export '(form-validation-error apply-validator
	  required-validation-error declare-validators decl-validate))

(defparameter *slot-validators-indicator* 'validators
  "An indicator that stores validators in a slot's plist.")

(defun slot-value-required-p (slot)
  "Returns true if 'slot' is declared to have an existance validator,
nil otherwise. See 'decl-validate' for more detauls."
  (find :required
	(get (slot-definition-name slot) *slot-validators-indicator*)))

(defun validate-slot-from-request (obj slot parsed-request-slot-value)
  "Call all validators recorded on the symbol that names the slot. If
any of the validators fails, the appropriate condition is propagated
up the call stack."
  (mapc (lambda (validator)
	  (apply-validator validator obj (cdr slot) parsed-request-slot-value))
	(get (slot-definition-name (car slot)) *slot-validators-indicator*)))

(define-condition form-validation-error (error)
  ((slot-name :accessor validation-error-slot :initarg :slot-name))
  (:documentation "An error condition at the root of all conditions
  caused by problems with parsing form values into objects."))

(defgeneric apply-validator (validator obj slot-name parsed-value)
  (:documentation
   "Validates 'parsed-value' with a validator specified by
'validator'. This generic function is called by
'validate-slot-from-request'. There are default specializations for
various validators. Specialize the methods on 'validator' to add more
custom validators."))

;;; Required validator
(define-condition required-validation-error (form-validation-error) ()
  (:report (lambda (condition stream)
	     (format stream "~S is a required field." (humanize-name (validation-error-slot condition)))))
  (:documentation "A condition signalled if a required field is
  missing."))

(defmethod apply-validator ((validator (eql :required)) obj slot-name parsed-value)
  (unless parsed-value
    (error (make-condition 'required-validation-error
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

The validators are mapped to those defined by 'apply-validator'
generic function.

See macro 'decl-validate' for a more comfortable syntax."
  (mapc (lambda (slot-name)	
	  (let* ((validation-slot (member slot-name args))
		 (validators (when validation-slot
			       (cadr validation-slot))))
	    (when validators
	      (setf (get slot-name *slot-validators-indicator*) validators))))
    (slot-names class-name)))
