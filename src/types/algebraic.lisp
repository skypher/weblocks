
(in-package :weblocks)

;;; Utilities
(eval-when (:compile-toplevel :load-toplevel)
  (defun count-to (n)
    "Creates a list with elements from zero to n-1."
    (loop for j from 0 to (- n 1)
       collect j)))

(defun cdr-safe (obj)
  "Returns the cdr of 'obj' if it's a cons. Otherwise returns nil."
  (and (consp obj) (cdr obj)))

(defun object-slot-names (obj)
  "Returns a list of slot names for object 'obj'."
  (mapcar #'slot-definition-name (class-slots (class-of obj))))

(defun mapslots (fn obj)
  "Maps 'fn' over slot names defined in object obj and concatenates
the results."
  (mapcar fn (object-slot-names obj)))

;;; Algebraic class
(defclass algebraic () ())

;;; Printer
(defgeneric print-algebraic-object (object stream &key firstp)
  (:documentation
   "Called by 'print-object' method specialized on algebraic
types. Prints the algebraic type in a list notation."))

(defmethod print-algebraic-object (object stream &key firstp)
  (prin1 object stream))

(defmethod print-algebraic-object ((object algebraic) stream &key firstp)
  (let ((ctr-args (object-slot-names object)))
    (when (and ctr-args (not firstp))
      (format stream "("))
    (princ (type-of object) stream)
    (unwind-protect
	 (mapcar (lambda (slot-name)
		   (format stream " ")
		   (print-algebraic-object (slot-value object slot-name) stream))
		 ctr-args)
      (when (and ctr-args (not firstp))
	(princ ")" stream)))))

(defmethod print-object ((object algebraic) stream)
  (when *print-readably*
    (error (make-instance 'print-not-readable :objectinitialization object)))
  (princ "#A(" stream)
  (unwind-protect
       (print-algebraic-object object stream :firstp t)
    (princ ")" stream)))

;;; Pattern matching
(eval-when (:compile-toplevel :load-toplevel)
  (defun define-algebraic-matcher (constructor-name arg-count)
    "Defines a customized pattern matcher for an algebraic data type
whose CLOS equivalent is 'algebraic-class'. The pattern matcher can
then be used with other fare matchers."
    (if (> arg-count 0)
	`(define-constructor-matcher
	     ,constructor-name
	     ,arg-count
	   (lambda (form)
	     (m%when (typep form ',constructor-name)
	       (apply #'values (mapslots (lambda (slot-name)
					   (slot-value form slot-name))
					 form)))))
	`(define-symbol-matcher ,constructor-name
	     `(lambda (form)
		(m%when (typep form (type-of ,',constructor-name))
		  (fare-matcher::m%success)))))))

;;; The compiler
(defmacro defalgebraic (typename &rest constructors)
  "Compiles to CL code that has rough equivalence to ML-style
algebraic data types. The name of the type is acquired from 'typename'
argument. The rest of the arguments may be either symbols or lists. A
symbol is interpreted to be a constructor name with no arguments. A
list is expected to have its car as a constructor name, and its cdr to
be a list of type specifiers for constructor arguments. A star may be
used to indicate type t.

Ex:
\(defalgebraic maybe nothing (just *))"
  `(progn
     ;; Define the main class
     (defclass ,typename (algebraic) ())
     (finalize-inheritance (find-class ',typename))
     ;; Loop through constructors
     ,@(mapcar (lambda (constructor)
		 (let* ((name (if (consp constructor)
				  (car constructor)
				  constructor))
			(arg-types (mapcar (lambda (arg-type)
					     (if (eq arg-type '*) t arg-type))
					   (cdr-safe constructor)))
			(slot-names (mapcar (lambda (x)
					      (declare (ignore x))
					      (gensym)) arg-types))
			(arg-names (mapcar (lambda (i)
					     (intern (format nil "A~A" i)))
					   (count-to (length slot-names)))))
		   `(progn
		      ;; Define constructor class
		      (defclass ,name (,typename)
			;; Convert constructor arguments to slots
			,(mapcar (lambda (argument-type argument-name)
				   `(,argument-name :type ,argument-type))
				 arg-types slot-names))
		      (finalize-inheritance (find-class ',name))
		      ;; Define construction function
		      (defun ,name ,arg-names
			;; Create instance
			(let ((instance (make-instance ',name)))
			  ,@(mapcar (lambda (slot-name arg-name slot-type)
				      `(progn
					; Check that types are correct
					 (check-type ,arg-name ,slot-type)
					; Set the slot values
					 (setf (slot-value instance ',slot-name)
					       ,arg-name)))
				    slot-names arg-names arg-types)
			  instance))
		      ,(when (zerop (length arg-names))
			     `(progn
				(defparameter ,name (,name))
				(fmakunbound ',name)))
		      ;; Define a matcher for this constructor
		      (eval-when (:compile-toplevel :load-toplevel)
			,(define-algebraic-matcher name (length slot-names))))))
	       constructors)
     ',typename))

;;; Common types
(defalgebraic maybe (just *) nothing)
(defalgebraic @list @nil (@cons * @list))

;;; List syntactic sugar
(defun print-@list (object stream)
  (ematch object
	  (@nil nil)
	  ((@cons a b) (princ a stream)
	   (unless (eql b @nil)
	     (princ " " stream))
	   (print-@list b stream))))

(defmethod print-algebraic-object ((object @list) stream &key firstp)
  (princ "[" stream)
  (unwind-protect
       (print-@list object stream)
    (princ "]" stream)))

(defmethod print-object ((object @list) stream)
  (when *print-readably*
    (error (make-instance 'print-not-readable :objectinitialization object)))
  (princ "#A[" stream)
  (unwind-protect
       (print-@list object stream)
    (princ "]" stream)))

(defun @list (&rest args)
  (reduce (lambda (&optional arg1 arg2)
	    (if (and (null arg1) (null arg2))
		@nil
		(@cons arg1 arg2)))
	  args
	  :initial-value @nil
	  :from-end t))

;;; Common list utility functions
(defun @map (fn list)
  (match list
   (@nil @nil)
   ((@cons x xs) (@cons (funcall fn x) (@map fn xs)))))
