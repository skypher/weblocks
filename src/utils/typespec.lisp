
(in-package :weblocks)

(export '(typespec-compound-only-p type-expand expand-typespec
	  type-prototype slot-management-generic-function slot-type
	  slot-type-args))

;;; Compound-only typespecs
(defun typespec-compound-only-p (typespec-name)
  (not (null (member typespec-name '(and eql member mod not or satisfies values)))))

;;; Expanding typespecs
(defun type-expand (typespec)
  "Expands user type aliases defined by 'deftype'. This function calls
implementation specific non-ANSI functions."
  #+cmu (kernel:type-expand typespec)
  #+sbcl (sb-kernel:type-expand typespec)
  #+clisp (ext:type-expand typespec)
  #+(or openmcl mcl) (ccl::type-expand typespec)
  #+allegro (excl:normalize-type typespec)
  #+lispworks (type:expand-user-type typespec)
  #-(or cmu sbcl clisp openmcl mcl allegro lispworks) (error "Not implemented on this lisp system."))

(defun expand-typespec (typespec)
  "Uses 'type-expand' to expand complex typespecs that may contain
compound-only specifiers."
  (when (not (listp typespec))
    (return-from expand-typespec
      (type-expand typespec)))
  (if (typespec-compound-only-p (car typespec))
      (apply #'list (car typespec) (mapcar #'expand-typespec (cdr typespec)))
      (type-expand typespec)))

;;; Typespec management for easier specializing
(defun inspect-typespec (typespec)
  "Converts 'typespec' into a representation suitable for specializing
via MOP. Returns two values. The first one is a symbol that can be
specialized on via eql specializer, and the second one is a list of
arguments to the typespec.
Ex:
\(inspect-typespec '(integer 1 5))
=> integer
=> (1 5)

This function also implements certain useful logic. For example:
\(inspect-typespec '(or null (integer 1 5)))
=> integer
=> (1 5)"
  (if (listp typespec)
      (case (car typespec)
	    (mod (values (car typespec) (cdr typespec)))
	    (eql (values (car typespec) (cdr typespec)))
	    (member (values (car typespec) (cdr typespec)))
	    (not (values nil nil))
	    (satisfies (values (car typespec) (cdr typespec)))
	    (values (values nil nil))
	    (and (values nil nil))
	    (or (match typespec
		       ((list 'or 'null y) (inspect-typespec y))
		       ((list 'or y 'null) (inspect-typespec y))
		       (y (values nil nil))))
	    (otherwise (values (car typespec) (cdr typespec))))
      (values typespec nil)))

(defclass slot-management-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:documentation "A custom metaclass for generic functions that
  operate on slots. Such generic functions have four required
  arguments: object whose slots is being operated on, the name of the
  slot, the type of the slot, and the value of the slot. Other keyword
  or &rest arguments may be accepted. A custom metaclass is necessary
  to customize the behavior of the third argument (slot-type). First
  the system attempts to convert a declared slot type to a class via
  'find-class'. If such a class exists, a method that specializes on
  it is looked for. If such a method is found, it is called with the
  prototype values of the class. If not, the actual type definition is
  passed. This is done to support simple specialization on slot types
  that is capable of taking advantage of class inheritance
  hierarchy."))

(defun type-prototype (type)
  "Behaves like 'class-prototype', except returns reasonable values
for built-in classes accross implementations."
  (etypecase type
    (standard-class (class-prototype type))
    (built-in-class (cond
		      ((eq type (find-class 't)) t)
		      ((eq type (find-class 'character)) (code-char 42))
		      ((eq type (find-class 'symbol)) '#:mu)
		      ((eq type (find-class 'function)) (lambda (&rest args) (declare (ignore args)) 42))
		      ((eq type (find-class 'number)) 42)
		      ((eq type (find-class 'complex)) (complex 42 42))
		      ((eq type (find-class 'real)) 42)
		      ((eq type (find-class 'integer)) 42)
		      ((eq type (find-class 'rational)) (rational 42))
		      ((eq type (find-class 'ratio)) (/ 1 3))
		      ((eq type (find-class 'fixnum)) 42)
		      ((eq type (find-class 'bignum)) (+ 1 most-positive-fixnum))
		      ((subtypep type (find-class 'float)) (float 42))
		      ((eq type (find-class 'simple-base-string)) (make-array 0 :element-type 'base-char))
		      ((eq type (find-class 'base-string)) (make-array 0 :element-type 'base-char
								       :fill-pointer t))
		      ((subtypep type (find-class 'string)) "42")
		      ((subtypep type (find-class 'bit-vector)) (make-array 1 :element-type 'bit))
		      ((subtypep type (find-class 'vector)) (make-array 0))
		      ((subtypep type (find-class 'array)) (make-array nil))
		      ((eq type (find-class 'cons)) (cons 4 2))
		      ((eq type (find-class 'list)) (list 4 2))
		      ((eq type (find-class 'null)) nil)
		      ((subtypep type (find-class 'sequence)) (list))))))

(defmethod compute-discriminating-function ((gf slot-management-generic-function))
  (let* ((default-discriminating-function (call-next-method))
	 (required-args-count (length (generic-function-argument-precedence-order gf)))
	 (lambda-list (generic-function-lambda-list gf))
	 (type-argument-index (position 'slot-type lambda-list
					:end required-args-count))
	 (insert-type-args-p (or (find '&rest lambda-list)
				 (find '&allow-other-keys lambda-list)
				 (find 'slot-type-args lambda-list))))
    (lambda (&rest args)
      (multiple-value-bind (typespec-symbol typespec-args)
	  (inspect-typespec (nth type-argument-index args))
	(flet ((generate-args-list (type-argument)
		 (let ((new-list (copy-list args)))
		   (setf (nth type-argument-index new-list) type-argument)
		   (when insert-type-args-p
		     (setf new-list (ninsert new-list :slot-type-args required-args-count))
		     (setf new-list (ninsert new-list typespec-args (1+ required-args-count))))
		   new-list)))
	  (let ((typespec-class (find-class typespec-symbol nil)))
	    (apply default-discriminating-function
		   (generate-args-list
		    (if (and typespec-class
			     (> (length (funcall
					 #'compute-applicable-methods gf 
					 (generate-args-list (type-prototype typespec-class))))
				(length (funcall
					 #'compute-applicable-methods gf
					 (generate-args-list typespec-symbol)))))
			(type-prototype typespec-class)
			typespec-symbol)))))))))

