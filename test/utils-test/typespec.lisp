
(in-package :weblocks-test)

;;; test typespec-compound-only-p
(deftest typespec-compound-only-p-1
    (values (typespec-compound-only-p 'and) (typespec-compound-only-p 'satisfies)
	    (typespec-compound-only-p 'eql) (typespec-compound-only-p 'member)
	    (typespec-compound-only-p 'mod) (typespec-compound-only-p 'values)
	    (typespec-compound-only-p 'not) (typespec-compound-only-p 'or)
	    (typespec-compound-only-p 'int))
  t t t t t t t t nil)

;;; make sure type-expand works (it's too implementation dependent to
;;; have specific tests)
(deftest type-expand-1
    (not (null (type-expand 'foo1)))
  t)

(deftest type-expand-2
    (type-expand 'someundefinedtype)
  someundefinedtype)

;;; test expand-typespec (we can't do a comprehensive test because of
;;; implementation differences)
(deftest expand-typespec-1
    (not (null (expand-typespec 'integer)))
  t)

;;; test inspect-typespec
(deftest inspect-typespec-1
    (weblocks::inspect-typespec 'integer)
  integer nil)

(deftest inspect-typespec-2
    (weblocks::inspect-typespec '(or null integer))
  integer nil)

(deftest inspect-typespec-3
    (weblocks::inspect-typespec '(or integer null))
  integer nil)

(deftest inspect-typespec-4
    (weblocks::inspect-typespec '(or blah integer))
  or (blah integer))

(deftest inspect-typespec-5
    (weblocks::inspect-typespec '(or integer))
  integer nil)

(deftest inspect-typespec-6
    (weblocks::inspect-typespec '(and integer))
  integer nil)

;;; test normalized-type-of
(deftest normalized-type-of-1
    (normalized-type-of (make-instance 'widget))
  widget)

(deftest normalized-type-of-2
    (normalized-type-of "hi")
  string)

(deftest normalized-type-of-3
    (normalized-type-of :foo)
  keyword)

(deftest normalized-type-of-4
    (normalized-type-of 'foo)
  symbol)

;;; normalized find-class
(deftest normalized-find-class-1
    (normalized-find-class 'boolean nil)
  nil)

(deftest normalized-find-class-2
    (car
     (multiple-value-list
      (ignore-errors
	(normalized-find-class 'boolean))))
  nil)

(deftest normalized-find-class-3
    (class-name (normalized-find-class 'widget))
  widget)

;;; test slot-management-[method/generic].initialize-instance
(defgeneric slot-management-method/generic-initialize-instance-test (a b)
  (:generic-function-class slot-management-generic-function))

(deftest slot-management-initialize-instance-1
    (car (multiple-value-list
	  (ignore-errors
	    (defmethod slot-management-method/generic-initialize-instance-test (a b)
	      1))))
  nil)

;;; test defslotmethod
(deftest defslotmethod-1
    (macroexpand-1
     '(defslotmethod some-method (a1 a2)
       (+ a1 a2)))
  (let ((weblocks::*defmethod-type-d* t))
    (declare (special weblocks::*defmethod-type-d*))
    (defmethod some-method (a1 a2)
      (declare (special weblocks::*full-slot-type*))
      (assert (boundp 'weblocks::*full-slot-type*))
      #+allegro (set-funcallable-instance-function
			     #'some-method
			     (compute-discriminating-function #'some-method))
      (let ((slot-type weblocks::*full-slot-type*))
	(+ a1 a2))))
  t)

;;; test type-prototype
(defclass type-prototype-test-class () (foo))

(deftest type-prototype-1
    (progn
      (finalize-inheritance (find-class 'type-prototype-test-class))
      (every (lambda (type)
	       (typep (type-prototype type) type))
	     (remove nil
		     (mapcar (curry-after #'normalized-find-class nil)
			     '(t character symbol function number complex real integer
			       rational ratio fixnum bignum float simple-base-string
			       base-string string bit-vector vector array cons list null
			       sequence widget type-prototype-test-class)))))
  t)

(deftest type-prototype-2
    (type-prototype nil)
  nil)

;;; test slot-management-generic-function
(defgeneric slot-management-generic-function-test-fn-1 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function))

(defslotmethod slot-management-generic-function-test-fn-1 (obj slot-name (slot-type (eql 'foo)) value
							       &rest args)
    1)

(defslotmethod slot-management-generic-function-test-fn-1 (obj slot-name (slot-type fixnum) value
							       &rest args)
    2)

(deftest slot-management-generic-function-1
    (slot-management-generic-function-test-fn-1 nil nil 'foo nil)
  1)

(deftest slot-management-generic-function-2
    (slot-management-generic-function-test-fn-1 nil nil 'integer nil)
  2)

(defgeneric slot-management-generic-function-test-fn-2 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function))

(defslotmethod slot-management-generic-function-test-fn-2 (obj slot-name (slot-type (eql 'integer))
							   value &rest args)
  1)

(defslotmethod slot-management-generic-function-test-fn-2 (obj slot-name (slot-type integer)
							   value &rest args)
  2)

(deftest slot-management-generic-function-3
    (slot-management-generic-function-test-fn-2 nil nil 'integer nil)
  1)

(defgeneric slot-management-generic-function-test-fn-3 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function))

(defslotmethod slot-management-generic-function-test-fn-3 (obj slot-name slot-type value &rest args)
    1)

(deftest slot-management-generic-function-4
    (slot-management-generic-function-test-fn-3 nil nil 'integer nil)
  1)

(defgeneric slot-management-generic-function-test-fn-4 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function))
  
(defslotmethod slot-management-generic-function-test-fn-4 (obj slot-name (slot-type fixnum) value &rest args)
    slot-type)

(deftest slot-management-generic-function-5
    (slot-management-generic-function-test-fn-4 nil nil '(integer 1 2) nil)
  (integer 1 2))

(defgeneric slot-management-generic-function-test-fn-5 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function))

(defslotmethod slot-management-generic-function-test-fn-5 (obj slot-name slot-type value &rest args)
    slot-type)

(deftest slot-management-generic-function-6
    (slot-management-generic-function-test-fn-5 nil nil '(or null integer) nil)
  (or null integer))

(defgeneric slot-management-generic-function-test-fn-6 (a slot-type b)
  (:generic-function-class slot-management-generic-function))
  
(defslotmethod slot-management-generic-function-test-fn-6 (a slot-type b)
    (values a slot-type b))

(deftest slot-management-generic-function-7
    (slot-management-generic-function-test-fn-6 1 '(integer 5 6) 2)
  1 (integer 5 6) 2)

