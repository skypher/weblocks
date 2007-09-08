
(in-package :weblocks-test)

;;; test typespec-compound-only-p
(deftest typespec-compound-only-p-1
    (values (typespec-compound-only-p 'and) (typespec-compound-only-p 'satisfies)
	    (typespec-compound-only-p 'eql) (typespec-compound-only-p 'member)
	    (typespec-compound-only-p 'mod) (typespec-compound-only-p 'values)
	    (typespec-compound-only-p 'not) (typespec-compound-only-p 'or)
	    (typespec-compound-only-p 'int))
  t t t t t t t t nil)

;;; test type-expand
(deftest type-expand-1
    (type-expand 'foo1)
  integer)

(deftest type-expand-2
    (type-expand 'foo2)
  integer)

(deftest type-expand-3
    (type-expand 'integer)
  integer)

;;; test expand-typespec
(deftest expand-typespec-1
    (expand-typespec 'integer)
  integer)

(deftest expand-typespec-2
    (expand-typespec 'foo2)
  integer)

(deftest expand-typespec-3
    (expand-typespec '(or integer))
  (or integer))

(deftest expand-typespec-4
    (expand-typespec '(or integer (and foo2 pathname)))
  (or integer (and integer pathname)))

;;; test type-prototype
(deftest type-prototype-1
    (every (lambda (type)
	     (typep (type-prototype (find-class type)) type))
	   '(t character symbol function number complex real integer
	     rational ratio fixnum bignum float simple-base-string
	     base-string string bit-vector vector array cons list null
	     sequence widget))
  t)

;;; test slot-management-generic-function
(defgeneric slot-management-generic-function-test-fn-1 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:method (obj slot-name (slot-type (eql 'foo)) value &rest args)
    1)
  (:method (obj slot-name (slot-type fixnum) value &rest args)
    2))

(deftest slot-management-generic-function-1
    (slot-management-generic-function-test-fn-1 nil nil 'foo nil)
  1)

(deftest slot-management-generic-function-2
    (slot-management-generic-function-test-fn-1 nil nil 'integer nil)
  2)

(defgeneric slot-management-generic-function-test-fn-2 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:method (obj slot-name (slot-type (eql 'integer)) value &rest args)
    1)
  (:method (obj slot-name (slot-type integer) value &rest args)
    2))

(deftest slot-management-generic-function-3
    (slot-management-generic-function-test-fn-2 nil nil 'integer nil)
  1)

(defgeneric slot-management-generic-function-test-fn-3 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:method (obj slot-name slot-type value &rest args)
    1))

(deftest slot-management-generic-function-4
    (slot-management-generic-function-test-fn-3 nil nil 'integer nil)
  1)

(defgeneric slot-management-generic-function-test-fn-4 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:method (obj slot-name slot-type value &key slot-type-args)
    slot-type-args))

(deftest slot-management-generic-function-5
    (slot-management-generic-function-test-fn-4 nil nil '(integer 1 2) nil)
  (1 2))

(defgeneric slot-management-generic-function-test-fn-5 (obj slot-name slot-type value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:method (obj slot-name slot-type value &key slot-type-args)
    slot-type))

(deftest slot-management-generic-function-6
    (slot-management-generic-function-test-fn-5 nil nil '(or null integer) nil)
  integer)

(defgeneric slot-management-generic-function-test-fn-6 (a slot-type b &key slot-type-args)
  (:generic-function-class slot-management-generic-function)
  (:method (a slot-type b &key slot-type-args)
    (values a slot-type b slot-type-args)))

(deftest slot-management-generic-function-7
    (slot-management-generic-function-test-fn-6 1 '(integer 5 6) 2)
  1 integer 2 (5 6))

(defgeneric slot-management-generic-function-test-fn-7 (a slot-type b)
  (:generic-function-class slot-management-generic-function)
  (:method (a slot-type b)
    (values a slot-type b)))

(deftest slot-management-generic-function-8
    (slot-management-generic-function-test-fn-7 1 '(integer 5 6) 2)
  1 integer 2)

