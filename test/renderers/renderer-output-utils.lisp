
(in-package :weblocks-test)

(deftest humanize-name-1
    (humanize-name 'hello-world)
  "Hello World")
(deftest humanize-name-2
    (humanize-name "HELLO-WORLD")
  "Hello World")
(deftest humanize-name-3
    (humanize-name 'hello-ref)
  "Hello")

(deftest attributize-name-1
    (attributize-name 'hello-world)
  "hello-world")
(deftest attributize-name-2
    (attributize-name "hello World-REF")
  "hello-world-ref")

(deftest list->assoc-1
    (list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

(defclass person ()
  ((name :reader first-name :initform "Joe")
   (age :initform 30)))

(defclass employee (person)
  ((manager :reader manager :initform "Jim")))

(defparameter *joe* (make-instance 'employee))

(defun object-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
	  (apply #'class-visible-slots (class-of obj) args)))

(deftest class-visible-slots-1
    (object-visible-slot-names *joe*)
  (name manager))

(deftest class-visible-slots-2
    (object-visible-slot-names *joe* :visible-slots '(age))
  (name age manager))

(deftest class-visible-slots-3
    (object-visible-slot-names *joe* :visible-slots '(age blah))
  (name age manager))
