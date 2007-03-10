
(in-package :weblocks-test)

;;; Test humanize-name function
(deftest humanize-name-1
    (humanize-name 'hello-world)
  "Hello World")

(deftest humanize-name-2
    (humanize-name "HELLO-WORLD")
  "Hello World")

(deftest humanize-name-3
    (humanize-name 'hello-ref)
  "Hello")

;;; Test attributize-name function
(deftest attributize-name-1
    (attributize-name 'hello-world)
  "hello-world")

(deftest attributize-name-2
    (attributize-name "hello World-REF")
  "hello-world-ref")

;;; Test list->assoc function
(deftest list->assoc-1
    (list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

;;; Introspection helper
(defun class-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
	  (apply #'class-visible-slots (class-of obj) args)))

;;; Test class-visible-slots function
(deftest class-visible-slots-1
    (class-visible-slot-names *joe*)
  (name manager))

(deftest class-visible-slots-2
    (class-visible-slot-names *joe* :visible-slots '(age))
  (name age manager))

(deftest class-visible-slots-3
    (class-visible-slot-names *joe* :visible-slots '(age blah))
  (name age manager))

;;; Introspection helper
(defun object-visible-slot-names (obj &rest args)
  (mapcar (lambda (x)
	    (cons (slot-definition-name (car x)) (cdr x)))
	  (apply #'object-visible-slots obj args)))

;;; Test object-visible-slots function
(deftest object-visible-slots-1
    (object-visible-slot-names *joe*)
  ((name . name) (manager . manager)))

(deftest object-visible-slots-2
    (object-visible-slot-names *joe* :slots '((name . "first-name")))
  ((name . "first-name") (manager . manager)))

(deftest object-visible-slots-3
    (object-visible-slot-names *joe* :slots '((name . first-name) age))
  ((name . first-name) (age . age) (manager . manager)))

(deftest object-visible-slots-4
    (object-visible-slot-names *joe* :slots '((name . first-name) (age . how-old)))
  ((name . first-name) (age . how-old) (manager . manager)))

(deftest object-visible-slots-5
    (object-visible-slot-names *joe* :slots '((manager . boss) doesnt-exist))
  ((name . name) (manager . boss)))

(deftest object-visible-slots-6
    (object-visible-slot-names *joe* :slots '(manager) :mode :hide)
  ((name . name)))

(deftest object-visible-slots-7
    (object-visible-slot-names *joe* :slots '(manager name) :mode :strict)
  ((manager . manager) (name . name)))

(deftest object-visible-slots-8
    (object-visible-slot-names *joe* :slots '(manager (name . first-name) (age . how-old))
				     :mode :strict)
  ((manager . manager) (name . first-name) (age . how-old)))

(deftest object-visible-slots-9
    (object-visible-slot-names *joe* :slots '(manager) :mode :strict)
  ((manager . manager)))

;;; Test object-class-name
(deftest object-class-name-1
    (object-class-name *joe*)
  employee)

;;; Test object-name
(deftest object-name-1
    (object-name *joe*)
  employee)

(deftest object-name-2
    (let (employee-name)
      (defun employee-name (empl)
	(first-name empl))
      (setf employee-name (object-name *joe*))
      (fmakunbound 'employee-name)
      employee-name)
  "Joe")

(deftest object-name-3
    (let (employee-name)
      (defun employee-name (empl)
	123)
      (setf employee-name (object-name *joe*))
      (fmakunbound 'employee-name)
      employee-name)
  employee)

;;; Test render-slot-inline-p
(deftest render-slot-inline-p-1
    (render-slot-inline-p *joe* 'name)
  t)

(deftest render-slot-inline-p-2
    (render-slot-inline-p *joe* 'address-ref)
  nil)

;;; Test get-slot-value
(deftest get-slot-value-1
    (get-slot-value *joe* (car (car (object-visible-slots *joe* :slots '(age) :mode :strict))))
  30)

(deftest get-slot-value-2
    (get-slot-value *joe* (car (car (object-visible-slots *joe*))))
  "Joe")

