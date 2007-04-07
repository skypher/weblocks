
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
    (weblocks::list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

;;; Introspection helper
(defun class-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
	  (apply #'weblocks::class-visible-slots (class-of obj) args)))

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

;;; test render-extra-tags
(deftest-html render-extra-tags-1
    (render-extra-tags "test-" 2)
  (htm (:div :class "test-1" "&nbsp;")
       (:div :class "test-2" "&nbsp;")))

;;; test with-extra-tags
(deftest-html with-extra-tags-1
    (with-extra-tags
      (with-html (:div "hi")))
  (htm (:div :class "extra-top-1" "&nbsp;")
       (:div :class "extra-top-2" "&nbsp;")
       (:div :class "extra-top-3" "&nbsp;")
       (:div "hi")
       (:div :class "extra-bottom-1" "&nbsp;")
       (:div :class "extra-bottom-2" "&nbsp;")
       (:div :class "extra-bottom-3" "&nbsp;")))

;;; test render-object-slot
(deftest render-object-slot-1
    (let ((render-object-tmp (lambda (obj &key inlinep name testkey)
			       (list obj inlinep name testkey))))
      (render-object-slot render-object-tmp nil "some-object" "some-slot" "some-value"
			  '(:testkey "some-key")))
  ("some-value" t "some-slot" "some-key"))

(deftest render-object-slot-2
    (let ((render-slot-tmp (lambda (obj slot-name obj-name &key testkey)
			       (list obj slot-name obj-name testkey))))
      (render-object-slot nil render-slot-tmp "some-object" "some-slot-ref" 1
			  '(:testkey "some-key")))
  ("some-object" "some-slot-ref" fixnum "some-key"))

;; Slot rendering helper
(defun render-slot-simple (obj slot-name slot-value &rest keys)
    (with-html
      (:p (str slot-name))
      (:p (str slot-value))))

;; test visit-object-slots
(deftest-html visit-object-slots-1
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     '(:slots (name) :mode :strict))
  (htm
   (:p "NAME")
   (:p "Joe")))

;; test render-standard-object
(deftest-html render-standard-object-1
    (render-standard-object nil #'render-slot-simple *joe* :inlinep t)
  (htm
   (:p "NAME")
   (:p "Joe")
   (:p "MANAGER")
   (:p "Jim")))

(deftest-html render-standard-object-2
    (render-standard-object (lambda (obj body-fn &rest keys)
			      (with-html
				(:div
				 (funcall body-fn))))
			    #'render-slot-simple *joe*)
  (htm
   (:div
    (:p "NAME")
    (:p "Joe")
    (:p "MANAGER")
    (:p "Jim"))))
