
(in-package :weblocks-test)

;;; Test object-class-name
(deftest object-class-name-1
    (object-class-name *joe*)
  employee)

;;; Test object-name
(deftest object-name-1
    (object-name *joe*)
  "Employee")

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
  "Employee")

;;; test render-object-slot
(deftest render-object-slot-1
    (let ((render-object-tmp (lambda (obj slot-name slot-type slot-value &key inlinep testkey)
			       (list slot-type slot-value inlinep testkey))))
      (render-object-slot render-object-tmp nil "some-object" "some-slot" 'some-type "some-value"
			  '(:testkey "some-key")))
  (some-type "some-value" t "some-key"))

(deftest render-object-slot-2
    (let ((render-slot-tmp (lambda (obj slot-name slot-type obj-name &key testkey)
			       (list obj slot-name (subtypep slot-type 'string) obj-name testkey))))
      (render-object-slot nil render-slot-tmp "some-object" "some-slot-ref" 'integer 1
			  '(:testkey "some-key")))
  ("some-object" "some-slot-ref" t "Fixnum" "some-key"))

;; test render-standard-object
(deftest-html render-standard-object-1
    (render-standard-object nil #'render-slot-simple *joe* :inlinep t)
  (htm
   (:p "NAME")
   (:p "STRING")
   (:p "Joe")
   (:p "MANAGER")
   (:p "T")
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
    (:p "STRING")
    (:p "Joe")
    (:p "MANAGER")
    (:p "T")
    (:p "Jim"))))
