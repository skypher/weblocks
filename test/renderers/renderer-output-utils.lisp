
(in-package :weblocks-test)

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

;;; test render-object-slot
(deftest render-object-slot-1
    (let ((render-object-tmp (lambda (obj &key inlinep testkey)
			       (list obj inlinep testkey))))
      (render-object-slot render-object-tmp nil "some-object" "some-slot" "some-value"
			  '(:testkey "some-key")))
  ("some-value" t "some-key"))

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
     :slots '(name) :mode :strict)
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
