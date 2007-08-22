
(in-package :weblocks-test)

;;; test data-print-object for booleans
(deftest data-print-object-booleans-1
    (data-print-object nil 'name 'boolean t)
  "Yes")

(deftest data-print-object-booleans-2
    (data-print-object nil 'name 'boolean nil)
  "No")

;;; test render-data/aux for booleans
(deftest-html render-data/aux-booleans-1
    (render-data *joe* :slots '(name veteran) :mode :strict)
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "veteran" (:span :class "label" "Veteran:&nbsp;") (:span :class "value" "No")))
     :postslots nil))

(deftest-html render-data/aux-booleans-2
    (let ((employee (copy-template *joe*)))
      (setf (slot-value employee 'veteran) t)
      (render-data employee  :slots '(name veteran) :mode :strict))
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "veteran" (:span :class "label" "Veteran:&nbsp;") (:span :class "value" "Yes")))
     :postslots nil))

;;; test render-form/aux for booleans
(deftest-html render-form/aux-booleans-1
    (with-request :post nil
      (render-form *joe* :slots '(name veteran) :mode :strict))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "veteran"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Veteran:&nbsp;"))
	 (:input :name "veteran" :type "checkbox" :class "checkbox" :value "t"))))))

(deftest-html render-form/aux-booleans-2
    (with-request :post nil
    (let ((employee (copy-template *joe*)))
      (setf (slot-value employee 'veteran) t)
      (render-form employee  :slots '(name veteran) :mode :strict)))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "veteran"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Veteran:&nbsp;"))
	 (:input :name "veteran" :type "checkbox" :class "checkbox" :value "t"
		 :checked "checked"))))))

;;; test parse-slot-from-request for booleans
(deftest parse-slot-from-request-booleans-1
    (parse-slot-from-request 'boolean nil "t")
  t)

(deftest parse-slot-from-request-booleans-2
    (parse-slot-from-request 'boolean nil nil)
  nil)

(deftest parse-slot-from-request-booleans-3
    (multiple-value-bind (res err)
	(ignore-errors
	  (parse-slot-from-request 'boolean nil "blah"))
      (format nil "~A" (type-of err)))
  "PARSE-ERROR")

;;; test slot-in-request-empty-p for booleans
(deftest slot-in-request-empty-p-booleans-1
    (slot-in-request-empty-p 'boolean "t")
  nil)

(deftest slot-in-request-empty-p-booleans-2
    (slot-in-request-empty-p 'boolean "nil")
  nil)

;;; test object-satisfies-search-p for booleans
(deftest object-satisfies-search-p-booleans-1
    (object-satisfies-search-p "Yes" *joe* 'veteran 'boolean t)
  t)

(deftest object-satisfies-search-p-booleans-2
    (object-satisfies-search-p "Yes" *joe* 'veteran 'boolean nil)
  nil)

(deftest object-satisfies-search-p-booleans-3
    (object-satisfies-search-p "No" *joe* 'veteran 'boolean nil)
  t)

(deftest object-satisfies-search-p-booleans-4
    (object-satisfies-search-p "No" nil nil t *joe* :slots '(veteran))
  t)

