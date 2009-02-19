
(in-package :weblocks-test)

;;; Test table view with-view-header
(deftest-html table-view-with-view-header-1
    (with-view-header (make-instance 'table-view)
      (list *joe* *bob*)
      nil (lambda (&rest args) (declare (ignore args))))
  (:div :class "view table employee"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

;;; Test  with-table-view-header
(deftest-html with-table-view-header-1
    (with-table-view-header (make-instance 'table-view)
      (list *joe* *bob*) nil
      (lambda (&rest args) (declare (ignore args)))
      (lambda (&rest args) (declare (ignore args))))
  (:table (:thead "") (:tbody "")))

;;; Test with-table-view-header-row
(deftest-html with-table-view-header-row-1
    (with-table-view-header-row (make-instance 'table-view)
      *joe* nil)
  (:tr ""))

;;; Test render-table-view-header-row
(deftest-html render-table-view-header-row-1
    (render-table-view-header-row (find-view '(table employee))
				  *joe* nil)
  (htm
   (:th :class "name" (:span :class "label" "Name"))
   (:th :class "manager" (:span :class "label" "Manager"))))

;;; Test table view render-view-field-header
(deftest-html table-view-render-view-field-header-1
    (render-view-field-header (make-instance 'table-view-field
					     :slot-name 'name)
			      (find-view '(table employee))
			      nil (make-instance 'text-presentation)
			      "Joe" *joe*)
  (:th :class "name" (:span :class "label" "Name")))

;;; Test table view render-view-field-header-value
(deftest-html table-view-render-view-field-header-value-1
    (render-view-field-header-value "Joe"
				    (make-instance 'text-presentation)
				    (make-instance 'table-view-field
						   :slot-name 'name)
				    (find-view '(table employee))
				    nil 
				     *joe*)
  (:span :class "label" "Name"))

;;; Test with-table-view-body-row
(deftest-html with-table-view-body-row-1
    (with-table-view-body-row (make-instance 'table-view)
      *joe* nil)
  (:tr ""))

;;; Test render-table-view-body-row
(deftest-html render-table-view-body-row-1
    (render-table-view-body-row (find-view '(table employee))
				  *joe* nil)
  (htm
   (:td :class "name" (:span :class "value" "Joe"))
   (:td :class "manager" (:span :class "value" "Jim"))))

;;; Test table view render-view-field
(deftest-html table-view-render-view-field-1
    (render-view-field (make-instance 'table-view-field
				      :slot-name 'name)
		       (find-view '(table employee))
		       nil (make-instance 'text-presentation)
		       "Joe" *joe*)
  (:td :class "name" (:span :class "value" "Joe")))

;;; Test table view render-view-field-value
(deftest-html table-view-render-view-field-value-1
    (render-view-field-value "Joe"
			     (make-instance 'text-presentation)
			     (make-instance 'table-view-field
					    :slot-name 'name)
			     (find-view '(table employee))
			     nil 
			     *joe*)
  (:span :class "value" "Joe"))

;;; Test table view render-object-view
(deftest-html table-view-render-object-view-1
    (let (weblocks::*page-dependencies*)
      (declare (special weblocks::*page-dependencies*))
      (render-object-view (list *joe* *bob*) '(table employee)))
  #.(table-header-template
     '((:th :class "name" (:span :class "label" "Name"))
       (:th :class "manager" (:span :class "label" "Manager")))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Bob"))
	(:td :class "manager" (:span :class "value" "Jim"))))))

