
(in-package :weblocks-test)

;;; utilities for easier testing
(defun table-header-template (headers rows &key summary pretable posttable
			      (table-class "employee"))
  `(:div :class ,(format nil "view table ~A" table-class)
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 ,@pretable
	 (:table :summary ,summary
	  (:thead
	   (:tr
	    ,@headers))
	  (:tbody
	   ,@rows))
	 ,@posttable
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
