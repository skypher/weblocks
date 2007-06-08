
(in-package :weblocks-test)

;;; utilities for easier testing
(defun table-header-template (headers rows &key summary pretable posttable)
  `(:div :class "renderer table employee"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 ,@pretable
	 (:table :summary ,summary
	  (:thead
	   (:tr
	    ,@headers))
	  (:tbody
	   ,@rows))
	 ,@posttable
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
