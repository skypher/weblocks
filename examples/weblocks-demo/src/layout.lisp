
(in-package :weblocks-demo)

(defun initial-page (k)
  "Initial page is so simple we can just define a function to render
it and use it as a widget. Since it will be used in a continuation
flow, it accepts K - the continuation parameter."
  (with-html
    (:div :style "text-align: center; font-size:1.4em; margin-top: 4em;"
	  (:p "This demo application was coded in" (:br)
	      (:a :href "http://weblocks.viridian-project.de/" "Weblocks")
	      " - a Lisp web framework." (:br) (:br)
	      "The source of the demo contains" (:br)
	      "200 lines of Lisp" (:br)
	      "and" (:br)
	      "0 line of HTML or Javascript." (:br))
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
			 (answer k))
		       "Start Demo"
		       :ajaxp nil)
	  (:p "(You can also try it with Javascript disabled.)"))))

(defun make-main-page ()
  "Lays out the main page. It consists of a FLASH widget for showing
initial message, and a NAVIGATION widget with panes that hold
employees page and companies page."
  (make-instance 'widget :children
		 (list
		  (make-instance 'flash :messages
				 (list (make-widget "Welcome to weblocks demo - a
                                                    technology demonstration for a
                                                    continuations-based web
                                                    framework written in Common
                                                    Lisp.")))
		  (make-navigation "Main Menu"
				   (list "Employees" (make-employees-page) "employees")
				   (list "Companies" (make-companies-page) "companies")))))

(defun make-employees-page ()
  "Lays out the widgets for the employees page. It consists of a
single GRIDEDIT widget."
  (make-instance 'widget :children
		 (list
		  (make-instance 'gridedit
				 :name 'employees-grid
				 :drilldown-type :view
				 :data-class 'employee
				 :view 'employee-table-view
				 :item-data-view 'employee-data-view
				 :item-form-view 'employee-form-view))))

(defun make-companies-page ()
  "Lays out the widgets for the companies page. It consists of a
single GRIDEDIT widget."
  (make-instance 'widget :children
		 (list
		  (make-instance 'gridedit
				 :name 'companies-grid
				 :data-class 'company
				 :view 'company-table-view
				 :item-form-view 'company-form-view))))

