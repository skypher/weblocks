
(in-package :weblocks-elephant-demo)

(defun initial-page (k)
  "Initial page is so simple we can just define a function to render
it and use it as a widget. Since it will be used in a continuation
flow, it accepts K - the continuation parameter."
  (with-html
    (:div :style "text-align: center; margin-top: 25em;"
	  (:p :style "font-style: italic;"
	      "Roses are red," (:br)
	      "Violets are blue," (:br)
	      (:a :href "http://en.wikipedia.org/wiki/Steve_Russell" "Steve Russell") " rocks," (:br)
	      "Homage to you!" (:br))
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
			 (answer k))
		       "Next"
		       :ajaxp nil))))

(defun make-main-page ()
  "Lays out the main page. It consists of a FLASH widget for showing
initial message, and a NAVIGATION widget with panes that hold
employees page and companies page."
  (make-instance 'composite :widgets
		 (list
		  (make-instance 'flash :messages
				 (list "Welcome to weblocks demo - a
				       technology demonstration for a
				       continuations-based web
				       framework written in Common
				       Lisp."))
		  (make-navigation 'main-menu
				   'employees (make-employees-page)
				   'companies (make-companies-page)))))

(defun make-employees-page ()
  "Lays out the widgets for the employees page. It consists of a
single GRIDEDIT widget."
  (make-instance 'composite :widgets
		 (list
		  (make-instance 'gridedit
				 :name 'employees-list
				 :view 'employee-table-view
				 :item-form-view 'employee-form-view
				 :data-class 'employee))))

(defun make-companies-page ()
  "Lays out the widgets for the companies page. It consists of a
single GRIDEDIT widget."
  (make-instance 'composite :widgets
		 (list
		  (make-instance 'gridedit
				 :name 'companies-grid
				 :data-class 'company
				 :view 'company-table-view
				 :item-form-view 'company-form-view))))

