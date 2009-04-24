
(in-package :weblocks)

(export '(breadcrumbs))

(defwidget breadcrumbs ()
  ()
  (:documentation "A (misnamed) breadcrumbs widget, showing the current
  position within the site's navigation system. Example: 'Home > Events
  > Latest Event'. Its render-widget-body method walks the widget tree,
  finds navigation widgets and learns about their selections."))

(defmethod render-widget-body ((obj breadcrumbs) &rest args)
  (declare (ignore args))
  (let (crumbs)
    (walk-widget-tree
     (root-widget)
     (lambda (obj depth)
       (declare (ignore depth))
       ;; we only process objects that eat URI tokens, we need to know them, unfortunately
       (cond 
	 ((subclassp (class-of obj) 'navigation)
	  (unless crumbs
	    (push (navigation-pane-name-for-token obj nil) crumbs))
	  (push-end (make-webapp-uri (selector-base-uri obj)) crumbs)
	  (push-end (navigation-pane-name-for-token obj (static-selector-current-pane obj)) crumbs))
	 ((equal (class-of obj) (find-class 'on-demand-selector))
	  (let ((name (car (last (car (on-demand-selector-cache obj))))))
	    (when name
	      (push-end (make-webapp-uri (selector-base-uri obj)) crumbs)
	      ;; hopefully one of our children defined a page-title method...
	      (push-end (or (first (remove nil (mapcar #'page-title (widget-children obj))))
			    (humanize-name name))
			crumbs)))))))
    (with-html
      (:ul
       (loop for item on crumbs by #'cddr
	  do (progn
	       (if (second item)
		   (htm (:li (:a :href (second item) (str (first item)))))
		   (htm (:li (str (first item)))))))))))


