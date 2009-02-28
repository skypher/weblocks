
(in-package :weblocks)

(export '(breadcrumbs))

(defwidget breadcrumbs ()
  ()
  (:documentation "A (misnamed) breadcrumbs widget, showing the current
  position within the site's navigation system. Example: 'Home > Events
  > Latest Event'. Its render-widget-body method walks the widget tree,
  finds navigation widgets and learns about their selections."))


(defgeneric walk-navigation (obj fn)
  (:documentation "Walk the widget tree starting at obj and calling fn at every node.")
  (:method ((obj null) fn) (assert nil))	; bug?
  (:method ((obj widget) fn) nil)		; widgets are leaves by default
  (:method ((obj function) fn) nil)	; functions have no children
  (:method ((obj string) fn) nil)		; and neither do strings
  (:method ((obj container) fn)
    (mapc (curry-after #'walk-navigation fn) (widget-children obj)))
  (:method ((obj navigation) fn)
    (funcall fn obj)
    (mapc (curry-after #'walk-navigation fn) (widget-children obj))))


(defmethod render-widget-body ((obj breadcrumbs) &rest args)
  (declare (ignore args))
  (let (crumbs)
    (walk-navigation
     (root-composite)
     (lambda (obj)
       (unless crumbs
	 (push (navigation-pane-name-for-token obj nil) crumbs))
       (push-end (widget-uri-path obj) crumbs)
       (push-end (navigation-pane-name-for-token obj (static-selector-current-pane obj)) crumbs)))
    (log-message* "crumbs: ~A" crumbs)
    (with-html
      (:ul
       (loop for item on crumbs by #'cddr
	  do (progn
	       (if (second item)
		   (htm (:li (:a :href (second item) (str (first item)))))
		   (htm (:li (str (first item)))))))))))

