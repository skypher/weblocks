
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entires."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu (options &key selected-uri header
		    (empty-message *menu-empty-message*))
  "Renders a menu snippet based on given options and selected
option. An option may be a dotted pair of a label and URL to link to,
or a name (which will be converted to a label and a URL via
humanize-name and attributize-name, respectively). The selected-uri
will be compared to an option's URL tokens via equalp. If the selected
option isn't specified, first option is rendered as selected."
  (declare (special *current-navigation-url*))
  (with-html
    (:div :class "view menu"
	  (with-extra-tags
	    (when header
	      (htm (:h1 (str header))))
	    (if (null options)
		(htm
		 (:div :class "empty-menu" (str empty-message)))
		(htm
		 (:ul
		  (mapc (lambda (option)
			  (unless (consp option)
			    (setf option
				  (cons (humanize-name option)
					(attributize-name option))))
			  (let* ((label (car option))
				 (uri (cdr option))
				 (pane-selected-p (equalp uri selected-uri))
				 (pane-class (when pane-selected-p
					       "selected-item"))
				 (uri-prefix *current-navigation-url*))
			    (htm
			     (:li :class pane-class
				  (if pane-selected-p
				      (htm (:span (str label)))
				      (htm (:a :href (concatenate 'string
								  (string-right-trim "/" uri-prefix)
								  "/"
								  (string-left-trim "/" uri))
					       (str label))))))))
			options))))))))

