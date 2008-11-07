
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entires."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu (options &key selected-pane header (container-id (gen-id))
		    (empty-message *menu-empty-message*))
  "Renders a menu snippet based on given options and selected
option. An option may be a dotted pair of a label and URL to link to,
or a name (which will be converted to a label and a URL via
humanize-name and attributize-name, respectively). The selected-uri
will be compared to an option's URL tokens via equalp. If the selected
option isn't specified, first option is rendered as selected.  If
CONTAINER-ID is provided, it is used as the basis of DOM IDs for the
menu and each menu item generated with `unattributized-name'."
  (declare (special *current-navigation-url*))
  (with-html
    (:div :class "view menu"
	  :id (unattributized-name container-id 'menu)
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
			  (unless selected-pane
			    (setf selected-pane (car option)))
			  (let* ((label (car option))
				 (target (cdr option))
				 (pane-selected-p (equalp (car option) selected-pane))
				 (pane-class (when pane-selected-p
					       "selected-item")))
			    (htm
			      (:li :id (unattributized-name (format nil "~A-~A" container-id label)
							    'menu-item)
				   :class pane-class
                                  (etypecase target
                                    (string
                                      (if pane-selected-p
                                        (htm (:span (str label)))
                                        (htm (:a :href (make-webapp-uri
                                                         (string-left-trim
                                                           "/" (concatenate 'string
                                                                            (string-right-trim "/" *current-navigation-url*)
                                                                            "/"
                                                                            (string-left-trim "/" target))))
                                                 (str label)))))
                                    (function
                                      (render-link target label)))))))
			options))))))))

