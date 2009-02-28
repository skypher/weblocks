
(in-package :weblocks)

(export '(navigation render-navigation-menu init-navigation make-navigation
	  navigation-pane-names navigation-header
          navigation-hidden-panes navigation-render-content))

(defwidget navigation (static-selector)
  ((pane-names :accessor navigation-pane-names
	       :initarg :pane-names
	       :initform nil
	       :documentation "An alist mapping uri-tokens to
	       human-readable pane names (rendered as a menu). Use nil
	       as the key for the default item.")
   (header :accessor navigation-header
	   :initarg :header
	   :initform nil
	   :documentation "A heading that will be rendered in a <h1> tag")
   (hidden-panes :accessor navigation-hidden-panes
		 :initarg :hidden-panes
		 :initform nil
		 :documentation "A list of uri-tokens representing a set
		 of panes that should be hidden (not rendered in a menu,
		 but accessible from within this navigation object.)")
   (render-content :accessor navigation-render-content
		   :initarg :render-content
		   :initform t
		   :documentation "Whether navigation should also render
		   its contents. You want to set this to nil if you use
		   the teleport widget to render the contents
		   elsewhere."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It is a static-selector that also knows what its
  pane names are, so it can render a menu, set a page title, and
  contribute to navigation breadcrumbs."))

(defun navigation-pane-name-for-token (navigation token)
  "Return the pane name for a given uri-token or NIL if not found. Token
may be NIL in which case the default pane name is provided."
  (cdr (assoc token (navigation-pane-names navigation) :test #'equalp)))

(defgeneric render-navigation-menu (obj &rest args)
  (:documentation "Renders the HTML menu for the navigation widget.")
  (:method ((obj navigation) &rest args &key menu-args &allow-other-keys)
    (declare (ignore args))
    (apply #'render-menu
           (remove nil
                   (mapcar (lambda (pane)
		       (let ((token (car pane)))
			 (unless (member token (navigation-hidden-panes obj)
					 :test #'string-equal)
			   (cons (navigation-pane-name-for-token obj token)
				 (compose-uri-tokens-to-url token)))))
                           (static-selector-panes obj)))
           :base (selector-base-uri obj)
           :selected-pane (static-selector-current-pane obj)
           :header (if (widget-name obj)
                     (humanize-name (widget-name obj))
                     "Navigation")
           :container-id (ensure-dom-id obj)
           :empty-message "No navigation entries"
           menu-args)))

(defmethod render-widget-body ((obj navigation) &rest args)
  (apply #'render-navigation-menu obj args))


(defmethod render-widget-children ((obj navigation) &rest args)
    (when (navigation-render-content obj)
      (with-html 
        (:div :class "navigation-body"
	    (mapc (lambda (obj) (apply #'render-widget obj args))
                  (widget-children obj :selector))))))

(defmethod per-class-dependencies append ((obj navigation))
  (list (make-local-dependency :stylesheet "menu")))

(defmethod page-title ((obj navigation))
  (navigation-pane-name-for-token obj (static-selector-current-pane obj)))

(defun init-navigation (obj &rest args)
  "A helper function to create a navigation widget."
  (mapc (lambda (pane-info)
          (let ((token (or (third pane-info) (attributize-name (first pane-info))))
                (name (first pane-info))
                (widget (second pane-info)))
            (when (string-equal token "")
              (setf token nil))
            (push-end (cons token name) (navigation-pane-names obj))
            (push-end (cons token widget) (static-selector-panes obj))))
        args)
  obj)

(defun make-navigation (name &rest args)
  "Instantiates the default navigation widget via 'make-instance' and
forwards it along with 'args' to 'init-navigation'. The navigation
widgets bears the title NAME."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))


(export '(lazy-navigation make-lazy-navigation))

(defwidget lazy-navigation (navigation)
  ()
  (:documentation "Lazy navigation does not create the entire widget
  tree immediately. Instead, parts of the widget tree are created as
  they are needed. Note that in the current implementation parts of the
  tree that are not selected are forgotten."))

(defmethod update-dependents ((obj lazy-navigation) children)
  "Lazily resolve our new children -- if any of them are functions, call
them to get the real widgets."
  (setf (widget-children obj :selector)
			(mapcar (lambda (child)
				  (if (functionp child) (funcall child) child))
				(ensure-list children))))

(defun make-lazy-navigation (name &rest args)
  (let ((nav (make-instance 'lazy-navigation :name name)))
    (apply #'init-navigation nav args)
    nav))


(export '(teleport teleport-source teleport-key))

(defwidget teleport ()
  ((source :accessor teleport-source
	   :initarg :source
	   :documentation "Source widget that should be teleported and
	   rendered.")
   (key :accessor teleport-key
	:initarg :key
	:initform #'identity
	:documentation "The function that will be used to access the
   widget from the source."))
  (:documentation "A widget that will render ('teleport') another widget
  to a particular place. It is your responsibility to make sure the
  teleported widget isn't rendered in its source location. A good use
  for this is navigation with detached content -- you often want to have
  the menu for your navigation rendered in one place, and its navigated
  content elsewhere. The teleport widget lets you do that."))

(defmethod render-widget-body ((obj teleport) &rest args)
  (apply #'render-widget (funcall (teleport-key obj) (teleport-source obj)) args))

