
(in-package :weblocks)

(export '(navigation navigation-panes current-pane
	  with-navigation-header render-navigation-body
	  current-pane-widget init-navigation make-navigation
	  pane-exists-p reset-current-pane))

(defclass navigation (widget)
  ((name :initform nil
	 :documentation "A navigation widget doesn't have a name
	 assigned to automatically.")
   (panes :accessor navigation-panes
	  :initform nil
	  :initarg :panes
	  :documentation "An association list of names and
	  widgets. The names will act as menu entries and attributized
	  names will go into the URL. When a particular entry is
	  clicked, its corresponding pane will be rendered.")
   (current-pane :initform nil
		 :initarg :current-pane
		 :documentation "A name that identifies currently
                  selected entry."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It contains a list of section names and widgets
  associated with those sections, and allows the user to select a
  particular section which it then renders."))

;;; During initialization, current pane will automatically be set to
;;; the first available pane in the list, unless specified otherwise.
(defmethod initialize-instance :after ((obj navigation) &rest initargs &key &allow-other-keys)
  (with-slots (panes current-pane) obj
    (when (null current-pane)
      (setf current-pane (caar panes)))))

(defgeneric with-navigation-header (obj body-fn &rest args)
  (:documentation
   "Renders the header of the navigation widget. Unlike
'with-widget-header', which in case of the navigation widget wraps the
current pane as well as the navigation html, 'with-navigation-header'
only wraps navigation html."))

(defmethod with-navigation-header ((obj navigation) body-fn &rest args)
  (with-slots (name panes) obj
    (with-html
      (:div :class "renderer menu"
	    (with-extra-tags
	      (if (null panes)
		  (htm
		   (:div :class "empty-navigation" "No navigation entries"))
		  (htm
		   (:h1 (if name
			    (str (humanize-name name))
			    (str "Navigation")))
		   (:ul
		    (apply body-fn obj args)))))))))

(defgeneric render-navigation-body (obj &rest args)
  (:documentation
   "Renders the body of the navigation widget. Unlike
'render-widget-body', which in case of navigation renders the current
pane, as well as navigation html, 'render-navigation-body' only
renders navigation HTML.

This method uses '*current-navigation-url*' special variable to
determine its location in order to properly render paths in links."))

(defmethod render-navigation-body ((obj navigation) &rest args)
  (with-slots (panes current-pane) obj
    (with-html
      (mapc (lambda (item)
	      (let* ((item-selected-p (equalp (car item) current-pane))
		     (item-class (when item-selected-p
				   "selected-item")))
		(htm
		 (:li :class item-class
		      (if item-selected-p
			  (htm (:span (str (humanize-name (car item)))))
			  (htm (:a :href (concatenate 'string *current-navigation-url*
						      (attributize-name (car item)))
				   (str (humanize-name (car item))))))))))
	    panes))))

(defmethod render-widget-body ((obj navigation) &rest args)
  (when (current-pane-widget obj)
    (let ((*current-navigation-url* (concatenate 'string
						 *current-navigation-url*
						 (slot-value obj 'current-pane)
						 "/")))
      (declare (special *current-navigation-url*))
      (render-widget (current-pane-widget obj))))
  (apply #'with-navigation-header obj #'render-navigation-body args))

(defun current-pane-widget (obj)
  "Accepts a navigation object and returns the widget that represents its
currently selected pane."
  (with-slots (panes current-pane) obj
    (cdar (member current-pane panes :key #'car :test #'string-equal))))

(defun init-navigation (nav &rest args)
  "A helper function to create a navigation widget

ex:

\(init-navigation
   \"test1\" (make-instance ...)
   \"test2\" (make-instance ...)"
  (loop for count from 1
        for x in args
        for y in (cdr args)
     when (oddp count)
     do (push-end `(,(attributize-name x) . ,y) (navigation-panes nav)))
  (with-slots (current-pane) nav
    (when (null current-pane)
      (setf current-pane (caar (navigation-panes nav)))))
  nav)

(defun make-navigation (name &rest args)
  "Instantiates 'navigation' widget via 'make-instance' and forwards
it along with 'args' to 'init-navigation'."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))

(defun pane-exists-p (navigation-object name)
  "Given a navigation object and a name determines if there is a pane
with the specified name."
  (not (null (find (attributize-name name)
		   (navigation-panes navigation-object) :key #'car :test #'equalp))))

(defun reset-current-pane (navigation-object)
  "Sets the current pane to the first available pane in the list."
  (setf (slot-value navigation-object 'current-pane)
	(caar (navigation-panes navigation-object))))

(defmethod find-widget-by-path* (path (root navigation))
  (find-widget-by-path* (cdr path)
			(cdar (member (car path)
				      (navigation-panes root)
				      :key #'car
				      :test #'string-equal))))
