
(in-package :weblocks)

(export '(navigation navigation-panes navigation-render-menu-p
	  current-pane *current-navigation-url*
	  navigation-default-pane with-navigation-header
	  render-navigation-body current-pane-widget init-navigation
	  make-navigation pane-exists-p reset-current-pane))

(defwidget navigation (widget)
  ((name :initform nil
	 :documentation "A navigation widget doesn't have a random
	 name assigned to automatically because the name is also
	 displayed to the user.")
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
                 selected entry.")
   (render-menu-p :accessor navigation-render-menu-p
		  :initform t
		  :initarg :render-menu-p
		  :documentation "If this flag is set to
		  true (default), navigation will render the menu with
		  links to the panes. Otherwise, no menu is
		  rendered."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It contains a list of section names and widgets
  associated with those sections, and allows the user to select a
  particular section which it then renders."))

(defparameter *current-navigation-url* nil
  "Always contains a navigation URL at the given point in rendering
cycle. This is a special variable modified by the navigation controls
during rendering so that inner controls can determine their location
in the application hierarchy.")

(defgeneric navigation-default-pane (obj)
  (:documentation
   "Must return the name of the default pane for the navigation
object. The default implementation returns the first pane from
'navigation-panes'. Specialize this function to specify how to pick
default panes for your navigation object."))

(defmethod navigation-default-pane ((obj navigation))
  (caar (navigation-panes obj)))

;;; During initialization, current pane will automatically be set to
;;; the first available pane in the list, unless specified otherwise.
(defmethod initialize-instance :after ((obj navigation) &rest initargs &key &allow-other-keys)
  (with-slots (panes current-pane) obj
    (when (null current-pane)
      (setf current-pane (navigation-default-pane obj)))))

(defgeneric with-navigation-header (obj body-fn &rest args)
  (:documentation
   "Renders the header of the navigation widget. Unlike
'with-widget-header', which in case of the navigation widget wraps the
current pane as well as the navigation html, 'with-navigation-header'
only wraps navigation html."))

(defmethod with-navigation-header ((obj navigation) body-fn &rest args)
  (when (navigation-render-menu-p obj)
    (with-slots (name panes) obj
      (with-html
	(:div :class "view menu"
	      (with-extra-tags
		(if (null panes)
		    (htm
		     (:div :class "empty-navigation" "No navigation entries"))
		    (htm
		     (:h1 (if name
			      (str (humanize-name name))
			      (str "Navigation")))
		     (:ul
		      (apply body-fn obj args))))))))))

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
						      (unless (equalp (car item)
								      (navigation-default-pane obj))
							(string-downcase
							 (url-encode (attributize-name (car item))))))
				   (str (humanize-name (car item))))))))))
	    panes))))

(defmethod render-widget-body ((obj navigation) &rest args)
  (when (current-pane-widget obj)
    (let ((*current-navigation-url* (concatenate 'string
						 *current-navigation-url*
						 (string-downcase
						  (url-encode (slot-value obj 'current-pane)))
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

;;; Code that implements friendly URLs ;;;
(defun apply-uri-to-navigation (tokens navigation-widget)
  "Takes URI tokens and applies them one by one to navigation widgets
in order to allow for friendly URLs. The URLs are basically intimately
linked to navigation controls to simulate document resources on the
server."
  (when (null tokens)
    (reset-navigation-widgets navigation-widget)
    (return-from apply-uri-to-navigation))
  (let ((first-token (url-decode (car tokens))))
    (if (and navigation-widget (pane-exists-p navigation-widget first-token))
	(progn
	  (setf (slot-value navigation-widget 'current-pane) first-token)
	  (apply-uri-to-navigation (cdr tokens)
				   (find-navigation-widget (current-pane-widget navigation-widget))))
	(setf (return-code) +http-not-found+))))

(defun obtain-uri-from-navigation (navigation-widget)
  "Walks the widget tree from the given navigation widget and builds a
URI string."
  (if navigation-widget
      (with-slots (current-pane) navigation-widget
	(format nil "/~A~A"
		current-pane
		(obtain-uri-from-navigation
		 (find-navigation-widget
		  (current-pane-widget navigation-widget)))))
      "/"))

(defun find-navigation-widget (comp)
  "Given a composite 'comp', returns the first navigation widget
contained in 'comp' or its children."
  (when (null comp)
    (return-from find-navigation-widget))
  (when (typep comp 'navigation)
    (return-from find-navigation-widget comp))
  (car (flatten (remove-if #'null
			   (mapcar (lambda (w)
				     (typecase w
				       (navigation w)
				       (composite (find-navigation-widget w))
				       (otherwise nil)))
				   (composite-widgets comp))))))

(defun reset-navigation-widgets (nav)
  "Resets all navigation widgets from 'nav' down, using
'reset-current-pane'."
  (unless (null nav)
    (reset-current-pane nav)
    (reset-navigation-widgets (find-navigation-widget (current-pane-widget nav)))))

