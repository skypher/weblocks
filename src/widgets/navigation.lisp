
(in-package :weblocks)

(export '(navigation navigation-panes navigation-on-find-pane
	  navigation-current-pane navigation-render-menu-p pane-name
	  pane-widget current-pane *current-navigation-url*
	  navigation-default-pane with-navigation-header
	  render-navigation-body current-pane-widget init-navigation
	  make-navigation find-pane reset-current-pane))

(defwidget navigation (widget)
  ((panes :accessor navigation-panes
	  :initform nil
	  :initarg :panes
	  :documentation "An association list of names and
	  widgets. The names will act as menu entries and attributized
	  names will go into the URL. When a particular entry is
	  clicked, its corresponding pane will be rendered.")
   (on-find-pane :accessor navigation-on-find-pane
		 :initform nil
		 :initarg :on-find-pane
		 :documentation "If bound to a function, and the url
		 does not point to one of the panes in
		 'navigation-panes', the function is called with two
		 arguments (navigation object, and the pane name). If
		 the function returns a widget, uses the widget to
		 present the URL. If the function returns nil,
		 assummes the pane does not exist. This can be used
		 for wiki-style URLs. Note, the returned widget will
		 be cached in 'dynamic-pane-cache'. To turn off
		 caching the widget, the function should return a
		 second value :no-cache.")
   (dynamic-pane-cache :accessor navigation-dynamic-pane-cache
		       :initform nil
		       :documentation "If a pane is generated
		       dynamically via 'on-find-pane', it is cached in
		       this slot. This is done to maintain the same
		       instance of the generated widget accross
		       non-ajax requests.")
   (current-pane :accessor navigation-current-pane
		 :initform nil
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

(defun pane-name (pane)
  "Given a pane, returns its name."
  (car pane))

(defun pane-widget (pane)
  "Given a pane, returns a widget it represents."
  (cdr pane))

(defgeneric navigation-default-pane (obj)
  (:documentation
   "Must return the name of the default pane for the navigation
object. The default implementation returns the first pane from
'navigation-panes'. Specialize this function to specify how to pick
default panes for your navigation object.")
  (:method ((obj navigation))
    (pane-name (car (navigation-panes obj)))))

;;; During initialization, current pane will automatically be set to
;;; the first available pane in the list, unless specified otherwise.
(defmethod initialize-instance :after ((obj navigation) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (null (navigation-current-pane obj))
    (setf (navigation-current-pane obj)
	  (navigation-default-pane obj)))
  ;; A navigation widget doesn't have a random name assigned to
  ;; automatically because the name is also displayed to the user.
  (unless (slot-boundp obj 'dom-id)
    (setf (slot-value obj 'dom-id) nil)))

(defgeneric with-navigation-header (obj body-fn &rest args)
  (:documentation
   "Renders the header of the navigation widget. Unlike
'with-widget-header', which in case of the navigation widget wraps the
current pane as well as the navigation html, 'with-navigation-header'
only wraps navigation html.")
  (:method ((obj navigation) body-fn &rest args)
    (when (navigation-render-menu-p obj)
      (with-html
	(:div :class "view menu"
	      (with-extra-tags
		(if (null (navigation-panes obj))
		    (htm
		     (:div :class "empty-navigation" "No navigation entries"))
		    (htm
		     (:h1 (if (widget-name obj)
			      (str (humanize-name (widget-name obj)))
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
determine its location in order to properly render paths in links.")
  (:method ((obj navigation) &rest args)
    (declare (ignore args))
    (with-html
      (mapc (lambda (pane)
	      (let* ((pane-name (pane-name pane))
		     (pane-selected-p (equalp pane-name (navigation-current-pane obj)))
		     (pane-class (when pane-selected-p
				   "selected-item")))
		(htm
		 (:li :class pane-class
		      (if pane-selected-p
			  (htm (:span (str (humanize-name pane-name))))
			  (htm (:a :href (concatenate 'string *current-navigation-url*
						      (unless (equalp pane-name
								      (navigation-default-pane obj))
							(string-downcase
							 (url-encode (attributize-name pane-name)))))
				   (str (humanize-name pane-name)))))))))
	    (navigation-panes obj)))))

(defmethod render-widget-body ((obj navigation) &rest args)
  (if (current-pane-widget obj)
      (let ((*current-navigation-url* (concatenate 'string
						   *current-navigation-url*
						   (string-downcase
						    (url-encode (navigation-current-pane obj)))
						   "/")))
	(declare (special *current-navigation-url*))
	(render-widget (current-pane-widget obj)))
      (setf (return-code) +http-not-found+))
  (apply #'with-navigation-header obj #'render-navigation-body args))

(defun current-pane-widget (obj)
  "Accepts a navigation object and returns the widget that represents its
currently selected pane."
  (pane-widget (find-pane obj (navigation-current-pane obj))))

(defun init-navigation (obj &rest args)
  "A helper function to create a navigation widget

ex:

\(init-navigation
   \"test1\" (make-instance ...)
   \"test2\" (make-instance ...)"
  (loop for count from 1
        for x in args
        for y in (cdr args)
     when (oddp count)
     do (push-end `(,(attributize-name x) . ,y) (navigation-panes obj)))
  (when (null (navigation-current-pane obj))
    (setf (navigation-current-pane obj)
	  (navigation-default-pane obj)))
  obj)

(defun make-navigation (name &rest args)
  "Instantiates 'navigation' widget via 'make-instance' and forwards
it along with 'args' to 'init-navigation'."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))

(defun find-pane (obj name)
  "Returns a cons cell identifying the navigation pane if it exists,
otherwise returns nil. This function first looks through static panes,
then through the cached dynamic pane if it exists, and finally calls
'on-find-pane' if it's bound to a function."
  (or (find (attributize-name name)
	    (navigation-panes obj) :key #'pane-name :test #'equalp)
      (when (equalp (pane-name (navigation-dynamic-pane-cache obj))
		    name)
	(navigation-dynamic-pane-cache obj))
      (when (navigation-on-find-pane obj)
	(multiple-value-bind (w caching)
	    (funcall (navigation-on-find-pane obj)
		     obj name)
	  (when w
	    (if (eq caching :no-cache)
		(cons name w)
		(setf (navigation-dynamic-pane-cache obj)
		      (cons name w))))))))

(defun reset-current-pane (obj)
  "Sets the current pane to the first available pane in the list."
  (setf (navigation-current-pane obj)
	(navigation-default-pane obj)))

(defmethod find-widget-by-path* (path (root navigation))
  (find-widget-by-path* (cdr path)
			(pane-widget (find-pane root (car path)))))

;;; Code that implements friendly URLs ;;;
(defun apply-uri-to-navigation (tokens obj)
  "Takes URI tokens and applies them one by one to navigation widgets
in order to allow for friendly URLs. The URLs are basically intimately
linked to navigation controls to simulate document resources on the
server."
  (when (null tokens)
    (reset-navigation-widgets obj)
    (return-from apply-uri-to-navigation))
  (let ((first-token (url-decode (car tokens))))
    (if (and obj (find-pane obj first-token))
	(progn
	  (setf (navigation-current-pane obj) first-token)
	  (apply-uri-to-navigation (cdr tokens)
				   (find-navigation-widget (current-pane-widget obj))))
	(setf (return-code) +http-not-found+))))

(defun obtain-uri-from-navigation (obj)
  "Walks the widget tree from the given navigation widget and builds a
URI string."
  (if obj
      (format nil "/~A~A"
	      (navigation-current-pane obj)
	      (obtain-uri-from-navigation
	       (find-navigation-widget
		(current-pane-widget obj))))
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

(defun reset-navigation-widgets (obj)
  "Resets all navigation widgets from 'obj' down, using
'reset-current-pane'."
  (unless (null obj)
    (reset-current-pane obj)
    (reset-navigation-widgets (find-navigation-widget (current-pane-widget obj)))))

