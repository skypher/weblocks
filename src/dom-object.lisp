(in-package :weblocks)

(export '(dom-object-mixin dom-id dom-class dom-classes))

(defclass dom-object-mixin ()
  ((dom-id :initarg :dom-id
	   :documentation "The DOM id of an object. Can be a symbol, a
	   string or nil. When accessed through the 'dom-id' accessor,
	   will always become a string. Use ensure-dom-id or
	   widget-name (for widgets) to access its underlying
	   implementation.")
   (dom-class :accessor dom-class :initform nil :initarg :dom-class
	      :documentation "The DOM class (CSS class) of an
	      object. Set this to a string if you'd like to add an
	      additional CSS class to the ones generated from the class
	      hierarchy by default."))
  (:documentation "Represents attributes and functionality common to all
  DOM-renderable objects."))

(defgeneric ensure-dom-id (obj)
  (:documentation "Ensure that the object has a 'dom-id' and return
  it. 'dom-id' is lazily generated on first read, because its creation
  requires a session to be present. Returns a string, symbol, or nil.")
  (:method ((obj dom-object-mixin))
    (if (slot-boundp obj 'dom-id)
	(slot-value obj 'dom-id)
	(when (boundp '*session*)
	  (setf (slot-value obj 'dom-id) (gen-id))))))

(defgeneric dom-id (obj)
  (:documentation "Provides a consistent interface to identifying widgets
by their DOM id. Returns a string or nil if the object is to have no id.")
  (:method ((obj dom-object-mixin))
    (let ((id (ensure-dom-id obj)))
      (and id (attributize-name id))))
  (:method ((obj symbol)) nil)
  (:method ((obj function)) nil)
  (:method ((obj string)) nil))

(defgeneric (setf dom-id) (id obj)
  (:method (id (obj dom-object-mixin))
    (with-slots (dom-id) obj
      (setf dom-id id))))

(defgeneric dom-classes (obj)
  (:documentation "Returns a string that represents all applicable CSS
classes for an object (usually a widget). Normally includes the class
name and the names of its subclasses. It is safe to assume that all
widgets will have a CSS class of 'widget'."))

(defmethod dom-classes ((obj dom-object-mixin))
  (format nil "~A~@[ ~A~]"
	  (apply #'concatenate 'string
		 (intersperse
		  (mapcar (compose #'attributize-name #'class-name)
			  (reverse
			   ;; we remove the dom-object-mixin from the list of classes, as it
			   ;; isn't too useful when styling widgets --jwr
			   (loop for i in (remove (find-class 'dom-object-mixin)
						  (moptilities:superclasses obj :proper? nil))
			      until (string-equal (class-name i) 'standard-object)
			      collect i)))
		  " "))
	  (dom-class obj)))

(defmethod dom-classes ((obj symbol))
  (format nil "widget function ~A" (attributize-name obj)))

(defmethod dom-classes ((obj function))
  "widget function")

(defmethod dom-classes ((obj string))
  "widget string")

