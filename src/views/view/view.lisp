
(in-package :weblocks)

(export '(view view-inherit-from view-fields
	  view-fields-default-prefix-fn view-fields-default-suffix-fn
	  view-field view-field-slot-name view-field-reader
	  view-field-hide-p view-field-prefix-fn view-field-suffix-fn
	  view-caption inline-view-field view-field-label
	  view-field-presentation mixin mixin-view-field
	  mixin-view-field-view mixin-view-field-init-form
	  with-view-header render-view-field render-view-field-value
	  print-view-field-value entity-class-name view-class-name
	  view-default-field-type view-field-class-name
	  presentation-class-name *form-submit-dependencies*))

;;; Compiled views
(defvar *views* (make-hash-table)
  "A hashtable that stores view instances keyed by their name.")

(defvar *form-submit-dependencies* nil
  "Dynamic javascript dependencies -- code that form elements need to
  run before form submission, and that therefore needs to be placed in
  the onsubmit handler of the entire form.")

;;; View description
(defclass view ()
  ((inherit-from :initform nil
		 :initarg :inherit-from
		 :accessor view-inherit-from
		 :documentation "A view to inherit from. Possible
		 values include scaffold views (in which case a
		 default scaffolding view will be used), a custom view
		 name, or NIL. Specific views should inherit from an
		 appropriate scaffold view by default.")
   (fields :initform nil
	   :initarg :fields
	   :accessor view-fields
	   :documentation "A list of objects of class 'view-field',
	   used to describe the fields that the view will render.")
   (default-fields-prefix-fn
     :initform nil
     :initarg :default-fields-prefix-fn
     :accessor view-fields-default-prefix-fn
     :documentation "A function called prior to rendering the
     fields. The function should expect the view object, the object
     being rendered, and any additional arguments passed to the
     view.")
   (default-fields-suffix-fn
     :initform nil
     :initarg :default-fields-suffix-fn
     :accessor view-fields-default-suffix-fn
     :documentation "A function called after rendering the fields. The
     function should expect the view object, the object being
     rendered, and any additional arguments passed to the view.")
   (caption :initform nil
	    :initarg :caption
	    :accessor view-caption
	    :documentation "A caption string to be used for the
	    view. If this field is set to NIL (the default), each view
	    may use a specialized caption."))
  (:documentation "A meta description of the user interface."))

;;; View field description
(defclass view-field ()
  ((slot-name :initform nil
	      :initarg :slot-name
	      :accessor view-field-slot-name
	      :documentation "The name of the slot that this field
	      represents. This value can be null, in which case the
	      field does not correspond to any slot.")
   (reader :initarg :reader
	   :accessor view-field-reader
	   :documentation "If this slot is bound to a function object,
	   the function will be called with the object being rendered
	   as argument, and its return value will be used as the value
	   of the field. If this slot is not bound to a function
	   object but another value, the value will be used to render
	   the value of the field as a convinience. If the slot is not
	   bound, 'slot-value' will be used.")
   (hidep :initform nil
	  :initarg :hidep
	  :accessor view-field-hide-p
	  :documentation "A predicate that determines whether the
	  field is to be hidden from the user. If set to true, the
	  field will not be rendered.")
   (prefix-fn :initform nil
	      :initarg :prefix-fn
	      :accessor view-field-prefix-fn
	      :documentation "A function called prior to rendering the
	      field. The function should expect the view object, the
	      field object, the object being rendered, and any
	      additional arguments passed to the view.")
   (suffix-fn :initform nil
	      :initarg :suffix-fn
	      :accessor view-field-suffix-fn
	      :documentation "A function called after rendering the
	      field. The function should expect the view object, the
	      field object, the object being rendered, and any
	      additional arguments passed to the view."))
  (:documentation "Contains a meta description of a given field in the
  view."))

;;; Inline view
(defclass inline-view-field (view-field)
  ((label :initform nil
	  :initarg :label
	  :accessor view-field-label
	  :documentation "A human readable label that will be used to
	  render the field. By default this value is set to a
	  humanized form of 'slot-name'.")
   (presentation :initform nil
		 :initarg :present-as
		 :accessor view-field-presentation
		 :documentation "A presentation object to be used to
	         render this field. If not specified, the default
	         presentation for the type of the view will be
	         used. In addition, scaffold views will attempt to
	         determine the default presentation from the value of
	         the slot type, if one exists."))
  (:documentation "Inline fields are fields that are rendered as part
  of the view."))

(defmethod initialize-instance :after ((obj inline-view-field) &rest initargs &key)
  (declare (ignore initargs))
  (with-slots (slot-name label) obj
    (unless label
      (setf label (humanize-name slot-name)))))

;;; Mixin view
(defclass mixin-view-field (view-field)
  ((view :initform nil
	 :initarg :view
	 :accessor mixin-view-field-view
	 :documentation "A mixin view to be used to render the
	 field.")
   (initform :initform nil
	     :initarg :initform
	     :accessor mixin-view-field-init-form
	     :documentation "If the object being mixed in is null, the
	     object produced by this form will be used instead."))
  (:documentation "Mixin fields render a field with another view."))

;;; View rendering protocol
(defgeneric with-view-header (view obj widget body-fn &rest args)
  (:documentation "Renders header and footer around the body of the
view. Specialize this function to customize rendering header and
footer of a view."))

(defgeneric render-view-field (field view widget presentation value obj &rest args)
  (:documentation "Renders a given view field. This function should
render appropriate structural markup, and call render-view-field-value
to render the actual value of the field. Specialize this function to
customize the way individual fields are rendered."))

(defgeneric render-view-field-value (value presentation field view widget obj &rest args)
  (:documentation "Renders 'value' obtained from 'field' using
'presentation'. If this function requires a textual representation of
the value, it should call 'print-view-field-value'. Specialize this
function to add ways to present data to users."))

(defgeneric print-view-field-value (value presentation field view widget obj &rest args)
  (:documentation "Converts a value to a textual representation.
Specialize this function to change the way a value is printed in
views."))

;;; Declarative view definition protocol
(defun entity-class-name (entity-type suffix)
  "A helper function that generates a class name from an entity name
and a suffix."
  (declare (optimize safety))
  (check-type entity-type (not keyword))
  (let ((entity-symbol (concatenate 'string (symbol-name entity-type)
				    (symbol-name suffix))))
    (or (find-own-symbol entity-symbol
			 (symbol-package entity-type))
	(and (boundp '*current-webapp*)
	     (find-own-symbol entity-symbol
			      (symbol-package
			       (class-name (class-of (current-webapp))))))
	(find-own-symbol entity-symbol '#:weblocks)
	(find-if-not #'keywordp (find-all-symbols entity-symbol))
	(error "Class ~A cannot be found." entity-symbol))))

(defgeneric view-class-name (view-type)
  (:documentation "Given a view 'type', converts it to the class name
of the view. Default implementation adds '-view' to the type and
returns the symbol.")
  (:method (view-type)
    (entity-class-name view-type '#:-view)))

(defgeneric view-default-field-type (view-type field-type)
  (:documentation "Given 'view-type' and 'field-type', returns the
type of the fields of the view. Default implementation returns
'field-type' if present, otherwise 'view-type'.")
  (:method (view-type field-type)
    (or field-type view-type)))

(defgeneric view-field-class-name (field-type)
  (:documentation "Given a type of the field, returns its class
name. Default implementation adds '-view-field' to the type and
returns the symbol.")
  (:method (field-type)
    (entity-class-name field-type '#:-view-field)))

(defgeneric presentation-class-name (presentation-type)
  (:documentation "Given a type of the presentation, returns its class
name. Default implementation adds '-presentation' to the type and
returns the symbol.")
  (:method (presentation-type)
    (entity-class-name presentation-type '#:-presentation)))

;; Views by default only report their fields' dependencies. This
;; method shouldn't really be an append method: for subclasses of view
;; we might get multiple field dependencies. It's here for the sake of
;; consistency, otherwise we would have to define a separate
;; view-dependencies method with a different combination.
(defmethod dependencies append ((view view))
  (mapappend #'dependencies (view-fields view)))

;; as for now, plain view-fields have no dependencies, but this could change
(defmethod dependencies append ((field view-field))
  ())

;; dependencies of an inline-view-field are really dependencies of the
;; presentation it contains
(defmethod dependencies append ((field inline-view-field))
  (dependencies (view-field-presentation field)))
