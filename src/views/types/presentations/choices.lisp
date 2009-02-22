
(in-package :weblocks)

(export '(presentation-choices-default-label-key
	  presentation-choices-default-value-key
	  choices-presentation-mixin presentation-choices
	  obtain-presentation-choices presentation-choices-label-key
	  presentation-choices-value-key))

;;; Some defaults
(defgeneric presentation-choices-default-label-key (value)
  (:documentation
   "Default key for choice labels. See
'presentation-choices-label-key' for more details.")
  (:method (value)
    (humanize-name value))
  (:method ((value standard-object))
    (humanize-name (object-class-name value))))

(defgeneric presentation-choices-default-value-key (value)
  (:documentation
   "Default key for choice values. See
'presentation-choices-value-key' for more details.")
  (:method (value)
    (attributize-name value))
  (:method ((value standard-object))
    (attributize-name (object-id value))))

;;; Choices mixin
(defclass choices-presentation-mixin ()
  ((choices :initform nil
	    :initarg :choices
	    :accessor presentation-choices
	    :documentation "A list of choices a user may select
	    from. This value may be bound either to a list of choices,
	    or a function object that accepts the object being
	    rendered and returns such a list. The list may contain
	    single entries (in which case the label for each choice
	    will be obtained via 'label-key' and value via
	    'value-key'), or cons pairs, in which case label will be
	    obtained by car, and value via cdr.")
   (label-key :initform #'presentation-choices-default-label-key
	      :initarg :label-key
	      :accessor presentation-choices-label-key
	      :documentation "A function used to transform a choice
	      into a label. Default implementation transforms CLOS
	      objects via 'object-class-name', and other lisp objects
	      via 'humanize-name' (see
	      presentation-choices-default-label-key).")
   (value-key :initform #'presentation-choices-default-value-key
	      :initarg :value-key
	      :accessor presentation-choices-value-key
	      :documentation "A function used to transform a choice
	      into a value. Default implementation transforms CLOS
	      objects via 'object-id', and other lisp objects via
	      'attributize-name' (see
	      presentation-choices-default-value-key).")))

(defmethod obtain-presentation-choices ((choices-mixin choices-presentation-mixin) obj)
  "Accepts a 'choices-presentation-mixin' and an object being rendered
and returns a list as specified in the 'choices' slot."
  (labels ((obtain-presentation-choices-aux (choices obj)
	     (if (or (functionp choices)
		     (and (symbolp choices)
			  (fboundp choices)))
		 (obtain-presentation-choices-aux (funcall choices obj) obj)
		 (mapcar (lambda (choice)
			   (if (consp choice)
			       (cons (format nil "~A" (car choice))
				     (format nil "~A" (cdr choice)))
			       (cons (funcall (presentation-choices-label-key choices-mixin) choice)
				     (funcall (presentation-choices-value-key choices-mixin) choice))))
			 choices))))
    (obtain-presentation-choices-aux (presentation-choices choices-mixin) obj)))

