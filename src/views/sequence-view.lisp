
(in-package :weblocks)

(export '(*render-empty-sequence-string* sequence sequence-view
	  sequence-view-empty-message sequence-view-row-prefix-fn
	  sequence-view-row-suffix-fn view-field-sorting-mixin
	  view-field-sorting-mixin-order-by
	  view-field-sorting-mixin-allow-sorting-p sequence-view-field
	  mixin-sequence-view-field))

;;; Some parameters
(defparameter *render-empty-sequence-string* "No information available."
  "The default string used by the sequence view to signify that there
is no information available.")

;;; Sequence view
(defclass sequence-view (view)
  ((empty-message :initform *render-empty-sequence-string*
		  :initarg :empty-message
		  :accessor sequence-view-empty-message
		  :documentation "See
		  *render-empty-sequence-string*.")
   (row-prefix-fn :initform nil
		  :initarg :row-prefix-fn
		  :accessor sequence-view-row-prefix-fn
		  :documentation "A function called prior to rendering
	          each sequence item. The function should expect the view
	          object, the object being rendered, and any
	          additional arguments passed to the view.")
   (row-suffix-fn :initform nil
		  :initarg :row-suffix-fn
		  :accessor sequence-view-row-suffix-fn
		  :documentation "A function called after rendering
	          each sequence item. The function should expect the
	          view object, the object being rendered, and any
	          additional arguments passed to the view."))
  (:documentation "An abstract class that should be subclassed by
  views designed to present sequences of object in a sequence to the
  user."))

;;; Sorting mixin
(defclass view-field-sorting-mixin ()
  ((order-by :initarg :order-by
	     :accessor view-field-sorting-mixin-order-by
	     :documentation "If set to a symbol or a list of symbols,
	     this slot will be used to build an order-by path that is
	     later passed to the backend store for ordering the
	     entries of the sequence. If this slot is unbound (the
	     default), the 'slot-name' slot of the field will be used
	     instead.")
   (allow-sorting-p :initform t
		    :initarg :allow-sorting-p
		    :accessor view-field-sorting-mixin-allow-sorting-p
		    :documentation "If set to true (default), the
		    field will be allowed to be sorted."))
  (:documentation "This is a class that can be mixed into field
  definitions in order to provide functionality necessary to support
  dataseq widgets."))

;;; By default, turn off sorting on any field with a `reader', unless it also has
;;; an `order-by'.
(defmethod initialize-instance :after ((field view-field-sorting-mixin)
				       &key allow-sorting-p &allow-other-keys)
  (when (and (not allow-sorting-p)
	     (slot-boundp field 'reader)
	     (not (slot-boundp field 'order-by)))
    (setf (view-field-sorting-mixin-allow-sorting-p field) nil)))


;;; Sequence view field
(defclass sequence-view-field (inline-view-field view-field-sorting-mixin)
  ()
  (:documentation "An abstract view field class representing a slot of
  an item in a sequence of items. This class should be subclassed by
  view fields for views that represent sequences of data."))

;;; Sequence view mixin field
(defclass mixin-sequence-view-field (mixin-view-field view-field-sorting-mixin)
  ()
  (:documentation "An abstract view field class representing a mixin
  item in a sequence of items. This class should be subclassed by
  mixin view fields for views that represent sequences of data."))

;;; Make the scaffolding system happy
(defclass sequence-scaffold (scaffold)
  ())

(defmethod view-default-field-type ((view-type (eql 'sequence)) (field-type (eql 'mixin)))
  'mixin-sequence)

;;; Header
(defmethod with-view-header ((view sequence-view) obj widget body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (let* ((object-name (object-class-name (car obj)))
	 (header-class (format nil "view sequence ~A"
			       (if (eql object-name 'null)
				   "empty"
				   (attributize-name object-name)))))
    (with-html
      (:div :class header-class
	    (with-extra-tags
	      (safe-apply fields-prefix-fn view obj args)
	      (apply body-fn view obj args)
	      (safe-apply fields-suffix-fn view obj args))))))

;;; Empty sequence
(defmethod render-object-view-impl ((obj null) (view sequence-view) widget &rest args)
  (apply #'with-view-header view obj widget
	 (lambda (view obj &rest args)
	   (declare (ignore obj args))
	   (render-message (sequence-view-empty-message view) (view-caption view)))
	 args))

