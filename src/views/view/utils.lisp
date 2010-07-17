
(in-package :weblocks)

(export '(find-view field-info field-info-field field-info-object
	  field-info-path get-object-view-fields map-view-fields
	  find-field-info find-view-field map-mixin-fields
	  count-view-fields obtain-view-field-value render-object-view
	  class-from-view render-view render-object-view-impl
	  attributize-presentation attributize-view-field-name))

;;; View rendering utils
(defun find-view (view &optional (signal-error-p t))
  "Finds a view. If 'view' is a symbol, looks up the appropriate view
object. If 'view' is a view object, simply returns it. Otherwise,
signals an error.

If 'view' is a list, finds a scaffold class by calling
'scaffold-class-name' and builds an appropriate scaffold view by
calling 'generate-scaffold-view' with the scaffold class name and the
second argument."
  (or (etypecase view
	(list (generate-scaffold-view (make-instance (scaffold-class-name (first view)))
				      (find-class (second view))))
	(symbol (gethash view *views*))
	(view view))
      (when signal-error-p
	(error "Cannot find view ~A" view))))

(defstruct field-info
  "A structure that holds information about a given field. Information
includes the field instance, the object whose slot the field
represents, and the path to the slot the field represents from the
root object being evaluated, and the field-info of the field's parent
if it was mixed into the view."
  field object parent-info)

(defun inserting-custom-fields (obj proc custom-fields)
  "Wrap PROC, a `map-view-fields' candidate, with a variant that
inserts each of CUSTOM-FIELDS as defined by `get-object-view-fields'.
Secondary, answer a termination thunk."
  (unless custom-fields
    (return-from inserting-custom-fields (values proc (constantly nil))))
  (multiple-value-bind (posned-customs end-customs)
      (partition custom-fields #'consp)
    (setf posned-customs (stable-sort posned-customs #'< :key #'car))
    (let ((posn -1))
      (labels ((custom-field->field-info (custom-field)
		 (etypecase custom-field
		   (field-info custom-field)
		   (view-field (make-field-info :field custom-field
						:object obj
						:parent-info nil))))
	       (wrapper (vfield-info)
		 (incf posn)
		 (loop while (and posned-customs
				  (<= (caar posned-customs) posn))
		       do (funcall proc (custom-field->field-info
					 (cdr (pop posned-customs))))
			  (incf posn))
		 (funcall proc vfield-info)))
	(values #'wrapper
		(f0 (mapc (compose #'wrapper #'custom-field->field-info)
			  end-customs)
		    (mapc (compose proc #'custom-field->field-info #'cdr)
			  posned-customs)))))))

(defun %map-object-view-fields (proc obj view-designator
				&key include-invisible-p (expand-mixins t)
				custom-fields &allow-other-keys)
  "Implement `get-object-view-fields', except for consing up the
result list, instead calling PROC on each resulting `field-info'."
  (labels ((map-level-fields (proc view)
	     (let ((view (or (and view (find-view view))
			     (return-from map-level-fields))))
	       (map-level-fields proc (view-inherit-from view))
	       (dolist (vfield (view-fields view))
		 (funcall proc vfield))))
	   (map-level (obj view mixin-container)
	     (let ((vfields (make-hash-table :test 'eq)))
	       ;; prefer latest
	       (map-level-fields
		(f_ (setf (gethash (view-field-slot-name _) vfields) _))
		view)
	       (map-level-fields
		(lambda (vfield)
		  ;; we only use the in-order vfield as a tag into the
		  ;; vfields HT, taking first-instance-only
		  (setf vfield (gethash (view-field-slot-name vfield) vfields))
		  (when vfield
		    (let ((vfield-info (make-field-info
					:field vfield :object obj
					:parent-info mixin-container)))
		      (etypecase vfield
			(inline-view-field
			   (when (or include-invisible-p
				     (not (let ((hide-p-value (view-field-hide-p vfield)))
					    (if (functionp hide-p-value)
						(funcall hide-p-value obj)
						hide-p-value))))
			     (funcall proc vfield-info)))
			(mixin-view-field
			   (if expand-mixins
			       (map-level
				(and obj
				     (or (obtain-view-field-value vfield obj)
					 (aif (mixin-view-field-init-form vfield)
                                           (funcall it)
                                           (error "Slot for mixin field ~S has neither value nor initform!" vfield))))
				(mixin-view-field-view vfield) vfield-info)
			       (funcall proc vfield-info)))))
		    ;; avoid duplicates
		    (remhash (view-field-slot-name vfield) vfields)))
		view))))
    (multiple-value-bind (wproc terminate-proc)
	(inserting-custom-fields obj proc custom-fields)
      (setf proc wproc)
      (map-level obj view-designator nil)
      (funcall terminate-proc))))

(defun get-object-view-fields (obj view-designator &rest args
			       &key include-invisible-p (expand-mixins t) custom-fields
			       &allow-other-keys)
  "Returns a list of 'field-info' structures. If 'include-invisible-p'
is set to true, fields declared as invisible will be returned as
well.

If 'expand-mixins' is set to true (default), mixin fields will be
expanded into inline fields, and will not be present in the results.

If 'custom-fields' is not null, it is expected to be a list with each
element being either a custom field, or a cons cells. If an element is
a custom field, the field is returned at the end of other fields. If
an element is a cons cell, CAR is expected to be a positive integer,
and CDR is expected to be a custom-field. In this case the
custom-field is inserted before the element with the index in the CAR
of the cons cell.

Each custom field can be either a field-info structure or a
view-field. Field-info structures are inserted as is, and view-fields
are wrapped in field-info structures with common-sense defaults."
  (declare (ignore include-invisible-p expand-mixins custom-fields))
  (let ((expansion '()))
    (apply #'%map-object-view-fields
	   (f_ (push _ expansion)) obj view-designator args)
    (nreverse expansion)))

(defun map-view-fields (fn view obj &rest args)
  "Acts like mapcar for view fields. FN should expect a structure of
type field-info."
  (let ((expansion '()))
    (apply #'%map-object-view-fields
	   (f_ (push (funcall fn _) expansion)) obj view args)
    (nreverse expansion)))

(defun find-field-info (name view obj &rest govf-args)
  "Finds a field-info object by name and returns it."
  (let (field)
    (apply #'map-view-fields
	   (lambda (fi)
	     (when (equalp (view-field-slot-name (field-info-field fi))
			   name)
	       (setf field fi)))
	   view obj govf-args)
    field))

(defun find-view-field (&rest args)
  (let ((field-info (apply #'find-field-info args)))
    (when field-info
      (field-info-field field-info))))

(defun map-mixin-fields (fn view obj &rest govf-args)
  (apply #'%map-object-view-fields
	 (lambda (field-info)
	   (when (typep (field-info-field field-info) 'mixin-view-field)
	     (funcall fn field-info)))
	 obj view :expand-mixins nil govf-args))

(defun count-view-fields (view &rest govf-args)
  "Counts the number of fields in a given view."
  (let ((count 0))
    (apply #'%map-object-view-fields (f_% (incf count)) nil view govf-args)
    count))

(defun slot-reader (class slot-name)
  "Returns a reader, if one is defined, on the slot."
  (let ((slot-dsd (find-slot-dsd class slot-name)))
    (when slot-dsd
      (car (slot-definition-readers slot-dsd)))))

(defun obtain-view-field-value (field obj)
  "Obtains the view field value of object. See documentation for
'reader' slot of 'field' for more details."
  (declare (optimize safety))
  (assert (or (view-field-slot-name field)
	      (slot-boundp field 'reader))
	  nil "Either the field must represent a slot, or it's READER slot must be bound.")
  (let* ((reader-bound-p (slot-boundp field 'reader))
	 (reader (if reader-bound-p
		     (view-field-reader field)
		     (when (view-field-slot-name field)
		       (or (slot-reader (class-of obj) (view-field-slot-name field))
			   (curry-after #'slot-value (view-field-slot-name field)))))))
    (if (or (functionp reader)
	    (and (symbolp reader)
		 (fboundp reader)))
	(handler-case (funcall reader obj)
	  (unbound-slot () nil))
	reader)))

(defun render-object-view (obj view &rest args
			   &key widget &allow-other-keys)
  "A helper function that finds the view and calls
'render-object-view-impl'. Additionally, calls 'dependencies' and adds
the returned items to *page-dependencies*. This is later used by
Weblocks to declare stylesheets and javascript links in the page
header."
  (declare (special *page-dependencies*))
  (setf *page-dependencies*
	(append *page-dependencies* (dependencies (find-view view))))
  (let (*form-submit-dependencies*)
    (declare (special *form-submit-dependencies*))
    ;; this is not the best place to introduce *form-submitp-dependencies*,
    ;; because it only applies to forms, but since render-object-view is
    ;; a function and not a method, we can't place an :around method
    ;; around it, and there is no other method that encapsulates all of
    ;; view rendering --jwr
    (apply #'render-object-view-impl obj (find-view view) widget args)))

(defun default-class-from-view-name (view)
  (if (symbolp view)
    (make-symbol (concatenate 'string (string view) "-OBJ-" (symbol-name (gensym))))
    (gensym)))

(defmethod class-from-view (view &optional (class-name
                                             (default-class-from-view-name view)))
  "A helper function that generates a class object from a view. The
view fields are enumerated and a CLOS class with slots based on field
names is generated.  This was made a method so the elephant backend
can intercept class names and provide proxies instead."
  (make-class
   (mapcar (lambda (field-info)
	     (view-field-slot-name (field-info-field field-info)))
	   (get-object-view-fields nil view))
   class-name))

(defun render-view (view &rest args &key (class-name (gensym)) &allow-other-keys)
  "A helper function that inspects the view, creates a fitting object
from it only the fly, and calls 'render-object-view'. The function
returns the created object."
  (let ((obj (make-instance (class-from-view view class-name))))
    (apply #'render-object-view obj view args)
    obj))

(defgeneric render-object-view-impl (obj view widget &rest args)
  (:documentation "Renders 'obj' using 'view'.")
  (:method (obj view widget &rest args)
    (apply #'with-view-header view obj widget
	   (lambda (view obj &rest args)
	     (apply #'map-view-fields
		    (lambda (field-info)
		      (let ((field (field-info-field field-info))
			    (obj (field-info-object field-info)))
			(safe-apply (view-field-prefix-fn field) view field obj args)
			(apply #'render-view-field
			       field view widget (view-field-presentation field)
			       (obtain-view-field-value field obj) obj 
			       :field-info field-info
			       args)
			(safe-apply (view-field-suffix-fn field) view field obj args)))
		    view obj args))
	   args)))

(defun attributize-presentation (presentation)
  "Attributizes presentation name."
  (string-remove-right 
   (attributize-name
    (object-class-name
     presentation))
   "-presentation"))

(defmethod attributize-view-field-name ((field-info field-info))
  "Attributize a view field name from its FIELD-INFO structure."
  (let ((parent-prefix (awhen (field-info-parent-info field-info)
                              (view-field-slot-name (field-info-field it))))
        (name (view-field-slot-name (field-info-field field-info))))
    (when name
      (attributize-name (format nil "~@[~A-~]~A" parent-prefix name)))))

(defmethod print-object ((obj field-info) stream)
  (flet ((field-key (field-info &aux (field (field-info-field field-info)))
                    (cons (view-field-slot-name field) (awhen (field-info-parent-info field-info)
                                                              (view-field-slot-name (field-info-field it))))))
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~S" (field-key obj)))))

(defmethod print-object ((obj view-field) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S" (view-field-slot-name obj))))

