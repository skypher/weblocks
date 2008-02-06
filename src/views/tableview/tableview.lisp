
(in-package :weblocks)

(export '(*render-empty-sequence-string* table table-view
	  table-view-default-summary table-view-caption
	  table-view-empty-message table-view-header-row-prefix-fn
	  table-view-header-row-suffix-fn table-view-row-prefix-fn
	  table-view-row-suffix-fn table-view-field
	  with-table-view-header with-table-view-header-row
	  render-table-view-header-row render-view-field-header
	  render-view-field-header-value with-table-view-body-row
	  render-table-view-body-row))

;;; Some parameters
(defparameter *render-empty-sequence-string* "No information available."
  "The default string used by the table view to signify that there is
no information available.")

;;; Table view
(defclass table-view (view)
  ((default-summary :initform nil
                    :initarg :summary
                    :accessor table-view-default-summary
                    :documentation "A summary string to be used for
	            the table if no :summary keyword is provided.")
   (caption :initform nil
	    :initarg :caption
	    :accessor table-view-caption
	    :documentation "A caption string to be used for the
	    table.")
   (empty-message :initform *render-empty-sequence-string*
		  :initarg :empty-message
		  :accessor table-view-empty-message
		  :documentation "See
		  *render-empty-sequence-string*.")
   (header-row-prefix-fn :initform nil
			 :initarg :header-row-prefix-fn
			 :accessor table-view-header-row-prefix-fn
			 :documentation "A function called prior to
	                 rendering the table header row. The function
	                 should expect the view object, the object
	                 being rendered, and any additional arguments
	                 passed to the view.")
   (header-row-suffix-fn :initform nil
			 :initarg :header-row-suffix-fn
			 :accessor table-view-header-row-suffix-fn
			 :documentation "A function called after
	                 rendering the header row. The function should
	                 expect the view object, the object being
	                 rendered, and any additional arguments passed
	                 to the view.")
   (row-prefix-fn :initform nil
		  :initarg :row-prefix-fn
		  :accessor table-view-row-prefix-fn
		  :documentation "A function called prior to rendering
	          each table row. The function should expect the view
	          object, the object being rendered, and any
	          additional arguments passed to the view.")
   (row-suffix-fn :initform nil
		  :initarg :row-suffix-fn
		  :accessor table-view-row-suffix-fn
		  :documentation "A function called after rendering
	          each table row. The function should expect the view
	          object, the object being rendered, and any
	          additional arguments passed to the view."))
  (:documentation "A view designed to present sequences of object in a
  table to the user."))

;;; Table view field
(defclass table-view-field (inline-view-field)
  ((presentation :initform (make-instance 'text-presentation)))
  (:documentation "A field class representing a column in the table
  view."))

;; Table heading
(defmethod with-view-header ((view table-view) obj widget body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (let* ((object-name (object-class-name (car obj)))
	 (header-class (format nil "view table ~A"
			       (if (eql object-name 'null)
				   "empty-table"
				   (attributize-name object-name)))))
    (with-html
      (:div :class header-class
	    (with-extra-tags
	      (safe-apply fields-prefix-fn view obj args)
	      (apply body-fn view obj args)
	      (safe-apply fields-suffix-fn view obj args))))))

(defgeneric with-table-view-header (view obj widget header-fn rows-fn &rest args
					 &key summary &allow-other-keys)
  (:documentation "Table specific header responsible for rendering
table, thead, and tbody HTML.")
  (:method ((view table-view) obj widget header-fn rows-fn &rest args
	    &key summary &allow-other-keys)
    (with-html
      (:table :summary (or summary (table-view-default-summary view))
	      (when (table-view-caption view)
		(htm (:caption (str (table-view-caption view)))))
	      (htm
	       (:thead
		(apply header-fn view (car obj) widget args))
	       (:tbody
		(apply rows-fn view obj widget args)))))))

;; Table header row
(defgeneric with-table-view-header-row (view obj widget &rest args)
  (:documentation
   "Used by table view to render header rows. This functions calls
'render-table-view-header-row' to render the header cells. Specialize
this function to modify HTML around a given header row's cells.")
  (:method ((view table-view) obj widget &rest args)
    (safe-apply (table-view-header-row-prefix-fn view) view obj args)
    (with-html
      (:tr (apply #'render-table-view-header-row view obj widget args)))
    (safe-apply (table-view-header-row-suffix-fn view) view obj args)))

(defgeneric render-table-view-header-row (view obj widget &rest args)
  (:documentation
   "Renders the row in the 'thead' element of the table. The default
implementation uses 'render-view-field-header' to render particular
cells. Specialize this method to achieve customized header row
rendering.")
  (:method ((view table-view) obj widget &rest args)
    (apply #'map-view-fields
	   (lambda (field-info)
	     (let ((field (field-info-field field-info))
		   (obj (field-info-object field-info)))
	       (apply #'render-view-field-header
		      field view widget (view-field-presentation field)
		      (obtain-view-field-value field obj) obj
		      :field-info field-info
		      args)))
	   view obj args)))

(defgeneric render-view-field-header (field view widget presentation value obj &rest args)
  (:documentation "Renders a table header cell.")
  (:method ((field table-view-field) (view table-view) widget presentation value obj &rest args)
    (with-html
      (:th :class (when (view-field-slot-name field)
		    (attributize-name (view-field-slot-name field)))
	   (apply #'render-view-field-header-value value presentation field view widget obj args)))))

(defgeneric render-view-field-header-value (value presentation field view widget obj &rest args)
  (:documentation "Renders a table header cell value.")
  (:method (value presentation (field table-view-field) (view table-view) widget obj &rest args)
    (declare (ignore args))
    (with-html
      (str (view-field-label field)))))

;; Table body
(defgeneric with-table-view-body-row (view obj widget &rest args &key alternp &allow-other-keys)
  (:documentation
   "Used by table view to render body rows. Specialize this function
to modify HTML around a given row's cells.")
  (:method ((view table-view) obj widget &rest args &key alternp &allow-other-keys)
    (safe-apply (table-view-row-prefix-fn view) view obj args)
    (with-html
      (:tr :class (if alternp "altern" nil)
	   (apply #'render-table-view-body-row view obj widget args)))
    (safe-apply (table-view-row-suffix-fn view) view obj args)))

(defgeneric render-table-view-body-row (view obj widget &rest args)
  (:documentation
   "Renders the rows in the 'tbody' element of the table. The
default implementation uses 'render-table-body-cell' to render
particular cells. See 'render-table-header-row' for more
details.")
  (:method ((view table-view) obj widget &rest args)
    (apply #'map-view-fields
	   (lambda (field-info)
	     (let ((field (field-info-field field-info))
		   (obj (field-info-object field-info)))
	       (safe-apply (view-field-prefix-fn field) view field obj args)
	       (apply #'render-view-field
		      field view widget (view-field-presentation field)
		      (obtain-view-field-value field obj) obj
		      args)
	       (safe-apply (view-field-suffix-fn field) view field obj args)))
	   view obj args)))

(defmethod render-view-field ((field table-view-field) (view table-view)
			      widget presentation value obj
			      &rest args)
  (with-html
    (:td :class (when (view-field-slot-name field)
		  (attributize-name (view-field-slot-name field)))
	 (apply #'render-view-field-value value presentation field view widget obj args))))

;; The table itself
(defmethod render-object-view-impl ((obj sequence) (view table-view) widget &rest args &key
				    (fields-prefix-fn (view-fields-default-prefix-fn view))
				    (fields-suffix-fn (view-fields-default-suffix-fn view))
				    &allow-other-keys)
  (apply #'with-view-header view obj widget
	 (lambda (view obj &rest args)
	   (apply #'with-table-view-header view obj widget
		  (lambda (view obj widget &rest args)
		    (safe-apply fields-prefix-fn view obj args)
		    (apply #'with-table-view-header-row view obj widget args))
		  (lambda (view obj widget &rest args)
		    (let ((row-num -1))
		      (mapc (lambda (obj)
			      (apply #'with-table-view-body-row view obj
				     widget
				     :alternp (oddp (incf row-num))
				     args))
			    obj))
		    (safe-apply fields-suffix-fn view obj args))
		  args))
	 args))

(defmethod render-object-view-impl ((obj null) (view table-view) widget &rest args)
  (apply #'with-view-header view obj widget
	 (lambda (view obj &rest args)
	   (declare (ignore obj args))
	   (with-html
	     (:p (if (table-view-caption view)
		     (htm (:span :class "caption" (str (table-view-caption view)) ":&nbsp;")))
		 (:span :class "message" (str (table-view-empty-message view))))))
	 args))
