
(in-package :weblocks)

(export '(data-editor dataform-data dataform-class-store
	  dataform-on-cancel dataform-on-success
	  dataform-allow-close-p dataform-on-close
	  data-editor-form-buttons render-dataform-data-buttons))

(defwidget data-editor ()
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data
	 :documentation "Data object rendered and modified by
	 this widget.")
   (class-store :accessor dataform-class-store
		:initform nil
		:initarg :class-store
		:documentation "A store that will be used for
		persisting the data object. If this slot isn't
		specified, the value is obtained by calling
		class-store on the data class.")
   (on-cancel :accessor dataform-on-cancel
	      :initform nil
	      :initarg :on-cancel
	      :documentation "An optional callback function with one
	      argument (the dataform widget). Called when the user
	      presses a cancel button.")
   (on-success :accessor dataform-on-success
	       :initform nil
	       :initarg :on-success
	       :documentation "An optional callback function with one
	       argument (the dataform widget). Called when the user
	       successfully submitted data that passed through the
	       validation stage.")
   (allow-close-p :accessor dataform-allow-close-p
		  :initform t
		  :initarg :allow-close-p
		  :documentation "If set to true (the default), and
		  'on-close' isn't nil, renders a close button.")
   (on-close :accessor dataform-on-close
	     :initform nil
	     :initarg :on-close
	     :documentation "An optional callback function with one
	     argument (the dataform widget). Called when the user
	     clicks on the close button. Note that the close button is
	     only rendered if 'allow-close-p' is true.")
   (form-buttons :initform nil
                 :initarg :form-buttons
                 :accessor data-editor-form-buttons
                 :documentation "Same as `form-view-buttons'. If not null,
	    used to override the value of `form-view-buttons' for the
	    view being rendered (by passing :form-view-buttons arg to
	    the view)."))
  (:documentation "The details of stateful handling of forms
  manipulating objects.  Mix this in to drop-in your own replacement
  for `dataform', when the view DSL isn't expressive enough and you
  need widgets instead."))

(defmethod initialize-instance :after ((obj data-editor) &key &allow-other-keys)
  "Default `class-store' to my data's `object-store'."
  (unless (dataform-class-store obj)
    (setf (dataform-class-store obj)
	  (object-store (dataform-data obj)))))

(defgeneric (setf dataform-ui-state) (new-value data-editor)
  (:documentation "When a dataform or similar, change DATA-EDITOR's
  state to NEW-VALUE, which should be :data or :form.")
  (:method (new-value (wij data-editor))
    (style-warn 'misunderstood-action
      :action (format nil "changed a dataform's state to ~S" new-value)
      :missing "`dataform-ui-state' accessor implementation")
    new-value))

(defgeneric render-dataform-data-buttons (dataform data)
  (:documentation "Render the buttons and links appearing with the
  data view on a dataform."))

(defmethod render-dataform-data-buttons ((obj data-editor) data)
  "Show the Modify and Close buttons as permitted by OBJ."
  (declare (ignore data))
  (with-html
    (:div :class "submit"
	  (render-link (make-action
			(f_% (setf (dataform-ui-state obj) :form)))
		       "Modify"
		       :class "modify")
	  (when (and (dataform-allow-close-p obj)
		     (dataform-on-close obj))
	    (str "&nbsp;")
	    (render-link (make-action
			  (f_% (funcall (dataform-on-close obj) obj)))
			 "Close"
			 :class "close")))))
