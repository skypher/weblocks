;;;; Generic table renderer
(in-package :weblocks)

(export '(*render-empty-sequence-string* with-table-header
	  render-table-header-row render-table-header-cell
	  render-table-body-row render-table-body-cell
	  render-table render-empty-table))

(defparameter *render-empty-sequence-string* "No information available."
  "The default string used by the table renderer to signify that
there is no information available.")

;; The usual div wrapper
(defgeneric with-table-header (obj body-fn)
  (:documentation
   "Responsible for rendering headers of a table
presentation. The default implementation renders appropriate
div's along with classes necessary for CSS styling. Look at
'with-data-header' for more details."))

(defmethod with-table-header (obj body-fn)
  (let* ((object-name (object-class-name obj))
	 (header-class (format nil "renderer table ~A"
			       (if (eql object-name 'null)
				   "empty-table"
				   (attributize-name object-name)))))
    (with-html
      (:div :class header-class
	    (with-extra-tags
	      (funcall body-fn))))))

;; Auxilary functions
(defun with-table-row (obj body-fn &key alternp &allow-other-keys)
  "Used internally by table renderer to render rows."
  (with-html
    (:tr :class (if alternp "altern" nil)
	 (funcall body-fn))))

;; Table header
(defgeneric render-table-header-row (obj &rest keys &key inlinep &allow-other-keys)
  (:documentation
   "Renders the row in the 'thead' element of the table. The
default implementation uses 'render-table-header-cell' to render
particular cells. Specialize this method to achieve customized
header row rendering. See 'render-standard-object' for more
details.

'obj' - the object being rendered.  'inlinep' - whether the
object should be rendered inline or should have its own
header."))

(defmethod render-table-header-row (obj &rest keys)
  (apply #'render-standard-object #'with-table-row #'render-table-header-cell obj :alternp nil keys))

(defgeneric render-table-header-cell (obj slot-name slot-value &rest keys
					  &key human-name &allow-other-keys)
  (:documentation
   "Renders the 'th' elements of the table. The default
implementation adds a class for CSS styling and renders a
humanized name of the slot. Specialize this method to achieve
customized header cell rendering.

If the 'slot-value' is a standard object that needs to be
rendered inline, the default implementation renders it as part of
the current header. See 'render-object-slot' function.

'obj' - Object whose slot is being rendered.
'slot-name' - Name of the slot.
'slot-value' - Value of the slot."))

(defmethod render-table-header-cell (obj slot-name slot-value &rest keys
				     &key (human-name slot-name) &allow-other-keys)
  (with-html
    (:th :class (attributize-name slot-name) (str (humanize-name human-name)))))

(defmethod render-table-header-cell (obj slot-name (slot-value standard-object) &rest keys)
  (render-object-slot #'render-table-header-row #'render-table-header-cell obj slot-name slot-value keys))

;; Table body
(defgeneric render-table-body-row (obj &rest keys &key inlinep &allow-other-keys)
  (:documentation
   "Renders the rows in the 'tbody' element of the table. The
default implementation uses 'render-table-body-cell' to render
particular cells. See 'render-table-header-row' for more
details."))

(defmethod render-table-body-row (obj &rest keys)
  (apply #'render-standard-object #'with-table-row #'render-table-body-cell obj keys))

(defgeneric render-table-body-cell (obj slot-name slot-value
				     &rest keys &key inlinep &allow-other-keys)
  (:documentation
   "Renders the 'td' elements of the table. See
'render-table-header-cell' for more details."))

(defmethod render-table-body-cell (obj slot-name slot-value &rest keys)
  (with-html
    (:td :class (attributize-name slot-name)
	 (apply #'render-data slot-value keys))))

(defmethod render-table-body-cell (obj slot-name (slot-value standard-object) &rest keys)
  (render-object-slot #'render-table-body-row #'render-table-body-cell obj slot-name slot-value keys))

;; The table itself
(defun render-table (objs &rest keys
		     &key (on-empty-string *render-empty-sequence-string*)
		     caption summary &allow-other-keys)
  "A generic table presentation renderer. This implementation
renders a sequence of objects into a table by calling
'render-table-body-row' for each object in the sequence. Table
header ('thead') is rendered via 'render-table-header-row' and
the wrapper header is renderer by 'with-table-header'. Override
these methods to customize functionality.

If the table is empty, 'render-empty-table' is called with
'on-empty-string' and 'caption'.

Since 'object-visible-slots' is used to determine which slots
should be renderered, any additional keys passed to
'render-table' will be forwarded to 'object-visible-slots'. All
techniques used to customize slot rendering in data renderers
also able to this renderer. See 'render-data' for more details.

All objects in the sequence are assumed to be of the same
type. This isn't explicitly enforced so you can use differently
typed objects at your own risk.

'objs' - a sequence of objects to be rendered into a table.
'on-empty-string' - a message printed if 'objs' is empty.
'caption' - a caption for the table.

\(render-table (list obj1 obj2 obj3))
\(render-table (list address1 address2) :slots (city) :mode :hide"
  (if (empty-p objs)
      (progn
	(render-empty-table :on-empty-string on-empty-string :caption caption)
	(return-from render-table)))
  (let ((row-num -1)
	(first-obj (first-element objs)))
    (with-table-header first-obj
      (lambda ()
	(with-html
	  (:table
	   :summary summary
	   (if caption
	       (htm (:caption (str caption))))
	   (htm
	    (:thead (apply #'render-table-header-row first-obj keys)))
	    (:tbody
	     (map 'list (lambda (obj)
			  (apply #'render-table-body-row obj
			   :alternp (oddp (incf row-num))
			   keys))
		  objs))))))))
     
(defun render-empty-table (&key on-empty-string caption)
  "A function used by 'render-table' to render an empty table.
'on-empty-string' - an informational string rendered to the
client.
'caption' - a caption for the block of information."
  (with-table-header nil
    (lambda ()
      (with-html
	(:p (if caption
		(htm (:span :class "caption" (str caption) ":&nbsp;")))
	    (:span :class "message" (str on-empty-string)))))))
