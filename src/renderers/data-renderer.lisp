;;;; Generic data renderer
(in-package :weblocks)

(defgeneric with-data-header (obj body-fn)
  (:documentation
   "Responsible for rendering headers of a data presentation. The
default method renders a humanized name of the object along with
appropriate CSS classes and extra tags for styling. The default
implementation also sets up an unordered list. After the header
is set up, 'with-data-header' calls a function of zero arguments
'body-fn' which renders the body of the object (slots, etc.)
'with-data-header' renders a footer after that.

'with-data-header' is normally called by 'render-data' and should
not be called by the programmer. Override 'with-data-header' to
provide customized header rendering."))

(defmethod with-data-header (obj body-fn)
  (let ((header-class (format nil "renderer data ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html
      (:div :class header-class
	    (render-extra-tags "extra-top-" 3)
	    (htm (:div :class "fields"
		  (:h1 (:span :class "action" "Viewing:&nbsp;")
		       (:span :class "object" (str (humanize-name (object-class-name obj)))))
		  (:ul (funcall body-fn))))
	    (render-extra-tags "extra-bottom-" 3)))))

(defgeneric render-data-slot (obj slot-name slot-value &rest args)
  (:documentation
   "Renders a given slot of a particular object.

'obj' - The object whose slot is being rendered.
'slot-name' - The name of a slot (a symbol) being rendered.
'slot-value' - The value of a slot determined with 'get-slot-value'.
'args' - A list of arguments to pass to 'render-data'.

The default implementation renders a list item. If the slot's
value is a CLOS object, 'render-data-slot' determines whether the
slot should be rendered iline via 'render-slot-inline-p' and then
calls 'render-data' with 'inlinep' value being set
appropriately.

Override this function to render a slot in a customized
manner. Note that you can override based on the object as well as
slot name, which gives significant freedom in selecting the
proper slot to override."))

(defmethod render-data-slot (obj slot-name (slot-value standard-object) &rest args)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-data slot-value :inlinep t args)
      (apply #'render-data-slot obj slot-name (object-name slot-value) args)))

(defmethod render-data-slot (obj slot-name slot-value &rest args)
  (with-html
    (:li (:span :class "label"
		(str (humanize-name slot-name)) ":&nbsp")
	 (apply #'render-data slot-value args))))

(defgeneric render-data (obj &rest keys &key inlinep &allow-other-keys)
  (:documentation
   "A generic data presentation renderer. The default
implementation of 'render-data' for CLOS objects dynamically
introspects object instances and serializes them to HTML
according to the following protocol:

1. If 'inlinep' is false a generic function 'with-data-header' is
called to render headers and footers. To avoid rendering headers
and footers set 'inlinep' to nil. Specialize 'with-data-header'
to customize header and footer HTML.

2. 'object-visible-slots' is called to determine which slots in
the object instance should be rendered. Any additional keys
passed to 'render-data' are forwarded to
'object-visible-slots'. To customize the order of the slots,
their names, visibility, etc. look at 'object-visible-slots'
documentation for necessary arguments, or specialize
'object-visible-slots'.

3. On each slot returned by the previous step, 'get-slot-value'
is called to determine its value and 'render-data-slot' is called
to render the slot. Specialize 'render-data-slot' to get
customized behavior.

If specializing above steps isn't sufficient to produce required
HTML, 'render-data' should be specialized for particular objects.

Ex:
\(render-data address)
\(render-data address :slots (city) :mode :hide
\(render-data address :slots ((city . town))
\(render-data address :slots ((city . town) :mode :strict)"))

(defmethod render-data ((obj standard-object) &rest keys &key inlinep &allow-other-keys)
  (let ((render-body
	 (lambda ()
	   (let ((keys-copy (copy-list keys)))
	     (remf keys-copy :inlinep)
	     (mapc (lambda (slot)
		     (apply #'render-data-slot obj (cdr slot)
			    (get-slot-value obj (car slot)) keys))
		   (apply #'object-visible-slots obj keys-copy))))))
    (if inlinep
	(funcall render-body)
	(with-data-header obj render-body))
    *weblocks-output-stream*))

(defmethod render-data (obj &rest keys &key inlinep &allow-other-keys)
  (with-html
    (:span :class "value"
     (str obj)))
  *weblocks-output-stream*)

