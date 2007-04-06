;;;; Generic form renderer
(in-package :weblocks)

(defgeneric with-form-header (obj body-fn &key name)
  (:documentation
   "Responsible for rendering headers of a data presentation. The
default method renders a humanized name of the object along with
appropriate CSS classes and extra tags for styling. The default
implementation also sets up an unordered list. After the header
is set up, 'with-form-header' calls a function of zero arguments
'body-fn' which renders the body of the object (slots, etc.)
'with-form-header' renders a footer after that.

'with-form-header' is normally called by 'render-form' and should
not be called by the programmer. Override 'with-form-header' to
provide customized header rendering."))

(defmethod with-form-header (obj body-fn &key name)
  (let ((header-class (format nil "renderer form ~A"
			      (attributize-name (object-class-name obj))))
	(object-name (if (null name)
			 (humanize-name (object-class-name obj))
			 name)))
    (with-html
      (:form :class header-class :action "#" :method "post"
	     (with-extra-tags
	       (htm (:fieldset
		     (:h1 (:span :class "action" "Viewing:&nbsp;")
			  (:span :class "object" (str object-name)))
		     (:ul (funcall body-fn)
			  (htm (:li :class "submit"
				    (:input :name "ok" :type "submit" :value "Submit")
				    (:input :name "cancel" :type "submit" :value "Cancel")))))))))))

(defgeneric render-form-slot (obj slot-name slot-value &rest args)
  (:documentation
   "Renders a given slot of a particular object.

'obj' - The object whose slot is being rendered.
'slot-name' - The name of a slot (a symbol) being rendered.
'slot-value' - The value of a slot determined with 'get-slot-value'.
'args' - A list of arguments to pass to 'render-form'.

The default implementation renders a list item. If the slot's
value is a CLOS object, 'render-form-slot' determines whether the
slot should be rendered iline via 'render-slot-inline-p' and then
calls 'render-form' with 'inlinep' value being set
appropriately.

Override this function to render a slot in a customized
manner. Note that you can override based on the object as well as
slot name, which gives significant freedom in selecting the
proper slot to override."))

(defmethod render-form-slot (obj slot-name (slot-value standard-object) &rest args)
  (render-object-slot #'render-form #'render-form-slot obj slot-name slot-value args))

(defmethod render-form-slot (obj slot-name slot-value &rest args)
  (let ((attribute-slot-name (attributize-name slot-name)))
    (with-html
      (:li (:label :class "label"
		   (:span (str (humanize-name slot-name)) ":&nbsp;")
		   (apply #'render-form slot-value :name attribute-slot-name args))))))

(defgeneric render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (:documentation
   "A generic data presentation renderer. The default
implementation of 'render-form' for CLOS objects dynamically
introspects object instances and serializes them to HTML
according to the following protocol:

1. If 'inlinep' is false a generic function 'with-form-header' is
called to render headers and footers. To avoid rendering headers
and footers set 'inlinep' to nil. Specialize 'with-form-header'
to customize header and footer HTML.

2. 'object-visible-slots' is called to determine which slots in
the object instance should be rendered. Any additional keys
passed to 'render-form' are forwarded to
'object-visible-slots'. To customize the order of the slots,
their names, visibility, etc. look at 'object-visible-slots'
documentation for necessary arguments, or specialize
'object-visible-slots'.

3. On each slot returned by the previous step, 'get-slot-value'
is called to determine its value and 'render-form-slot' is called
to render the slot. Specialize 'render-form-slot' to get
customized behavior.

If specializing above steps isn't sufficient to produce required
HTML, 'render-form' should be specialized for particular objects.

Ex:
\(render-form address)
\(render-form address :slots (city) :mode :hide
\(render-form address :slots ((city . town))
\(render-form address :slots ((city . town) :mode :strict)"))

(defmethod render-form ((obj standard-object) &rest keys &key inlinep name &allow-other-keys)
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))

(defmethod render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (with-html
    (:input :type "text" :name name :value obj))
  *weblocks-output-stream*)

