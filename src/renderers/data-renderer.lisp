;;;; Generic data renderer
(in-package :weblocks)

(export '(with-data-header render-data-slot render-data
	  data-print-object render-data-aux))

(defgeneric with-data-header (obj body-fn &rest keys &key preslots-fn postslots-fn &allow-other-keys)
  (:documentation
   "Responsible for rendering headers of a data presentation. The
default method renders a humanized name of the object along with
appropriate CSS classes and extra tags for styling. The default
implementation also sets up an unordered list. After the header
is set up, 'with-data-header' calls a function of zero arguments
'body-fn' which renders the body of the object (slots, etc.)
'with-data-header' renders a footer after that.

'preslots-fn' and 'postslots-fn' can be used to add specialized
rendering before and after the list of slots is rendered,
respectively. When supplied, these keys should be bound to a
function that takes the object being rendered ('obj') and a list
of keys and render appropriate html. See 'render-form-controls'
for an example.

'with-data-header' is normally called by 'render-data-aux' and should
not be called by the programmer. Override 'with-data-header' to
provide customized header rendering."))

(defmethod with-data-header (obj body-fn &rest keys &key preslots-fn postslots-fn &allow-other-keys)
  (let ((header-class (format nil "renderer data ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html
      (:div :class header-class
	    (with-extra-tags
	      (htm (:h1 (:span :class "action" "Viewing:&nbsp;")
			(:span :class "object" (str (humanize-name (object-class-name obj)))))
		   (safe-apply preslots-fn obj keys)
		   (:ul (funcall body-fn))
		   (safe-apply postslots-fn obj keys)))))))

(defgeneric render-data-slot (obj slot-name slot-type slot-value &rest keys &key human-name &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Renders a given slot of a particular object.

'obj' - The object whose slot is being rendered.
'slot-name' - The name of a slot (a symbol) being rendered.
'slot-type' - The type of a slot being rendered.
'slot-value' - The value of a slot determined with 'get-slot-value'.
'args' - A list of arguments to pass to 'render-data-aux'.

The default implementation renders a list item. If the slot's value is
a CLOS object, 'render-data-slot' determines whether the slot should
be rendered iline via 'render-slot-inline-p' and then calls
'render-data-aux' with 'inlinep' value being set appropriately.

Override this function to render a slot in a customized
manner. Note that you can override based on the object as well as
slot name, which gives significant freedom in selecting the
proper slot to override."))

(defslotmethod render-data-slot (obj slot-name slot-type (slot-value standard-object) &rest keys)
  (render-object-slot #'render-data-aux #'render-data-slot obj slot-name slot-type slot-value keys))

(defslotmethod render-data-slot (obj slot-name slot-type slot-value &rest keys
				     &key (human-name slot-name) &allow-other-keys)
  (with-html
    (:li :class (attributize-name slot-name)
	 (:span :class "label"
		(str (humanize-name human-name)) ":&nbsp;")
	 (apply #'render-data-aux obj slot-name slot-type slot-value keys))))

(defun render-data (obj &rest keys &key parent-object slot-name
		    (slot-type t) &allow-other-keys)
  "A convinient wrapper for 'render-data-aux'. 

Ex:
\(render-data address)
\(render-data address :slots (city) :mode :hide
\(render-data address :slots ((city . town))
\(render-data address :slots ((city . town) :mode :strict)"
  (apply #'render-data-aux parent-object slot-name slot-type obj keys))

(defgeneric data-print-object (obj slot-name slot-type slot-value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Prints 'slot-value' to a string that will be used to render the
value in a data renderer. This function is called by
'render-data-aux'. Default implementation simply uses (format nil
\"~A\" slot-value). Specialize this function to customize simple data
printing without having to specialize more heavy 'render-data-aux'."))

(defslotmethod data-print-object (obj slot-name slot-type slot-value &rest args)
  (format nil "~A" slot-value))

(defgeneric render-data-aux (obj slot-name slot-type slot-value &rest keys
				 &key inlinep highlight &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "A generic data presentation renderer.

'obj' - an object that contains the slot whose value is to be rendered.
'slot-name' - name of the slot whose value is to be rendered.
'slot-type' - type of the slot whose value is to be rendered.
'slot-value' - value to be rendered.

The default implementation of 'render-data-aux' for CLOS objects
dynamically introspects object instances and serializes them to HTML
according to the following protocol:

1. If 'inlinep' is false a generic function 'with-data-header' is
called to render headers and footers. To avoid rendering headers
and footers set 'inlinep' to nil. Specialize 'with-data-header'
to customize header and footer HTML.

2. 'object-visible-slots' is called to determine which slots in
the object instance should be rendered. Any additional keys
passed to 'render-data-aux' are forwarded to
'object-visible-slots'. To customize the order of the slots,
their names, visibility, etc. look at 'object-visible-slots'
documentation for necessary arguments, or specialize
'object-visible-slots'.

3. On each slot returned by the previous step, 'get-slot-value'
is called to determine its value and 'render-data-slot' is called
to render the slot. Specialize 'render-data-slot' to get
customized behavior.

If specializing above steps isn't sufficient to produce required
HTML, 'render-data-aux' should be specialized for particular objects.

When 'highlight' is set not null, render-data-aux searches for the
ppcre regular expression it represents and renders it as a strong
element. This is done to support incremental searching in some
controls.

See 'render-data' for examples."))

(defslotmethod render-data-aux (obj slot-name slot-type (slot-value standard-object) &rest keys)
  (apply #'render-standard-object #'with-data-header #'render-data-slot slot-value keys))

(defslotmethod render-data-aux (obj slot-name slot-type slot-value
				    &rest keys &key highlight &allow-other-keys)
  (let* ((item (apply #'data-print-object obj slot-name slot-type slot-value keys)))
    (with-html
      (:span :class "value"
	     (str (if highlight
		      (highlight-regex-matches item highlight)
		      (escape-for-html item)))))))

(defslotmethod render-data-aux (obj slot-name slot-type (slot-value (eql nil))
				    &rest keys &key ignore-missing-values-p &allow-other-keys)
  (if ignore-missing-values-p
      (call-next-method)
      (with-html
	(:span :class "value missing" "Not Specified"))))

(defun highlight-regex-matches (item highlight)
  "This function highlights regex matches in text by wrapping them in
HTML 'string' tag. The complexity arises from the need to escape HTML
to prevent XSS attacks. If we simply wrap all matches in 'strong' tags
and then escape the result, we'll escape our 'strong' tags which isn't
the desired outcome."
  (apply #'concatenate 'string
	 (remove ""
		 (loop for i = 0 then k
		       for (j k) on (ppcre:all-matches highlight item) by #'cddr
		       for match = (subseq item j k)
		    collect (escape-for-html (subseq item i j)) into matches
		    when (not (equalp match ""))
		         collect (format nil "<strong>~A</strong>"
					 (escape-for-html match))
		            into matches
		    finally (return (if (and j k)
				(push-end (escape-for-html (subseq item k (length item)))
					  matches)
				(list (escape-for-html item))))))))
