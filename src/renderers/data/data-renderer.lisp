;;;; Generic data renderer
(in-package :weblocks)

(defun render-extra-tags (tag-class count)
  "Renders extra tags to get around CSS limitations. 'tag-class'
is a string that specifies the class name and 'count' is the
number of extra tags to render.
Ex:
\(render-extra-tags \"extra-\" 2) =>
\"<div class=\"extra-1\">&nbsp;</div><div class=\"extra-1\">&nbsp;</div>\""
  (with-html-output (*weblocks-output-stream*)
    (loop for i from 1 to count
          for attr = (format nil "~A~A" tag-class i)
       do (htm (:div :class attr "&nbsp;")))))

(defmethod with-data-header (obj body-fn)
  (let ((header-class (format nil "data ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html-output (*weblocks-output-stream*)
      (:div :class header-class
	    (render-extra-tags "extra-top-" 3)
	    (htm (:h1 (:span :class "action" "Viewing:&nbsp;")
		      (:span :class "object" (str (humanize-name (object-class-name obj)))))
		 (:ul (funcall body-fn)))
	    (render-extra-tags "extra-bottom-" 3)))))

(defmethod render-data-slot (obj slot-name (slot-value standard-object) &rest args)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-data slot-value :inlinep t args)
      (apply #'render-data-slot obj slot-name (object-name slot-value) args)))

(defmethod render-data-slot (obj slot-name slot-value &rest args)
  (with-html-output (*weblocks-output-stream*)
    (:li (:h2 (str (humanize-name slot-name)) ":")
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
\(render-data address :slots ((city . town)) :mode :strict"))

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
  (with-html-output (*weblocks-output-stream*)
    (:span (str obj)))
  *weblocks-output-stream*)

