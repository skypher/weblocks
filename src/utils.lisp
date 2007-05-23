
(in-package :weblocks)

(export '(humanize-name attributize-name object-visible-slots
	  get-slot-value slot-value-by-path render-slot-inline-p
	  safe-apply safe-funcall request-parameter
	  string-whitespace-p render-extra-tags with-extra-tags
	  strictly-less-p equivalentp))

(defun humanize-name (name)
  "Convert a string or a symbol to a human-readable string
suitable for presentation. If the arguments ends with a '-ref'
the last four characters are removed, as this suffix is used as a
cue to to renderers that the attribute shouldn't be rendered
inline.

Ex:
\(humanize-name 'hello-world) => \"Hello World\"
\(humanize-name \"HELLO-WORLD\") => \"Hello World\"
\(humanize-name 'hello-ref) => \"Hello\""
  (let* ((namestr (if (symbolp name)
		      (string-downcase (symbol-name name))
		      name))
	 (namestrpost (if (string-ends-with namestr "-ref")
			  (substring namestr 0 (- (length namestr) 4))
			  namestr)))
    (string-capitalize (substitute #\Space #\- namestrpost))))

(defun attributize-name (name)
  "Convert a string or a symbol to a format suitable for
serialization (in particular for markup languages like HTML).

Ex:
\(attributize-name 'hello-world) => \"hello-world\"
\(attributize-name \"Hello world-ref\") => \"hello-world-ref\""
  (when (null name)
    (return-from attributize-name ""))
  (let ((namestr (if (symbolp name)
		     (symbol-name name)
		     name)))
    (string-downcase (substitute #\- #\Space namestr))))

(defun list->assoc (lst &key (map #'identity))
  "Nondestructively convert a list of elements to an association
list If an element of a list is a cons cell, it is left as
is. Otherwise, it is replaced with a cons cell whose 'car' is the
element and whose 'cdr' is a result of 'map' applied to the
element. The 'map' is an identity by default.

Ex:
\(list->assoc '(name age (city . location))) => ((name . name) (age . age) (city . location))
\(list->assoc '(1 (2 . 2) 3) :map #'1+) => ((1 . 2) (2 . 2) (3 . 4))"
  (mapcar (lambda (i)
	    (if (consp i) i (cons i (funcall map i))))
	  lst))

(defgeneric object-visible-slots (obj &key slots mode &allow-other-keys)
  (:documentation
   "Returns a list of cons cells where 'car' of each member is
the 'direct-slot-object' and a 'cdr' of each member is a symbol
or a string representing the name of the slot. The rules for
determining visible slots are the same as for
'class-visible-slots'. This method is used by renderers to
determine which slots should be displayed. Specialize this method
for your objects to customize this behavior. (Note, look for
documentation of 'class-visible-slots' for relevant definitions
used in the examples.)

Ex:
\(object-visible-slots *joe*) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . NAME)
     (#<STANDARD-DIRECT-SLOT-DEFINITION MANAGER> . MANAGER))

If 'mode' keyword parameter is set to nil (the default), 'slots'
keyword parameter is expected to contain a list of cons cells
that modify the names of the slots as well as slot names that
should be displayed even though they have no accessors. This is
used by the renderers to easily change rendered field names.

Ex:
\(object-visible-slots *joe* :slots (age (name . first-name))) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . FIRST-NAME)
     (#<STANDARD-DIRECT-SLOT-DEFINITION AGE> . AGE)
     (#<STANDARD-DIRECT-SLOT-DEFINITION MANAGER> . MANAGER))

If 'mode' is set to ':hide', 'slots' is expected to contain a
list of slot names that should not be displayed (and hence will
not be returned by 'object-visible-slots'.)

Ex:
\(object-visible-slots *joe* :slots (name) :mode :hide) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION MANAGER> . MANAGER))

If 'mode' is set to ':strict', 'slots' has similar semantics to
when 'mode' is set to nil, except that only the slots listed will
be displayed and the order will be maintained:

Ex:
\(object-visible-slots *joe* :slots (age (name . first-name)) :mode :strict) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION AGE> . AGE)
     (#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . FIRST-NAME))

A function is a legidimate 'cdr' of a cons pair when passed via :slots
argument. In this case the argument will be treated by various pieces
of the framework as an 'override' that will be used to render the slot
instead of the usual mechanism. 'object-visible-slots' simply returns
the function in such cases:

\(object-visible-slots *joe* :slots (list
                                     (cons name
                                           (lambda ()
                                              nil)))
                             :mode :strict) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . #<FUNCTION # {C239E45}>)

In strict mode slot names that do not exist in the object's class can
be specified. They will be returned as is, and the renderers can later
use them to render custom slots.
"))

(defmethod object-visible-slots (obj &key slots mode &allow-other-keys)
  (remove-if
   (curry #'eq nil)
   (if (eql mode :hide)
       (let ((all-slots (class-visible-slots (class-of obj))))
	 (list->assoc (remove-if (curry-after #'member slots :test #'string-equal)
				 all-slots :key #'slot-definition-name)
		      :map #'slot-definition-name))
       (let* ((slot-assoc (list->assoc slots))
	      (all-slots (class-visible-slots (class-of obj) :visible-slots (mapcar #'car slot-assoc))))
	 (if (eql mode :strict)
	     (mapcar (lambda (i)
		       (let ((slot (car (member (car i) all-slots
						:test #'string-equal
						:key #'slot-definition-name))))
			 (if (not (null slot))
			     (cons slot (cdr i))
			     i)))
		     slot-assoc)
	     (mapcar (lambda (i)
		       (cons i (let* ((slot-name (slot-definition-name i))
				      (alt-name (assoc slot-name slot-assoc)))
				 (if (null alt-name)
				     slot-name
				     (cdr alt-name)))))
		     all-slots))))))

(defun class-visible-slots (cls &key visible-slots)
  "Returns a list of 'standard-direct-slot' objects for a class
and its subclasses. Slots objects for slots that do not have
reader accessors are filtered out and not returned. This behavior
can be modified by providing a list of symbols indicating slot
names via 'visible-slots' keyword argument. Slot objects whose
names show up in 'visible-slots' list are returned regardless of
whether an accessor is defined for them.

Ex: \(defclass person ()
  ((name :reader first-name :initform \"Joe\")
   (age :initform 30)))

\(defclass employee (person)
  ((manager :reader manager :initform \"Jim\")))

\(setf *joe* (class-of (make-instance 'employee)))

\(class-visible-slots *joe*) =>
    (#<STANDARD-DIRECT-SLOT-DEFINITION NAME>
     #<STANDARD-DIRECT-SLOT-DEFINITION MANAGER>)
\(class-visible-slots *joe* :visible-slots '(age)) =>
    (#<STANDARD-DIRECT-SLOT-DEFINITION NAME>
     #<STANDARD-DIRECT-SLOT-DEFINITION AGE>
     #<STANDARD-DIRECT-SLOT-DEFINITION MANAGER>)"
  (if (eql (class-name cls) 'standard-object)
      nil
      (apply #'append (append (mapcar (curry-after #'class-visible-slots :visible-slots visible-slots)
				      (class-direct-superclasses cls))
			      (list (remove-if (lambda (x)
						 (and (null (slot-definition-readers x))
						      (not (member (slot-definition-name x) visible-slots))))
					       (class-direct-slots cls)))))))

(defun get-slot-value (obj slot)
  "If a reader accessor for the slot exists, gets the value of
'slot' via the accessor. Otherwise, uses slot-value.

'slot' - slot-definition object."
  (let ((slot-reader (car (slot-definition-readers slot))))
    (if (null slot-reader)
	(slot-value obj (slot-definition-name slot))
	(funcall slot-reader obj))))

(defun slot-value-by-path (obj path)
  "Retrieves a value of a slot from a hierarchy of objects. Nil is
ignored.

ex:
\(slot-value-by-path employee '(address street)) => \"17 Sunvalley St.\"
\(slot-value-by-path address '(street)) => \"17 Sunvalley St.\"
\(slot-value-by-path address '(nil street)) => \"17 Sunvalley St.\"

obj - a CLOS object
path - a list of slot names"
  (when (symbolp path)
    (return-from slot-value-by-path (slot-value obj path)))
  (let* ((clean-path (remove nil path))
	 (value (slot-value obj (car clean-path)))
	 (path-rest (cdr clean-path)))
    (if path-rest
	(slot-value-by-path value path-rest)
	value)))

(defgeneric render-slot-inline-p (obj slot-name)
  (:documentation
   "Returns a boolean value that indicates whether an object
should be rendered inline. The renderers use this method to
determine whether the fields of a complex slot should be rendered
as part of the object, or the name of the object the slot
represents should be rendered instead.

The default implementation returns true if the slot name ends
with \"-ref\" and nil otherwise.

Override this method to specify whether objects should be
rendered inline.

'obj' - The object whose slot is being rendered.
'slot-name' - The name of a slot (a symbol) being rendered.
"))

(defmethod render-slot-inline-p (obj slot-name)
  (let ((name (if (symbolp slot-name)
		  (symbol-name slot-name)
		  slot-name)))
    (not (string-ends-with name "-ref" :ignore-case-p t))))

(defmacro safe-apply (fn &rest args)
  "Apply 'fn' if it isn't nil. Otherwise return nil."
  `(when ,fn
       (apply ,fn ,@args)))

(defmacro safe-funcall (fn &rest args)
  "Funcall 'fn' if it isn't nil. Otherwise return nil."
  `(when ,fn
       (funcall ,fn ,@args)))

(defun request-parameter (name)
  "Get parameter 'name' from the request. If the request was
submitted via GET method, the parameter is obtained from the
query string. If the request was submitted via POST, the
parameter is obtained from the body of the request. Otherwise, an
error is signalled."
  (ecase (request-method)
    (:get (get-parameter name))
    (:post (post-parameter name))))

(defun request-parameters ()
  "Get parameters alist from the request. If the request was submitted
via GET method, the parameters are obtained from the query string. If
the request was submitted via POST, the parameters are obtained from
the body of the request. Otherwise, an error is signalled."
  (ecase (request-method)
    (:get (get-parameters))
    (:post (post-parameters))))

(defun string-whitespace-p (str)
  "Returns true if every character in a string is a whitespace
character, nil otherwise."
  (loop for c across str
     always (whitespacep c)))

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

(defmacro with-extra-tags (&body body)
  "A macro used to wrap html into extra tags necessary for
hacking CSS formatting. The macro wraps the body with three
headers on top and three on the bottom. It uses
'render-extra-tags' function along with 'extra-top-' and
'extra-bottom-' arguments."
  `(progn
     (render-extra-tags "extra-top-" 3)
     ,@body
     (render-extra-tags "extra-bottom-" 3)))

(defgeneric strictly-less-p (a b)
  (:documentation
   "Returns true if 'a' is strictly less than 'b'. This function is
used by the framework for sorting data."))

(defmethod strictly-less-p ((a number) (b number))
  (< a b))

(defmethod strictly-less-p ((a string) (b string))
  (string-lessp a b))

(defgeneric equivalentp (a b)
  (:documentation
   "Returns true if 'a' is in some sense equivalent to 'b'. This
function is used by the framework for sorting data."))

(defmethod equivalentp (a b)
  (equalp a b))

(defun visit-object-slots (obj render-slot-fn &rest keys &key slot-path (call-around-fn-p t)
			   &allow-other-keys)
  "Used by 'render-standard-object' to visit visible slots of an
object and apply a render function to them.

If 'object-visible-slots' returns a function as a 'cdr' of a
particular slot cons pair due to appropriate arguments,
'visit-object-slots' calls this function instead of
'render-slot-fn' (unless 'call-around-fn-p' argument is set to
nil). This can be used to quickly render slots in a custom way without
specializing CLOS functions."
  (mapc (lambda (slot)
	  (let ((render-fn (if (and call-around-fn-p
				    (functionp (cdr slot)))
			       (cdr slot)
			       render-slot-fn))
		slot-name slot-value)
	    (if (typep (car slot) 'standard-direct-slot-definition)
		(setf slot-name (slot-definition-name (car slot))
		      slot-value (get-slot-value obj (car slot)))
		(setf slot-name (car slot)))
	    (apply render-fn obj slot-name
		   slot-value
		   :human-name (if (not (functionp (cdr slot)))
				   (cdr slot)
				   slot-name)
		   :slot-path (append slot-path (list slot-name))
		   keys)))
	(apply #'object-visible-slots obj keys)))

