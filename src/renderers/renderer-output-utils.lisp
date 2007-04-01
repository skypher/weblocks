;;;; Utility functions for generic renderers
(in-package :weblocks)

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
			     (cons slot (cdr i)))))
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

(defgeneric object-class-name (obj)
  (:documentation
   "Returns an object's class name (i.e. \"Employee\"). This
method is used by renderers to present the name of an entity to
the user. Override this method to change the name for particular
objects."))

(defmethod object-class-name (obj)
  (class-name (class-of obj)))

(defgeneric object-name (obj)
  (:documentation
   "Takes an object and returns its name (as opposed to class
name, the name of an object is specific to its instance,
i.e. \"Joe Average\", instead of \"Employee\"). The renderers use
this method to present objects that aren't rendered inline by
name.

By default this method looks for a symbol of a form
[class-name]-NAME (e.g. EMPLOYEE-NAME) in the same package where
the class of 'obj' was defined. If this symbol is bound to a
function the function is then callsed with the object as the only
argument. If it returns a string, the value is returned as the
name of the object. Otherwise, the value of 'object-class-name'
is returned.

Override this method to provide object names."))

(defmethod object-name (obj)
  (let* ((cls-symbol (class-name (class-of obj)))
	 (cls-package (symbol-package cls-symbol))
	 (expected-name (concatenate 'string (symbol-name cls-symbol) "-NAME"))
	 (expected-symbol (find-symbol expected-name cls-package)))
    (if (and (not (null expected-symbol))
	     (fboundp expected-symbol))
	(let ((obj-name (funcall expected-symbol obj)))
	  (if (stringp obj-name)
	      (return-from object-name obj-name)))))
  (object-class-name obj))

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

(defun get-slot-value (obj slot)
  "If a reader accessor for the slot exists, gets the value of
'slot' via the accessor. Otherwise, uses slot-value."
  (let ((slot-reader (car (slot-definition-readers slot))))
    (if (null slot-reader)
	(slot-value obj (slot-definition-name slot))
	(funcall slot-reader obj))))

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
