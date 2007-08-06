
(in-package :weblocks)

(export '(humanize-name attributize-name insert-after insert-at
	  object-visible-slots get-slot-value slot-value-by-path
	  render-slot-inline-p safe-apply safe-funcall
	  request-parameter request-parameters string-whitespace-p
	  render-extra-tags with-extra-tags strictly-less-p
	  equivalentp object-id id alist->plist intersperse
	  remove-keyword-parameter public-file-relative-path
	  public-files-relative-paths request-uri-path))

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
  (let ((namestr (etypecase name
		     (symbol (symbol-name name))
		     (string name)
		     (integer (format nil "~A" name)))))
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

(defun insert-after (newelt list index) 
  "Destructively inserts 'newelt' into 'list' after 'index'."
  (push newelt (cdr (nthcdr index list))) 
  list)

(defmacro insert-at (newelt list index) 
  "Destructively inserts 'newelt' into 'list' before 'index'."
  `(if (zerop ,index)
       (push ,newelt ,list)
       (insert-after ,newelt ,list (1- ,index))))

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

Custom slots that do not exist can also be added in any mode using
the :custom-slots keyword. The keyword should be bound to an
association list where the car of each cons cell is an index and the
cdr is the slot. The slot will be added and returned as is at the
specified index. If car of the cons pair is not a number or the
element isn't a cons pair, it is treated as a slot which will be
appended at the end.

\(object-visible-slots *joe* :custom-slots '((0 . test))) =>
    (test
     (#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . NAME)
     (#<STANDARD-DIRECT-SLOT-DEFINITION MANAGER> . MANAGER))

\(object-visible-slots *joe* :custom-slots '(test)) =>
    ((#<STANDARD-DIRECT-SLOT-DEFINITION NAME> . NAME)
     (#<STANDARD-DIRECT-SLOT-DEFINITION MANAGER> . MANAGER)
     test)
"))

(defmethod object-visible-slots (obj &key slots mode custom-slots &allow-other-keys)
  (let ((visible-slots (remove
			nil
			(if (eql mode :hide)
			    (let ((all-slots (class-visible-slots (class-of obj))))
			      (list->assoc (remove-if (curry-after #'member slots :test #'string-equal)
						      all-slots :key #'slot-definition-name)
					   :map #'slot-definition-name))
			    (let* ((slot-assoc (list->assoc slots))
				   (all-slots (class-visible-slots (class-of obj)
								   :visible-slots
								   (mapcar #'car slot-assoc))))
			      (if (eql mode :strict)
				  (mapcar (lambda (i)
					    (let ((slot (car (member (car i) all-slots
								     :test #'string-equal
								     :key #'slot-definition-name))))
					      (when (not (null slot))
						  (cons slot (cdr i)))))
					  slot-assoc)
				  (mapcar (lambda (i)
					    (cons i (let* ((slot-name (slot-definition-name i))
							   (alt-name (assoc slot-name slot-assoc)))
						      (if (null alt-name)
							  slot-name
							  (cdr alt-name)))))
					  all-slots)))))))
    (mapc (lambda (obj)
	    (if (and (consp obj)
		     (integerp (car obj)))
		(insert-at (cdr obj) visible-slots (car obj))
		(push-end obj visible-slots)))
	  custom-slots)
    (list->assoc visible-slots)))

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

(defun slot-value-by-path (obj path &key observe-inline-p)
  "Retrieves a value of a slot from a hierarchy of objects. A nil on
the path is ignored. 

If 'observe-inline-p' is set to true, 'slot-value-by-path' will return
the result of 'object-name' for objects that should not be rendered
inline according to 'render-slot-inline-p'

ex:
\(slot-value-by-path employee '(address street)) => \"17 Sunvalley St.\"
\(slot-value-by-path employee '(address-ref)) => \"Address\"
\(slot-value-by-path employee 'address-ref) => \"Address\"
\(slot-value-by-path address '(street)) => \"17 Sunvalley St.\"
\(slot-value-by-path address '(nil street)) => \"17 Sunvalley St.\"

obj - a CLOS object
path - a list of slot names"
  (when (symbolp path)
    (return-from slot-value-by-path
      (slot-value-by-path obj (list path) :observe-inline-p observe-inline-p)))
  (let* ((clean-path (remove nil path))
	 (value (ignore-errors (slot-value obj (car clean-path))))
	 (path-rest (cdr clean-path)))
    (if path-rest
	(slot-value-by-path value path-rest :observe-inline-p observe-inline-p)
	(if (and (not (render-slot-inline-p obj (car clean-path)))
		 observe-inline-p)
	    (object-name value)
	    value))))

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
\"<div class=\"extra-1\"></div><div class=\"extra-1\"></div>\""
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

(defmethod strictly-less-p ((a (eql nil)) (b (eql nil)))
  nil)

(defmethod strictly-less-p (a (b (eql nil)))
  t)

(defmethod strictly-less-p ((a (eql nil)) b)
  nil)

(defgeneric equivalentp (a b)
  (:documentation
   "Returns true if 'a' is in some sense equivalent to 'b'. This
function is used by the framework for sorting data."))

(defmethod equivalentp (a b)
  (equalp a b))

(defmethod equivalent ((a (eql nil)) (b (eql nil)))
  t)

(defgeneric object-id (obj)
  (:documentation
   "Returns a value that uniquely identifies an object in memory or in
a backend store. The default implementation looks for an 'id' slot via
'slot-value'. If such slot is not present, signals an
error. Specialize this function for various back end stores and other
object identification schemes."))

(defmethod object-id ((obj standard-object))
  (handler-case (slot-value obj 'id)
    (error (condition) (error "Cannot determine object ID. Object ~A has no slot 'id'." obj))))

(defun visit-object-slots (obj render-slot-fn &rest keys &key slot-path (call-around-fn-p t)
			   (ignore-unbound-slots-p nil) &allow-other-keys)
  "Used by 'render-standard-object' to visit visible slots of an
object and apply a render function to them.

If 'object-visible-slots' returns a function as a 'cdr' of a
particular slot cons pair due to appropriate arguments,
'visit-object-slots' calls this function instead of
'render-slot-fn' (unless 'call-around-fn-p' argument is set to
nil). This can be used to quickly render slots in a custom way without
specializing CLOS functions.

If 'ignore-unbound-slots-p is true, 'visit-object-slots' will
ignore errors that result from attempting to get slot values. In case
of errors, slot-value will simply be set to nil. Use this parameter to
allow visiting objects whose instances aren't bound to particular
values."
  (mapcar (lambda (slot)
	    (let ((render-fn (if (and call-around-fn-p
				      (functionp (cdr slot)))
				 (cdr slot)
				 render-slot-fn))
		  slot-name slot-value)
	      (if (typep (car slot) 'standard-direct-slot-definition)
		  (progn
		    (setf slot-name (slot-definition-name (car slot)))
		    (if ignore-unbound-slots-p
			(ignore-errors (setf slot-value (get-slot-value obj (car slot))))
			(setf slot-value (get-slot-value obj (car slot)))))
		  (setf slot-name (car slot)))
	      (apply render-fn obj slot-name
		     slot-value
		     :human-name (if (not (functionp (cdr slot)))
				     (cdr slot)
				     slot-name)
		     :slot-path (append slot-path (list slot-name))
		     (remove-keyword-parameter keys :custom-slots))))
	  (apply #'object-visible-slots obj keys)))

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
		   (intern (symbol-name (car i)) keyword-package)
		   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun intersperse (list delimeter)
  "Intersperses a list with a delimeter.

\(intersperse '(1 2 3 4 5) 0)
=> (1 0 2 0 3 0 4 0 5)"
  (if (null list)
      list
      (flatten
       (cons (car list)
	     (loop for i in (cdr list)
		collect (list delimeter i))))))


(defun remove-keyword-parameter (parameter-list keyword)
  "Removes a keyword parameter from a parameter-list.
\(remove-keyword-parameter '(1 2 3 :a 1 :b 2 :c 3) :b)
=> (1 2 3 :a 1 :c 3)"
  (let (remove)
    (loop for i in parameter-list
          when (eql i keyword)
            do (setf remove t)
          else when remove
            do (setf remove nil)
          else collect i)))

(defun tokenize-uri (uri)
  "Tokenizes a URI into a list of elements.

ex:
\(tokenize-uri \"/hello/world/blah\\test\\hala/world?hello=5;blah=7\"
=> (\"hello\" \"world\" \"blah\" \"test\" \"hala\" \"world\")"
  (remove-if (curry #'string-equal "")
	     (cl-ppcre:split "[/\\\\]" (cl-ppcre:regex-replace "\\?.*" uri ""))))

(defun public-file-relative-path (type filename)
  "Constructs a relative path to a public file from the \"/pub\" directory.

'type' - currently either :stylesheet or :script
'filename' the name of the file

Ex:
\(public-file-relative-path :stylesheet \"navigation\")
=> #P\"stylesheets/navigation\""
  (make-pathname :directory `(:relative
			      ,(ecase type
				      (:stylesheet "stylesheets")
				      (:script "scripts")))
		 :name filename
		 :type (ecase type
			 (:stylesheet "css")
			 (:script "js"))))

(defun public-files-relative-paths (&rest args)
  "A helper function that returns a list of paths for files provided
in 'args'. Each argument must be a cons cell where car is
either :stylesheet or :script and cdr is a name of the file.

Useful when generating a list of dependencies for widgets and/or the
application (see 'widget-public-dependencies' and
*application-public-dependencies*.)

Ex:
\(get-public-files-paths '(:stylesheet . \"navigation\")
                         '(:script . \"effects\"))
=> (#P\"stylesheets/navigation.css\" #P\"scripts/effects.js\")"
  (loop for i in args
     collect (public-file-relative-path (car i) (cdr i))))

(defun request-uri-path ()
  "Returns the path component of the request URI. The path component
does not include the domain name, and any query string parameters.
Ex (when URI is http://blah.com/foo/bar?x=1&y=2):
\(request-uri-path)
=> \"/foo/bar\""
  (declare (special *uri-tokens*))
  (apply #'concatenate 'string "/" (intersperse *uri-tokens* "/")))
