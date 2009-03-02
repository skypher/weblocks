
(in-package :weblocks)

(wexport '(gen-id humanize-name attributize-name insert-after insert-at
	   slot-value-by-path safe-apply safe-funcall request-parameter
	   request-parameters string-whitespace-p render-extra-tags
	   with-extra-tags alist->plist intersperse
	   remove-keyword-parameter remove-keyword-parameters
	   public-file-relative-path public-files-relative-paths
	   request-uri-path string-remove-left string-remove-right
	   find-all stable-set-difference symbol-status
	   string-invert-case ninsert add-get-param-to-url
	   remove-parameter-from-uri asdf-system-directory
	   make-isearch-regex hash-keys object-class-name
	   append-custom-fields find-slot-dsd find-slot-esd drop-last
	   function-designator-p list-starts-with safe-subseq)
	 '(t util))

(defun gen-id (&optional (prefix ""))
  "Generates an ID unique accross the session. The generated ID can be
used to create IDs for html elements, widgets, etc."
  (let ((new-widget-id (1+ (or (session-value 'last-unique-id) -1))))
    (setf (session-value 'last-unique-id) new-widget-id)
    (format nil "~A~A" prefix new-widget-id)))

(defgeneric humanize-name (name)
  (:documentation "Convert objects to a human-readable string suitable
for presentation. Default implementations beautify strings and
symbols.

Ex:
\(humanize-name 'hello-world) => \"Hello World\"
\(humanize-name \"HELLO-WORLD\") => \"Hello World\"")
  (:method ((name string))
    (string-capitalize (substitute #\Space #\- name)))
  (:method ((name symbol))
    (humanize-name (string-downcase (symbol-name name)))))

(defgeneric attributize-name (name)
  (:documentation "Convert objects to a format suitable for
serialization (in particular for markup languages like HTML).

Ex:
\(attributize-name 'hello-world) => \"hello-world\"")
  (:method ((name null))
    "")
  (:method ((name string))
    (string-downcase (substitute #\- #\Space name)))
  (:method ((name symbol))
    (attributize-name (symbol-name name)))
  (:method ((name integer))
    (format nil "~A" name)))

(defun unattributized-name (name &optional (type-marker 'uname))
  "Call `attributize-name' with NAME and modify the result to be a
variant that is still usable in all cases where `attributize-name'
results can be used, but is extremely unlikely to ever be returned by
`attributize-name'.

TYPE-MARKER is just a label for human discernment of the different
users of this function."
  (format nil "~:@(~A~)-~A" type-marker (attributize-name name)))

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

(defun slot-value-by-path (obj path)
  "Retrieves a value of a slot from a hierarchy of objects. A nil on
the path is ignored. 

ex:
\(slot-value-by-path employee '(address street)) => \"17 Sunvalley St.\"
\(slot-value-by-path employee '(address)) => #<ADDRESS {XXX}>
\(slot-value-by-path employee 'address) => #<ADDRESS {XXX}>
\(slot-value-by-path address '(street)) => \"17 Sunvalley St.\"
\(slot-value-by-path address '(nil street)) => \"17 Sunvalley St.\"

obj - a CLOS object
path - a list of slot names"
  (when (symbolp path)
    (return-from slot-value-by-path
      (slot-value-by-path obj (list path))))
  (let* ((clean-path (remove nil path))
	 (value (ignore-errors (slot-value obj (car clean-path))))
	 (path-rest (cdr clean-path)))
    (if path-rest
	(slot-value-by-path value path-rest)
	value)))

(defun safe-apply (fn &rest args)
  "Apply 'fn' if it isn't nil. Otherwise return nil."
  (when fn
    (apply #'apply fn args)))

(defun safe-funcall (fn &rest args)
  "Funcall 'fn' if it isn't nil. Otherwise return nil."
  (when fn
    (apply #'funcall fn args)))

(defun request-parameter (name)
  "Get parameter 'name' from the request. If the request was
submitted via GET method, the parameter is obtained from the
query string. If the request was submitted via POST, the
parameter is obtained from the body of the request. Otherwise, an
error is signalled."
  (ecase (request-method*)
    (:get (get-parameter name))
    (:post (post-parameter name))))

(defun request-parameters ()
  "Get parameters alist from the request. If the request was submitted
via GET method, the parameters are obtained from the query string. If
the request was submitted via POST, the parameters are obtained from
the body of the request. Otherwise, an error is signalled."
  (ecase (request-method*)
    (:get (get-parameters*))
    (:post (post-parameters*))))

(defun string-whitespace-p (str)
  "Returns true if every character in a string is a whitespace
character, nil otherwise."
  (loop for c across str
     always (whitespacep c)))

(defmethod render-extra-tags (tag-class count)
  "Renders extra tags to get around CSS limitations. 'tag-class'
is a string that specifies the class name and 'count' is the
number of extra tags to render.
Ex:
\(render-extra-tags \"extra-\" 2) =>
\"<div class=\"extra-1\"></div><div class=\"extra-1\"></div>\""
  (with-html-output (*weblocks-output-stream*)
    (loop for i from 1 to count
          for attr = (format nil "~A~A" tag-class i)
       do (htm (:div :class attr "<!-- empty -->")))))

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

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
		   (intern (symbol-name (car i)) keyword-package)
		   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun intersperse (list delimeter &key (last delimeter))
  "Intersperses a list with a delimeter.

If 'last' is specified, it will be used for the last delimeter,
instead of 'delimeter'.

\(intersperse '(1 2 3 4 5) 0)
=> (1 0 2 0 3 0 4 0 5)"
  (cond
    ((null list) list)
    ((null (cdr list)) list)
    ((null (cddr list)) (list (car list)
			      last
			      (cadr list)))
    (t (cons (car list)
	     (cons delimeter
		   (intersperse (cdr list) delimeter :last last))))))


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

(defun remove-keyword-parameters (parameter-list &rest keywords)
  "Removes all parameters with keys in 'keywords' from
'parameter-list'."
  (loop for argument in keywords
        with i = parameter-list
        do (setf i (remove-keyword-parameter i argument))
        finally (return i)))

(defun tokenize-uri (uri &optional (remove-app-prefix t) (app (when remove-app-prefix
                                                                (current-webapp))))
  "Tokenizes an URI into a list of elements.

ex:
\(tokenize-uri \"/hello/world/blah\\test\\hala/world?hello=5;blah=7\"
=> (\"hello\" \"world\" \"blah\" \"test\" \"hala\" \"world\")"
  (loop for token in (cl-ppcre:split "[/\\\\]" 
				     (cl-ppcre:regex-replace "\\?.*"
							     (if remove-app-prefix
								 (subseq uri
                                                                         (length
                                                                           (webapp-prefix
                                                                             app)))
								 uri)
							     ""))
     unless (string-equal "" token)
       collect (url-decode token)))

(defun public-file-relative-path (type filename)
  "Constructs a relative path to a public file from the \"/pub\" directory.

'type' - currently either :stylesheet or :script
'filename' the name of the file or 'reldir/filename'

Ex:
\(public-file-relative-path :stylesheet \"navigation\")
=> #P\"stylesheets/navigation.css\""
  (merge-pathnames (make-pathname :defaults filename)
		   (make-pathname :directory `(:relative
					       ,(ecase type
						       (:stylesheet "stylesheets")
						       (:script "scripts")))
				  :type (ecase type
					  (:stylesheet "css")
					  (:script "js")))))

(defun public-files-relative-paths (&rest args)
  "A helper function that returns a list of paths for files provided
in 'args'. Each argument must be a cons cell where car is
either :stylesheet or :script and cdr is a name of the file.

Useful when generating a list of dependencies for widgets and/or the
application (see the 'dependencies' generic function and
*application-dependencies*.)

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
  (identity (cl-ppcre:regex-replace "/?(?:\\?.*)$" (request-uri*) "")))

(defun string-remove-left (str prefix &key ignore-case-p)
  "If string 'str' starts with 'prefix', remove 'prefix' from the
start of 'str'."
  (when (string-starts-with str prefix
			    :test (if ignore-case-p #'char-equal #'char=))
    (subseq str (length prefix))))

(defun string-remove-right (str suffix &key ignore-case-p)
  "If string 'str' ends with 'suffix', remove 'suffix' from the end of
'str'."
  (when (string-ends-with str suffix
			  :test (if ignore-case-p #'char-equal #'char=))
    (subseq str 0 (- (length str)
		     (length suffix)))))

(defun find-all (sequence predicate &key (key #'identity))
  "Returns a sequence of all elements found in 'sequence' that match
'predicate'. If 'key' is provides, each it is used to retreive each
item before passing it to 'predicate'."
  (loop for i in sequence
       when (funcall predicate (funcall key i))
       collect i))

(defun stable-set-difference (list-1 list-2 &key (test #'eql) (key #'identity))
  "Returns a list of element of 'list-1' that do not appear in 'list-2'. "
  (loop for i in list-1
       unless (find (funcall key i) list-2 :test test :key key)
       collect i))

;;; Status of a symbol
(defun symbol-status (symbol)
  "Returns a status of 'symbol' in its package (internal, external,
etc.)"
  (nth-value 1 (find-symbol (symbol-name symbol)
			    (symbol-package symbol))))

;;; String helpers
(defun string-invert-case (str)
  (map 'string (lambda (char)
		 (if (upper-case-p char)
		     (char-downcase char)
		     (char-upcase char)))
       (etypecase str
	 (string str)
	 (symbol (symbol-name str)))))

;;; Element insertion
(defun ninsert (list thing pos)
    (if (zerop pos)
        (cons thing list)
        (let ((old-tail (nthcdr (1- pos) list)))
          (setf (cdr old-tail) (cons thing (cdr old-tail)))
          list)))

;;; URI from pathname
(defmethod puri:uri ((thing pathname))
  (puri:uri
   (format nil "~A~{~A/~}~A~A"
	   (if (eql (car (pathname-directory thing)) :absolute) "/" "")
	   (cdr (pathname-directory thing))
	   (or (pathname-name thing) "")
	   (if (pathname-type thing)
	       (format nil ".~A" (pathname-type thing))
	       ""))))

(defun add-get-param-to-url (url name value)
  "Based on Edi's code in URL-REWRITE but uses & instead of &amp;
which is more appropriate for our uses."
  (concatenate 'string
               url
               (if (find #\? url :test #'char=)
                 "&"
                 "?")
               name
               "="
               (url-rewrite:url-encode value)))

(defun remove-parameter-from-uri (uri parameter)
  "Removes the given parameter from a URI."
  (let ((path (puri:uri-path (puri:parse-uri uri))))
    (loop for x in (get-parameters*)
       when (not (string-equal (car x) parameter))
       do (setf path (add-get-param-to-url path (car x) (cdr x))))
    path))

(defun asdf-system-directory (asdf-system-name)
  "Computes the directory in which the .asdf file for a given ASDF
system resides."
  (make-pathname :directory
		 (pathname-directory (truename (asdf:system-definition-pathname
						(asdf:find-system asdf-system-name))))))

(defun make-isearch-regex (search)
  "Create a regular expression from the user's input that tries to be
faithful to Emacs' isearch."
  (if (some #'upper-case-p search)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode nil)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode t)))

(defun hash-keys (hashtable)
  "Returns all keys in the hashtable."
  (loop for key being the hash-keys in hashtable
        collect key))

(defgeneric object-class-name (obj)
  (:documentation
   "Returns an object's class name (i.e. \"Employee\"). This method is
be used to present the name of an entity to the user. Override this
method to change the name for particular objects.")
  (:method (obj)
    (class-name (class-of obj))))

(defun append-custom-fields (custom-fields args)
  "Appends 'custom-fields' to the end of custom fields that are
already defined in 'args'."
  (append (cadr (member :custom-fields args))
	  custom-fields))

(defun find-slot-dsd (class slot-name)
  "Returns a direct-slot-definition object of a slot with 'slot-name'
in 'class'."
  (let ((class (if (symbolp class)
		   (find-class class)
		   class)))
    (or (loop
	   for dsd in (class-direct-slots class)
	   when (eq (slot-definition-name dsd) slot-name)
	   do (return dsd))
	(find-if (compose #'not #'null)
		 (mapcar (curry-after #'find-slot-dsd slot-name)
			 (class-direct-superclasses class))))))

(defun find-slot-esd (class slot-name)
  "Returns an effective-slot-definition object of a slot with
'slot-name' in 'class'."
  (let ((class (if (symbolp class)
		   (find-class class)
		   class)))
    (loop
       for esd in (class-slots class)
       when (eq (slot-definition-name esd) slot-name)
       do (return esd))))

(defun drop-last (list)
  "Returns a copy of the list without the last element."
  (reverse (cdr (reverse list))))

(defun function-designator-p (obj)
  "Returns true if the object is a function designator."
  (or (functionp obj)
      (and (symbolp obj)
	   (not (null (fboundp obj))))
      (typep obj 'funcallable-standard-object)))

(defun list-starts-with (list elements &key (test 'eq))
  "Determines if a list starts with the given elements."
  (let ((elements (ensure-list elements)))
    (if elements
	(when (funcall test (car list) (car elements))
	  (list-starts-with (cdr list) (cdr elements) :test test))
	t)))

(defun safe-subseq (sequence start &optional end)
  "A safe alternative to subseq that automatically adjust indices."
  (let ((length (length sequence)))
    (when (> start length)
      (setf start length))
    (when (and end (> end length))
      (setf end length))
    (subseq sequence start end)))

(defun find-own-symbol (name &optional (package nil packagep))
  "Like `find-symbol', but reject symbols not really in PACKAGE."
  (multiple-value-bind (sym status)
      (if packagep (find-symbol name package) (find-symbol name))
    (and (member status '(:internal :external))
	 (values sym status))))

;;; working with those pesky slashes
(defun remove-spurious-slashes (str)
  "Condense multiple consecutively occuring slashes in STR
into a single slash.

ex:

(remove-spurious-slashes \"/ab/////c///\")
=> \"/ab/c/\""
  (with-output-to-string (s)
    (loop for c across str
          with last-char
          unless (and (eql c #\/) (eql last-char #\/))
            do (princ c s)
          do (setf last-char c))
    s))

(defun strip-trailing-slashes (str)
  "Relentlessly strip all trailing slashes from STR.
A string solely consisting of slashes is no special case.

Returns the number of stripped slashes as second value."
  ;; we just count them and return an appropriate subsequence
  (let ((n 0))
    (loop for c across (reverse str)
          while (eql c #\/)
          do (incf n))
    (values (subseq str 0 (- (length str) n)) n)))

(defun maybe-add-trailing-slash (s)
  "Supply a trailing slash if needed."
  (typecase s
    (pathname (fad:pathname-as-directory s))
    (otherwise
       (if (equal (subseq s (1- (length s))) "/")
	   s
	   (concatenate 'string s "/")))))

(defun congruent-lambda-expression (lambda-list function)
  "Answer a lambda expression with LAMBDA-LIST that passes all
args (assuming the call is allowed by LAMBDA-LIST) to FUNCTION,
answering its result."
  (let* ((reqs (required-args lambda-list))
	 (opts (aand (member '&optional lambda-list)
		     (loop for llelt in (cdr it)
			   until (member llelt lambda-list-keywords)
			   if (consp llelt)
			     collect (list (first llelt) (second llelt)
					   (or (third llelt) (gensym "OPTP")))
			   else collect (list llelt nil (gensym "OPTP")))))
	 (keys? (member '&key lambda-list))
	 (more (and (or keys? (position-if (f_ (member _ '(&body &rest)))
					   lambda-list))
		    (gensym "MORE"))))
    `(lambda (,@reqs
	      ,@(and opts (cons '&optional opts))
	      ,@(and more (list '&rest more))
	      ,@(and keys? '(&key &allow-other-keys)))
       ,(if (or opts more)
	    `(apply ',function ,@reqs
		    ,(reduce (lambda (optarg rest)
			       `(and ,(third optarg)
				     (cons ,(first optarg) ,rest)))
			     opts :from-end t :initial-value more))
	    `(funcall ',function ,@reqs)))))


(defmacro with-file-write ((stream-name path) &body body)
  `(progn
     (ensure-directories-exist ,path)
     (with-open-file (,stream-name ,path :direction :output :if-exists
				   :supersede :if-does-not-exist :create)
       ,@body)))

(defun write-to-file (object path)
  (with-file-write (stream path)
    (write object :stream stream)))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (eval (read stream nil nil))))

(defun slurp-file (filepath)
  (let* ((stream (open filepath))
	 (seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun merge-files (file-list saved-path)
  (with-file-write (stream saved-path)
	(dolist (file file-list)
	  (write-string (slurp-file file) stream))))

(defun relative-path (full-path prefix-path)
  (make-pathname :directory (cons :relative
				  (nthcdr (length (pathname-directory prefix-path))
					  (pathname-directory full-path)))
		 :name (pathname-name full-path)
		 :type (pathname-type full-path)))

(defun modified-time-path (real-file-path &key (system :weblocks) (modified-time-folder "modified-time/"))
  (let ((system-folder (asdf-system-directory system)))
    (merge-pathnames (relative-path real-file-path system-folder)
		     (merge-pathnames modified-time-folder system-folder))))


(defun previous-modified-time (record-file-path)
  (read-from-file record-file-path))

(defun write-to-modified-time (time record-path)
  (write-to-file time record-path))

(defun file-modified-p (file-path)
  (let ((record-path (modified-time-path file-path))
	(modified-time (file-write-date file-path)))
    (if (and (cl-fad:file-exists-p record-path)
	     (= (file-write-date file-path)
		(previous-modified-time record-path)))
	nil
	(progn
	  (write-to-modified-time modified-time record-path)
	  t))))

(defun files-modified-p (path-list)
  (let ((modified nil))
    (dolist (path path-list modified)
      (when (file-modified-p path)
	(setf modified t)))))