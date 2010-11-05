
(in-package :weblocks)

(wexport '(gen-id
	   safe-apply
           safe-funcall
           request-parameter
	   request-parameters
	   public-file-relative-path
           public-files-relative-paths
           symbol-status
           ninsert
           asdf-system-directory
           hash-keys
	   append-custom-fields
	   function-designator-p
           defrender)
	 '(t util))

(defun gen-id (&optional (prefix "dom"))
  "Generates an ID unique accross the session. The generated ID can be
used to create IDs for html elements, widgets, etc."
  (let ((new-widget-id (1+ (or (session-value 'last-unique-id) -1))))
    (setf (session-value 'last-unique-id) new-widget-id)
    (apply #'concatenate 'string (mapcar #'princ-to-string (list prefix new-widget-id)))))

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
  (when (eq (request-method*) :head)
    (warn "User agent ~S sent a HEAD request" (hunchentoot:user-agent)))
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

(defun public-file-relative-path (type filename)
  "Infer FILENAME's relative path and extension from TYPE.

Example:

\(public-file-relative-path :stylesheet \"navigation\")
=> \"stylesheets/navigation.css\""
  (multiple-value-bind (folder ext) (ecase type
                                      (:stylesheet (values "stylesheets" "css"))
                                      (:script (values "scripts" "js")))
    (concatenate 'string folder "/" filename "." ext)))

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

;;; Status of a symbol
(defun symbol-status (symbol)
  "Returns a status of 'symbol' in its package (internal, external,
etc.)"
  (nth-value 1 (find-symbol (symbol-name symbol)
			    (symbol-package symbol))))

(defvar *asdf-system-cache* (make-hash-table :test #'equalp))

(defun asdf-system-directory (asdf-system-name)
  "Computes the directory in which the .asdf file for a given ASDF
system resides."
  (sor (gethash asdf-system-name *asdf-system-cache*)
       (setf it (make-pathname :directory
                               (pathname-directory
                                 (truename
                                   (asdf:system-definition-pathname
                                     (asdf:find-system asdf-system-name))))))))

(defun hash-keys (hashtable)
  "Returns all keys in the hashtable."
  (loop for key being the hash-keys in hashtable
        collect key))

(defun append-custom-fields (custom-fields args)
  "Appends 'custom-fields' to the end of custom fields that are
already defined in 'args'."
  (append (cadr (member :custom-fields args))
	  custom-fields))

(defun function-designator-p (obj)
  "Returns true if the object is a function designator."
  (or (functionp obj)
      (and (symbolp obj)
	   (not (null (fboundp obj))))
      (typep obj 'funcallable-standard-object)))

(defun find-own-symbol (name &optional (package nil packagep))
  "Like `find-symbol', but reject symbols not really in PACKAGE."
  (multiple-value-bind (sym status)
      (if packagep (find-symbol name package) (find-symbol name))
    (and (member status '(:internal :external))
	 (values sym status))))

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


(defmacro with-file-write ((stream-name path &key (element-type ''base-char))
                           &body body)
  `(progn
     (ensure-directories-exist ,path)
     (with-open-file (,stream-name ,path :direction :output
                                         :element-type ,element-type
				         :if-exists #+ccl :overwrite #-ccl :supersede
                                         :if-does-not-exist :create)
       ,@body)))

(defun write-to-file (object path)
  (with-file-write (stream path)
    (write object :stream stream)))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (eval (read stream nil nil))))

(defun slurp-file (filepath &key (element-type 'base-char))
  (with-open-file (stream filepath :element-type element-type)
    (let ((seq (make-array (file-length stream) :element-type element-type)))
      (read-sequence seq stream)
      seq)))

(defun merge-files (file-list saved-path
		    &key (element-type '(unsigned-byte 8)) linkage-element-fn)
  (with-file-write (stream saved-path :element-type element-type)
    (write-sequence (slurp-file (car file-list) :element-type element-type)
		    stream)
    (dolist (file (cdr file-list))
      (when linkage-element-fn
	(funcall linkage-element-fn stream))
      (write-sequence (slurp-file file :element-type element-type)
		      stream))))

(defun merge-files-with-newline (file-list saved-path)
  (merge-files file-list saved-path
	       :linkage-element-fn (lambda (stream) (write-byte 10 stream))))

(defun relative-path (full-path prefix-path)
  (princ-to-string
   (make-pathname :directory (cons :relative
				   (nthcdr (length (pathname-directory prefix-path))
					   (pathname-directory full-path)))
		  :name (pathname-name full-path)
		  :type (pathname-type full-path))))

(defun gzip-file (input output &key (if-exists #+ccl :overwrite #-ccl :supersede) (if-does-not-exist :create)
		  (minimum-length 300))
  "Redefined salsa2:gzip-file with more keywords."
  (with-open-file (istream input :element-type '(unsigned-byte 8))
    (unless (< (file-length istream) minimum-length)
      (with-open-file (ostream output
			       :element-type '(unsigned-byte 8)
			       :direction :output
			       :if-does-not-exist if-does-not-exist
			       :if-exists if-exists)
	(salza2:gzip-stream istream ostream)))
    (probe-file output)))

(defmacro defrender (widget-type &body body)
  `(defmethod render-widget-body ((widget ,widget-type) &rest args)
     (declare (ignore args))
     (catch 'abort-widget-rendering
       (block nil
         ,@body))))

