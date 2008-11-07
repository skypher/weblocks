
(in-package :weblocks-test)

(defparameter *diff-shell-command* "diff -du"
  "Shell command used to compare the expected against actual output.")

;;;; make-temporary-file and such

#+cffi
(progn
  (cffi:defcfun ("tmpnam" c-tmpnam) :string
    (result :pointer))
  (defun simple-tmpnam ()
    (c-tmpnam (cffi:null-pointer))))
#-cffi
(defun simple-tmpnam ()
  "Like the C function `tmpnam', but woefully incomplete, insecure,
and suitable only for Weblocks test runners."
  (let ((name-part (format nil "wbtmp~36,6,'0R" (random 2176782335))))
    (if (cl-fad:directory-exists-p #P"/tmp/")
	(merge-pathnames name-part #P"/tmp/")
	(merge-pathnames name-part (asdf-system-directory '#:weblocks)))))

(defun make-temporary-file
    (&rest open-args &key (direction :output) &allow-other-keys)
  "Answer a newly opened temporary file."
  (let ((tries 20))
    (tagbody
     restart
       (let ((tmpnam (simple-tmpnam)))
	 (handler-bind ((file-error (lambda (err)
				      (declare (ignore err))
				      (unless (zerop (decf tries))
					(go restart)))))
	   (return-from make-temporary-file
	     (apply #'open tmpnam :if-exists :error
		    :direction direction open-args)))))))

(defun call/tempfile (proc &rest open-args)
  "Call PROC with an open temporary file stream, passing in
OPEN-ARGS.  Delete after exiting."
  (let* ((tmpfile (apply #'make-temporary-file open-args))
	 (fname (pathname tmpfile)))
    (unwind-protect
	 (unwind-protect (funcall proc tmpfile)
	   (close tmpfile))
      (handler-case (delete-file fname) (file-error ())))))

;;;; diff invocation

(defun shell-command-proc ()
  (let ((tspak (find-package '#:trivial-shell)))
    (and tspak
	 (find-symbol (symbol-name '#:shell-command) tspak))))

(defun diff-strings (old new)
  "Answer the result of running `*diff-shell-command*' on OLD and NEW
strings."
  (call/tempfile
   (lambda (oldf)
     (call/tempfile
      (lambda (newf)
	(princ old oldf)
	(princ new newf)
	(force-output oldf)
	(force-output newf)
	(funcall (shell-command-proc)
		 (format nil "~A ~A ~A" *diff-shell-command*
			 (pathname oldf) (pathname newf))))))))

;;;; LIFT extensions

(defun emulate-lift-kwargs (kwargs)
  "Quote arguments where appropriate for macroexpansion of ensure-*
forms."
  (setf kwargs (copy-list kwargs))
  (loop for keypair on kwargs by #'cddr
	for (key val) = keypair
        if (and (eq key :test) (symbolp val))
        do (setf (second keypair) `',val))
  kwargs)

(defun %ensure-nodiff (actual-values expected-values test-form
		       &key (test *lift-equality-test*))
  "Helper for `ensure-nodiff'."
  (declare (ignore test-form))
  (ensure-same (length actual-values) (length expected-values) :test =)
  (mapc (lambda (actual expected)
	  (unless (funcall test actual expected)
	    (let (report report-args)
	      (when (and (stringp expected) (shell-command-proc))
		(setf report "Differences found:~%~A~%"
		      report-args (list (diff-strings expected actual))))
	      (apply #'lift::maybe-raise-not-same-condition
		     actual expected test report report-args))
	    (return-from %ensure-nodiff nil)))
	actual-values expected-values)
  t)

(defmacro ensure-nodiff (form values &rest key-args)
  "As with `ensure-same', but when the expected value is a string, use
a special reporter that prints the output of `*diff-shell-command*'
run on the expected and actual values.

See `%ensure-nodiff' for available keyword args."
  (setf key-args (emulate-lift-kwargs key-args))
  `(%ensure-nodiff
    (multiple-value-list ,form) (multiple-value-list ,values)
    ',form . ,key-args))

(defun %ensure-same-html (actual-values expected-values test-form
			  &rest %ensure-nodiff-args)
  "Helper for `ensure-same-html'."
  (flet ((maybe-split-html (obj)
	   (if (and (stringp obj) (length-at-least-p obj 1)
		    (char= (char obj 0) #\<))
	       (cl-ppcre:regex-replace-all "><" obj ">
<")
	       obj)))
    (apply #'%ensure-nodiff (mapcar #'maybe-split-html actual-values)
	   (mapcar #'maybe-split-html expected-values)
	   test-form %ensure-nodiff-args)))

(defmacro ensure-same-html (form values &rest key-args)
  "As with `ensure-nodiff', but first transforming the HTML strings
into values more suitable for diffing.

See `%ensure-nodiff' for available keyword args."
  (setf key-args (emulate-lift-kwargs key-args))
  `(%ensure-same-html
    (multiple-value-list ,form) (multiple-value-list ,values)
    ',form . ,key-args))

;;;; HTML comparison (what `deftest-html' does)

(defun call-with-fake-output (thunk)
  "Rebind `*weblocks-output-stream*' and others necessary, call THUNK,
restore the old context, and answer the string of whatever was written
to it in the meantime."
  (let (*weblocks-output-stream* (*test-widget-id* 1000))
    (with-output-to-string (*weblocks-output-stream*)
      (funcall thunk))))

(defmacro ensure-html-output (form cl-whovian &rest ensure-same-html-args)
  "As with `ensure-same-html', but capture the output during FORM
instead, comparing it to CL-WHO-formatted CL-WHOVIAN.  Remaining args
are passed to `ensure-same-html'."
  `(ensure-same-html
    (call-with-fake-output (f0 ,form))
    (with-html-output-to-string (,(gensym "S")) ,cl-whovian)
     . ,ensure-same-html-args))
