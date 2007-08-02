;;; Code shared accross the entire weblocks framework
(defpackage #:weblocks
  (:use :cl :c2mop :metabang.utilities :moptilities :hunchentoot :cl-who :json)
  (:shadowing-import-from :c2mop #:defgeneric
			  #:standard-generic-function #:defclass #:ensure-generic-function
			  #:standard-class #:defgeneric #:standard-generic-function #:defclass
			  #:ensure-generic-function #:standard-class)
  (:documentation
   "Weblocks is a Common Lisp framework that eases the pain of
web application development. It achieves its goals by
standardizing on various libraries, providing flexible and
extensible generic renderers, and exposing a unique widget-based
approach to maintaining UI state."))

(in-package :weblocks)

(export '(*weblocks-output-stream* *current-navigation-url*
	  *application-public-dependencies* defwebapp with-html
	  reset-sessions str server-type server-version
	  with-javascript public-file-relative-path
	  public-files-relative-paths))

(defparameter *weblocks-output-stream* nil
  "Output stream for Weblocks framework created for each request
and available to code executed within a request as a special
variable. All html should be rendered to this stream.")

(defparameter *current-navigation-url* nil
  "Always contains a navigation URL at the given point in rendering
  cycle. This is a special variable modified by the navigation
  controls during rendering so that inner controls can determine their
  location in the application hierarchy.")

(defparameter *dirty-widgets* nil
  "Contains a list of dirty widgets at the current point in rendering
  cycle. This is a special variable modified by the actions that
  change state of widgets.")

(defvar *webapp-name* nil
  "The name of the currently running web application. See
'defwebapp' for more details.")

(defvar *application-public-dependencies* nil
  "A list of dependencies on scripts and/or stylesheets that will
persist throughout the whole application. See documentation for
'widget-public-dependencies' for more details.")

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

(defun defwebapp (name &key (public-dependencies
			     (public-files-relative-paths
			      '(:stylesheet . "layout")
			      '(:stylesheet . "main"))))
  "Sets the application name (the *webapp-name* variable). 'name'
must be a symbol. This symbol will later be used to find a
package that defined 'init-user-session' - a function responsible
for the web application setup.

'init-user-session' must be defined by weblocks client in the
same package as 'name'. This function will accept a single
parameter - a composite widget at the root of the
application. 'init-user-session' is responsible for adding
initial widgets to this composite.

'public-dependencies' argument can be used for convinience. If it's
specified, 'defwebapp' will assign its value to
*application-public-dependencies*. By default 'defwebapp' sets up the
application to depend on the following resources:

Stylesheets: layout.css, main.css"
  (check-type name symbol)
  (setf *webapp-name* name)
  (when public-dependencies
    (setf *application-public-dependencies* public-dependencies)))

(defmacro with-html (&body body)
  "A wrapper around cl-who with-html-output macro."
  `(with-html-output (*weblocks-output-stream* nil :indent nil)
     ,@body))

(defmacro with-javascript (source &rest args)
  "Places 'source' between script and CDATA elements. Used to avoid
having to worry about special characters in JavaScript code."
  `(with-html
     (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt ,source ,@args)
	      (fmt "~%// ]]>~%"))))

(defun server-type ()
  "Hunchentoot")

(defun server-version ()
  hunchentoot::*hunchentoot-version*)

;;; This turns off a regex optimization that eats A LOT of memory
(setq cl-ppcre:*use-bmh-matchers* nil)
