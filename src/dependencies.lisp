(in-package :weblocks)

(export '(dependency url-dependency-mixin stylesheet-dependency script-dependency
	  javascript-code-dependency
	  dependency-url
	  dependencies-equalp dependencies-lessp
	  render-dependency-in-page-head render-dependency-in-page-body-top
	  render-dependency-in-page-body-bottom render-dependency-in-form-submit
	  render-dependency-in-ajax-response
	  make-local-dependency per-class-dependencies dependencies
	  compact-dependencies))

;; Dependencies.
;;
;; Context: weblocks used to be very limited as to what dependencies can
;; be specified. I've been reworking this a number of times in order to
;; fit various new requirements and finally decided that it is time to
;; redesign dependency tracking completely.
;;
;; Rationale: we need to be able to gather various kinds of dependencies
;; from renderable objects (widgets, views, presentations). Dependencies
;; are local (to our webserver) or external (external URLs), stylesheet
;; or javascript code, and some could be introduced during AJAX page
;; updates. We need to be able to handle all of these, where handling
;; means: gather, compare and remove duplicates, detect new
;; dependencies, render (possibly generating code that calls back
;; javascript callbacks after updates have been completed).
;;
;; It looks complex, because the problem domain is complex. There is no
;; escape from that. Fortunately, all that complexity will be hidden
;; behind the scenes. Most people will only need to define one method
;; for their new widget and that method will simply return one dependency.
;;
;; --Jan Rychter <jan@rychter.com>

(defclass dependency ()
  ()
  (:documentation "Models a dependency of a particular renderable
  item (widget, view, presentation) on a resource defined elsewhere
  within the page (CSS, external JavaScript, JavaScript code)."))

(defclass url-dependency-mixin ()
  ((url :accessor dependency-url :initarg :url
	:documentation "URL to the object, represented as a string. May
	be a relative or an absolute URL. Using the reader for this slot
	always returns an URI object (as defined by the PURI
	library)."))
  (:documentation "Any dependency which can be represented by an external URL."))

(defmethod dependency-url ((obj url-dependency-mixin))
  "Reading dependency-url always returns an URI object"
  (puri:uri (slot-value obj 'url)))

(defclass stylesheet-dependency (dependency url-dependency-mixin)
  ((media :accessor stylesheet-media :initarg :media :initform nil))
  (:documentation "A CSS stylesheet dependency"))

(defclass script-dependency (dependency url-dependency-mixin)
  ()
  (:documentation "A JavaScript file dependency"))

(defclass javascript-code-dependency (dependency)
 ((code :accessor javascript-code :initarg :code
	:documentation "String containing the javascript code."))
  (:documentation "JavaScript code that is to be placed within the page
  being rendered or sent as part of an AJAX update."))


(defgeneric dependencies-equalp (d1 d2)
  (:documentation "Determine whether two dependencies are equal. We need
to be able to compare dependencies in order to 1) remove duplicates from
dependencies we gathered from the widget tree and 2) be able to detect
when new dependencies appeared in AJAX page updates.")
  ;; by default dependencies aren't equal
  (:method (d1 d2) nil)		  
  ;; we also know how to compare uris
  (:method ((d1 url-dependency-mixin) (d2 url-dependency-mixin))
    (puri:uri= (dependency-url d1) (dependency-url d2))))

(defgeneric dependencies-lessp (d1 d2)
  (:documentation "Used for ordering dependencies, determines which ones
  will come first in the page header.")

  ;; in the general case, it doesn't matter
  (:method (d1 d2) nil)

  ;; stylesheets come before scripts
  (:method ((d1 stylesheet-dependency) (d2 script-dependency)) t)
  (:method ((d1 script-dependency) (d2 stylesheet-dependency)) nil))

(defun prune-dependencies (dependency-list)
  "Remove duplicates from a list of dependencies."
  (remove-duplicates dependency-list :test #'dependencies-equalp :from-end t))

(defun sort-dependencies-by-type (dependency-list)
  (sort dependency-list #'dependencies-lessp))

(defgeneric compact-dependencies (dependency-list)
  (:documentation "Provides a hook for dependency processing. The
  default implementation just removes duplicates and sorts
  dependencies. By adding :before, :after or :around methods one could
  do more interesting things, such as combining differences.")
  (:method (dependency-list) (sort-dependencies-by-type (prune-dependencies dependency-list))))

;; Dependency rendering protocol

(defgeneric render-dependency-in-page-head (obj)
  (:method (obj)))

(defgeneric render-dependency-in-page-body-top (obj)
  (:method (obj)))

(defgeneric render-dependency-in-page-body-bottom (obj)
  (:method (obj)))

(defgeneric render-dependency-in-form-submit (obj)
  (:method (obj)))

(defgeneric render-dependency-in-ajax-response (obj)
  (:method (obj)))


;; Rendering implementations

(defmethod render-dependency-in-page-head ((obj stylesheet-dependency))
  (with-html
    (:link :rel "stylesheet" :type "text/css"
	   :href (dependency-url obj)
	   :media (stylesheet-media obj))))

(defmethod render-dependency-in-page-head ((obj script-dependency))
  (with-html
    (:script :src (dependency-url obj) :type "text/javascript" "")))

(defmethod render-dependency-in-page-body-top ((obj javascript-code-dependency))
  (with-javascript obj))

(defmethod render-dependency-in-page-body-bottom ((obj javascript-code-dependency))
  (with-javascript obj))

(defmethod render-dependency-in-form-submit ((obj javascript-code-dependency))
  (javascript-code obj))

;; since rendering dependencies for views is more complex than a simple mapc, there is a utility function
(defun render-form-submit-dependencies (dependency-list)
  (let ((code-string (reduce (lambda (e v)
	    (concatenate 'string e (render-dependency-in-form-submit v)))
	  (remove nil dependency-list)
	  :initial-value "")))
    (if (equalp code-string "")
	nil
	code-string)))

;; Dependency gathering

(defun make-local-dependency (type file-name &key do-not-probe media)
  "Make a local (e.g. residing on the same web server) dependency of
type :stylesheet or :script. Unless :do-not-probe is set, checks if
file-name exists in the server's public files directory, and if it does,
returns a dependency object."
  (when (or do-not-probe (probe-file (merge-pathnames (public-file-relative-path type file-name)
						      *public-files-path*)))
    (let ((full-path (merge-pathnames (public-file-relative-path type file-name)
				      (make-pathname :directory '(:absolute "pub")))))
      (ecase type
	(:stylesheet (make-instance 'stylesheet-dependency
				    :url full-path :media media))
	(:script (make-instance 'script-dependency :url full-path))))))


(defun dependencies-by-symbol (symbol)
  "A utility function used to help in gathering dependencies. Determines
dependencies for a particular symbol, which could (but doesn't have to)
represent a class."
  (let ((attributized-widget-class-name (attributize-name symbol)))
    (loop for type in '(:stylesheet :script)
       collect (make-local-dependency type attributized-widget-class-name))))

(defgeneric per-class-dependencies (obj)
  (:documentation "Return a list of dependencies for an object of a
  particular class. For widgets, this method is defined by defwidget
  automatically. If you don't use defwidget or if you want other
  renderable types to have per-class-dependencies, you need to take care
  to define this method for your class.

  The default implementation for widgets uses the following protocol to
  determine if a widget has dependencies. It looks under
  *public-files-path*/scripts/[attributized-widget-class-name].js and
  *public-files-path*/stylesheets/[attributized-widget-class-name].css. If
  it finds the aforementioned files, it returns them as
  dependencies. This way the developer can simply place relevant files
  in the appropriate location and not worry about specializing this
  function most of the time.")
  (:method-combination append)
  ;; no dependencies by default
  (:method append (obj) ()))


(defgeneric dependencies (obj)
  (:documentation "Return a list of dependencies for a particular
  object.

  Whenever a widget is rendered by weblocks, this function is called to
  determine which stylesheets, javascript files and javascript code the
  widget depends on. These dependencies are then processed and included
  in various places (header, body, AJAX page update) of the containing
  page.

  The default implementation for widgets finds some dependencies
  automatically based on the widget's class name, see
  'per-class-dependencies for details.

  This method uses the append method combination to also return
  dependencies for the superclasses of 'obj', because it's intuitive to
  assume that a widget will use the stylesheets of its superclasses.")

  (:method-combination append)

  ;; To actually gather dependencies, we need to 1. gather everything
  ;; from user-defined append methods and 2. traverse the class tree and
  ;; gather all class-related dependencies. This :around method collects
  ;; everything and removes empty dependencies.
  (:method :around (obj) (remove nil (append (call-next-method) (per-class-dependencies obj))))

  ;; No dependencies by default
  (:method append (obj) ())

  ;; For strings and symbols we try a filename lookup.
  (:method append ((obj symbol)) (dependencies-by-symbol obj))
  (:method append ((obj string)) (dependencies-by-symbol obj)))

