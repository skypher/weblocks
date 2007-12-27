
(in-package :weblocks)

(export '(class-id-slot-name object-id-slot-name object-id id
	  class-store object-store defstore persist-objects
	  mapstores))

;;; Object ID management
(defgeneric class-id-slot-name (class-name)
  (:documentation
   "Must returns the symbol that identifies the slot name which
   represents the unique ID of the class named 'class-name'. Default
   implementation returns 'ID'. Specialize this function if you want
   to name the slot that holds the class' unique ID differently."))

(defmethod class-id-slot-name (class-name)
  (declare (ignore class-name))
  'id)

(defun object-id-slot-name (obj)
  "Returns the slot name of the slot that holds the unique identifier
of 'obj'. This information is obtained via calling
'class-id-slot-name'."
  (class-id-slot-name (class-name (class-of obj))))

(defgeneric object-id (obj)
  (:documentation
   "Returns a value that uniquely identifies an object in memory or in
a backend store. The default implementation looks for an 'id' slot via
'slot-value'. If such slot is not present, signals an
error. Specialize this function for various back end stores and other
object identification schemes."))

(defmethod object-id ((obj standard-object))
  (handler-case (slot-value obj (object-id-slot-name obj))
    (error (condition) (error "Cannot determine object ID. Object ~A has no slot 'id'." obj))))

(defgeneric (setf object-id) (id obj)
  (:documentation
   "Sets the value that uniquely identifies an object in memory or in
a backend store. The default implementation tries to set an 'id' slot
via 'slot-value'. If such slot is not present, signals an
error. Specialize this function for various back end stores and other
object identification schemes."))

(defmethod (setf object-id) (id (obj standard-object))
  (handler-case (setf (slot-value obj (object-id-slot-name obj)) id)
    (error (condition) (error "Cannot determine object ID. Object ~A has no slot 'id'." obj))))

;;; Object store location
(defgeneric class-store (class-name)
  (:documentation "Returns the store where objects of class name
should be persisted. This function should be used by widgets to
persist objects. Default implementation returns *default-store*."))

(defmethod class-store (class-name)
  (declare (ignore class-name))
  *default-store*)

(defun object-store (obj)
  "Returns the store for the object by calling 'class-store'."
  (class-store (class-name (class-of obj))))

;;; Store configuration
(defstruct store-info
  "Information about a store."
  (type nil :type symbol)
  (args nil))

(defparameter *stores* (make-hash-table)
  "A hashmap of stores, where each item has store name as key, and
structure of type 'store-info' as value.")

(defmacro defstore (name type &rest args)
  "A macro that helps define a store. A global variable 'name' is
defined, and 'open-store' is called with appropriate store type and
arguments. Note that the last store will also be the default (see
*default-store*). All stores defined via 'defstore' will be opened and
bound when 'start-weblocks' is called, and closed when 'stop-weblocks'
is called."
  (let ((system-name (gensym)))
    `(progn
       (setf (gethash ',name *stores*)
	     (make-store-info :type ,type :args ,(cons 'list args)))
       (defparameter ,name nil)
       (let ((,system-name ',(make-symbol (concatenate 'string "WEBLOCKS-" (symbol-name type)))))
	 (unless (asdf:find-system ,system-name nil)
	   (load (merge-pathnames (make-pathname :directory '(:relative "src" "store"
							      ,(string-downcase (symbol-name type)))
						 :name (string-downcase (symbol-name ,system-name))
						 :type "asd")
				  (asdf-system-directory :weblocks))))
	 (asdf:operate 'asdf:load-op ,system-name)))))

(defun open-stores ()
  "Opens and binds all stores."
  (maphash (lambda (store-name store)
	     (unless (symbol-value store-name)
	       (setf (symbol-value store-name)
		     (apply #'open-store (store-info-type store) (store-info-args store)))))
	   *stores*))

(defun close-stores ()
  "Closes all stores."
  (maphash (lambda (store-name store)
	     (declare (ignore store))
	     (when (symbol-value store-name)
	       (close-store (symbol-value store-name))
	       (setf (symbol-value store-name) nil)))
	   *stores*))

(defun mapstores (fn)
  "Maps a function over existing stores. Returns NIL."
  (maphash (lambda (store-name store)
	     (declare (ignore store))
	     (when (symbol-value store-name)
	       (funcall fn (symbol-value store-name))))
	   *stores*))

;;; Persisting objects
(defun persist-objects (store objects)
  "Persists all objects in 'objects' sequence into 'store'."
  (dolist (obj objects)
    (persist-object store obj)))

