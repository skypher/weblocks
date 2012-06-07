
(in-package :weblocks)

(export '(class-id-slot-name object-id-slot-name object-id id
	  class-store object-store defstore persist-objects
	  mapstores open-stores))

(declaim (special *default-store*))

;;; Object ID management
(defgeneric class-id-slot-name (class)
  (:documentation
   "Must return the symbol that identifies the slot name which
represents the unique ID of the class named 'class'. Default
implementation returns 'ID'. Specialize this function if you want to
name the slot that holds the class' unique ID differently."))

(defmethod class-id-slot-name ((class t))
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
object identification schemes.")
  (:method ((obj null))
    nil)
  (:method ((obj standard-object))
    (let ((object-id-slot-name (object-id-slot-name obj)))
      (handler-case (when (slot-boundp obj object-id-slot-name)
		      (slot-value obj object-id-slot-name))
	(error ()
          (error "Cannot determine object ID. Object ~A has no slot '~A'."
		 obj object-id-slot-name))))))

(defgeneric (setf object-id) (id obj)
  (:documentation
   "Sets the value that uniquely identifies an object in memory or in
a backend store. The default implementation tries to set an 'id' slot
via 'slot-value'. If such slot is not present, signals an
error. Specialize this function for various back end stores and other
object identification schemes."))

(defmethod (setf object-id) (id (obj standard-object))
  (handler-case (setf (slot-value obj (object-id-slot-name obj)) id)
    (error ()
      (error "Cannot determine object ID. Object ~A has no slot '~A'."
	     obj (object-id-slot-name obj)))))

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

(defvar *stores* (make-hash-table)
  "A hashmap of stores, where each item has store name as key, and
structure of type 'store-info' as value.")

(defvar *store-names* nil
  "A list of store names in the order in which they were defined.")

(defun %defstore-predefine (name type &rest args)
  "Helper for `defstore'."
  (setf (gethash name *stores*)
	(make-store-info :type type :args args))
  (unless (find name *store-names*)
    (push-end name *store-names*))
  ;; `*package*' should be appropriate as defstore should be toplevel
  (dolist (probable-webapp-class (package-webapp-classes))
    (sunless (webapp-default-store-name probable-webapp-class)
      (setf it name))))

(defun %defstore-postdefine (name type)
  "Helper for `defstore'."
  (declare (ignore name))
  (let ((system-name (make-symbol (concatenate 'string "WEBLOCKS-" (symbol-name type)))))
    (unless (asdf:find-system system-name nil)
      (load (merge-pathnames
	     (make-pathname :directory `(:relative "src" "store"
					 ,(string-downcase (symbol-name type)))
			    :name (string-downcase (symbol-name system-name))
			    :type "asd")
	     (asdf-system-directory :weblocks))))
    (asdf:operate 'asdf:load-op system-name)))

(defmacro defstore (name type &rest store-args)
  "A macro that helps define a store. A global variable 'name' is
defined, and 'open-store' is called with appropriate store type and
arguments. Note that the last store will also be the default (see
*default-store*). All stores defined via 'defstore' will be opened and
bound when 'start-weblocks' is called, and closed when 'stop-weblocks'
is called. If `store-args' contains a keyword 'load-store-system-p'
with a value of NIL following it then the store's ASDF system
(`weblocks-STORENAME') will not be loaded afterwards. Turning this off
is useful for binary images."
  (multiple-value-bind (load-store-system-p foundp)
      (safe-getf store-args :load-store-system-p)
    (unless foundp
      (setf load-store-system-p t))
    `(progn
       (%defstore-predefine ',name ,type ,@(remove-keyword-parameter store-args :load-store-system-p))
       (defvar ,name nil)
       ,(when load-store-system-p
              `(%defstore-postdefine ',name ,type)))))

(defun open-stores ()
  "Opens and binds all stores."
  (remove-if #'null
             (mapcar (lambda (store-name)
                       (unless (symbol-value store-name)
                         (let ((store-info (gethash store-name *stores*)))
                           (setf (symbol-value store-name)
                                 (apply #'open-store
                                        (store-info-type store-info)
                                        (store-info-args store-info))))))
                     *store-names*)))

(defun close-stores ()
  "Closes all stores."
  (dolist (store-name *store-names*)
    (when (symbol-value store-name)
      (close-store (symbol-value store-name))
      (setf (symbol-value store-name) nil))))

#+sbcl(pushnew 'close-stores sb-ext:*exit-hooks*)

(defun mapstores (fn)
  "Maps a function over existing stores in the order in which they
were defined. Returns NIL."
  (dolist (store-name *store-names*)
    (when (symbol-value store-name)
      (funcall fn (symbol-value store-name)))))

;;; Persisting objects
(defun persist-objects (store objects &rest keys)
  "Persists all objects in 'objects' sequence into 'store'."
  (dolist (obj objects)
    (apply #'persist-object store obj keys)))

