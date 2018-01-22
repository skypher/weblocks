(defpackage #:weblocks/widgets/dom
  (:use #:cl)
  (:import-from #:weblocks/session
                #:in-session-p
                #:gen-id)
  (:import-from #:weblocks/utils/string
                #:attributize-name)
  (:export #:dom-object-mixin
           #:dom-id))
(in-package weblocks/widgets/dom)


(defclass dom-object-mixin ()
  ((dom-id :initarg :dom-id
           :documentation "The DOM id of an object. Can be a symbol, a
           string or nil. When accessed through the 'dom-id' accessor,
           will always become a string. Use ensure-dom-id or
           widget-name (for widgets) to access its underlying
           implementation."))
  (:documentation "Represents attributes and functionality common to all
  DOM-renderable objects."))

(defgeneric ensure-dom-id (obj)
  (:documentation "Ensure that the object has a 'dom-id' and return
  it. 'dom-id' is lazily generated on first read, because its creation
  requires a session to be present. Returns a string, symbol, or nil.")
  (:method ((obj dom-object-mixin))
    (if (slot-boundp obj 'dom-id)
        (slot-value obj 'dom-id)
        
        (when (in-session-p)
          (setf (slot-value obj 'dom-id)
                (gen-id))))))

(defgeneric dom-id (obj)
  (:documentation "Provides a consistent interface to identifying widgets
by their DOM id. Returns a string or nil if the object is to have no id.")
  (:method ((obj dom-object-mixin))
    (let ((id (ensure-dom-id obj)))
      (and id (attributize-name id))))
  (:method ((obj symbol)) nil)
  (:method ((obj function)) nil)
  (:method ((obj string)) nil))

(defgeneric (setf dom-id) (id obj)
  (:method (id (obj dom-object-mixin))
    (with-slots (dom-id) obj
      (setf dom-id id))))


