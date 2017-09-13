(defpackage #:weblocks.routes
  (:use #:cl)
  (:export
   #:route
   #:get-dependency
   #:make-route
   #:*routes*
   #:register-dependencies
   #:reset-routes))
(in-package weblocks.routes)


(defvar *routes* (make-instance 'routes:mapper)
  "We will store mapping from URL to dependency here.")


(defun reset-routes ()
  "Resets routes before starting Weblocks server."
  (setf *routes* (make-instance 'routes:mapper)))


(defun register-dependencies (dependencies)
  "Adds dependencies to the router to make HTTP server handle them."
  (dolist (dependency dependencies)
    ;; to not create a circular dependency between weblocks.dependencies and weblocsk.routes
    ;; we have to intern this symbol at runtime
    (let ((route (funcall (intern "GET-ROUTE" 'weblocks.dependencies)
                          dependency)))
      (when route
        (routes:connect *routes* route)))))


(defclass route (routes:route)
  ((dependency :initform nil
               :initarg :dependency
               :reader get-dependency)))


(defclass websocket-route (routes:route)
  ())


(defun make-route (uri dependency)
  "Makes a route for dependency.

Automatically adds a prefix depending on current webapp and widget."

  (make-instance 'route
                 :template (routes:parse-template uri)
                 :dependency dependency))


(defun make-websocket-route (uri)
  "Makes a route for websocket handle.

Automatically adds a prefix depending on current webapp and widget."

  (let ((route (make-instance 'websocket-route
                              :template (routes:parse-template uri))))
    (routes:connect *routes* route)))
