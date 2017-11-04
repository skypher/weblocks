(defpackage #:weblocks.routes
  (:use #:cl)
  (:import-from #:routes
                #:route)
  (:export
   #:connect
   #:route
   #:*routes*
   #:reset-routes
   #:serve))
(in-package weblocks.routes)


(defvar *routes* (make-instance 'routes:mapper)
  "We will store mapping from URL to dependency here.")


(defun connect (route)
  "Inserts a new route into the routing table."
  (unless (routes:match *routes* route)
    (routes:connect *routes* route)))


(defun reset-routes ()
  "Resets routes before starting Weblocks server."
  (setf *routes* (make-instance 'routes:mapper)))



(defgeneric serve (route env) 
  (:documentation "Methods should return a list like that:
\(list 200                                 ;; status-code
       \(list :content-type content-type\) ;; headers
       content\)                           ;; content
"))


