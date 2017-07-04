(defpackage #:weblocks.dependencies
  (:use #:cl)
  (:export
   #:dependency
   #:static-dependency
   #:get-content-type
   #:get-path
   #:make-static-js-dependency
   #:serve
   #:make-static-css-dependency
   #:get-route
   #:get-dependencies
   #:*page-dependencies*
   #:render-in-head
   #:remote-dependency
   #:make-remote-js-dependency
   #:make-remote-css-dependency
   #:get-integrity
   #:get-cross-origin
   #:make-static-image-dependency
   #:*cache-remote-dependencies-in*
   #:get-url))
(in-package weblocks.dependencies)


(defvar *page-dependencies* nil
  "A list which contains all page dependencies.

Weblocks fills this list during page rendering.")


(defvar *cache-remote-dependencies-in* nil
  "If this variable is set to a pathname, then
remote dependencies will be cached and served as local dependencies.

This pathname should point to a directory where cached dependencies will
be stored.")


(defclass dependency ()
  ((route :initarg :route
          :initform nil
          :reader get-route)))


(defclass remote-dependency (dependency)
  ((content-type :type string
                 :initarg :content-type
                 :reader get-content-type)
   (url :type string
        :initarg :url
        :reader get-url)
   (integrity :type string
              :initarg :integrity
              :initform nil
              :reader get-integrity
              :documentation "A hash, used by modern browsers for subresource integrity checking.

See more information at: https://www.w3.org/TR/SRI/")
   (cross-origin :type string
                 :initarg :cross-origin
                 :initform "anonymous"
                 :reader get-cross-origin)))


(defclass static-dependency (dependency)
  ((content-type :type string
                 :initarg :content-type
                 :reader get-content-type)
   (path :type pathname
         :initarg :path
         :reader get-path)
   (binary :type bool
           :initarg :binary
           :initform nil
           :reader is-binary)))


(defgeneric get-url (dependency)
  (:documentation "Returns URL of the dependency.

If dependency should be served by the server, second value is :local.
Otherwise it is :external. Also, in first case dependency's URL
should have only path part, like /static/css/bootstrap.css."))


(defgeneric render-in-head (dependency)
  (:documentation "Renderns a piece of html.")
  (:method (dependency)
    (format weblocks:*weblocks-output-stream*
            "<!-- Dependency ~S -->"
            dependency)))


(defmethod render-in-head ((dependency dependency))
  (cond
    ;; Javascript
    ((equal (get-content-type dependency)
            "application/javascript")
     (weblocks::with-html
       (:script :src (get-url dependency)
                :type "text/javascript" "")))
    ;; CSS
    ((equal (get-content-type dependency)
            "text/css")
     (weblocks::with-html
       (:link :rel "stylesheet" :type "text/css"
              :href (get-url dependency)
              :media "screen")))))


(defmethod render-in-head ((dependency remote-dependency))
  (if (equal (get-content-type dependency)
             "application/javascript")
      ;; Javascript
      (weblocks::with-html
        (:script :src (get-url dependency)
                 :type "text/javascript" ""))
      ;; CSS
      (weblocks::with-html
        (:link :rel "stylesheet" :type "text/css"
               :href (get-url dependency)
               :media "screen"
               :integrity (get-integrity dependency)
               :cross-origin (get-cross-origin dependency)))))


(defgeneric serve (dependency)
  (:documentation "Returns two values - content and content-type.

Example output::

  (values \"body {background: light-green;}\"
          \"text/css\")"))


(defgeneric get-dependencies (object)
  (:documentation "Returns a list of object's dependencies.

Object could be an application or a widget.
Weblocks will call this method for application and every widget on a page
to gather necessary dependencies and to inject them into resulting HTML.")

  (:method ((object t))
    "By default, there are no dependencies for an object."
    nil))


(defmethod initialize-instance :after ((dependency static-dependency)
                                       &rest initargs)
  "Creating a route for the dependency if it is \"local\".

This way it will be possible to serve requests for this dependency from
a browser."
  
  (declare (ignorable initargs))

  ;; Each dependency should have an url.
  (multiple-value-bind (url type)
      (get-url dependency)

    ;; But we only interested in a local dependencies,
    ;; not in those which will be served by CDN.
    (when (eql type
               :local)

      ;; And now we will bind a route to dependency and vice-versa.
      (setf (slot-value dependency
                        'route)
            (weblocks.routes:make-route url
                                        dependency)))))


(defmethod initialize-instance :after ((dependency remote-dependency)
                                       &rest initargs)
  "Creating a route for the dependency if it is \"local\".

This way it will be possible to serve requests for this dependency from
a browser."
  
  (declare (ignorable initargs))

  ;; Each dependency should have an url.
  (multiple-value-bind (url type)
      (get-url dependency)

    ;; But we only interested in a local dependencies,
    ;; not in those which will be served by CDN.
    (when (eql type
               :local)

      ;; And now we will bind a route to dependency and vice-versa.
      (setf (slot-value dependency
                        'route)
            (weblocks.routes:make-route url
                                        dependency)))))



(defun make-static-js-dependency (path &key system)
  "Creates a JavaScript dependency, served from the disk.

If system's name was give, then path is calculated relative
to this system's source root."
  
  (make-instance 'static-dependency
                 :content-type "application/javascript"
                 :path (if system
                           (asdf:system-relative-pathname system
                                                          path)
                           path)))


(defun make-static-css-dependency (path &key system)
  "Creates a CSS dependency, served from the disk.

If system's name was give, then path is calculated relative
to this system's source root."
  
  (make-instance 'static-dependency
                 :content-type "text/css"
                 :path (if system
                           (asdf:system-relative-pathname system
                                                          path)
                           path)))


(defun make-static-image-dependency (path &key system)
  "Creates an image dependency, served from the disk.

It is not rendered into an HTML, but served from disk."
  
  (let* ((path (if system
                  (asdf:system-relative-pathname system
                                                 path)
                  (pathname path)))
         (ext (pathname-type path))
         (content-type (format nil "image/~a"
                               ext)))
    
    (make-instance 'static-dependency
                   :content-type content-type
                   :path path
                   :binary t)))


(defun make-remote-js-dependency (url &key integrity
                                           (cross-origin "anonymous"))
  "Creates a JavaScript dependency, served from CDN."
  (make-instance 'remote-dependency
                 :content-type "application/javascript"
                 :url url
                 :integrity integrity
                 :cross-origin cross-origin))


(defun make-remote-css-dependency (url &key integrity
                                            (cross-origin "anonymous"))
  "Creates a CSS dependency, served from CDN."
  (make-instance 'remote-dependency
                 :content-type "text/css"
                 :url url
                 :integrity integrity
                 :cross-origin cross-origin))


(defmethod serve ((dependency static-dependency))
  "Serves static dependency from the disk."
  (values (pathname (get-path dependency))
          (get-content-type dependency)))


(defmethod serve ((dependency remote-dependency))
  "Serves remote dependency from local cache."
  (let ((local-path (pathname (get-path dependency))))
    
    (unless (cl-fad:file-exists-p local-path)
      (let* ((url (slot-value dependency 'url))
             (input (dex:get url
                             :want-stream t
                             :force-binary t)))
        
        (ensure-directories-exist local-path)
        
        (with-open-file (output local-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (cl-fad:copy-stream input output))))
    
    (values local-path
            (get-content-type dependency))))


(defmethod get-url ((dependency static-dependency))
  "Returns dependency's url and it's type.

URL type is returned as second value and can be :local or :remote.
For static-dependency it is :local."
  
  (let* ((path (get-path dependency))
         ;; (parts (split-sequence:split-sequence #\/ path))
         ;; (filename (car (last parts)))
         (filename (pathname-name path))
         (ext (pathname-type path))
         (prefix (format nil "/static/~a/"
                         ext))
         (filename-with-prefix (concatenate 'string
                                            prefix
                                            filename
                                            "."
                                            ext)))
    (values filename-with-prefix :local)))


(defmethod get-url ((dependency remote-dependency))
  (let* ((url (slot-value dependency 'url)))
    (if *cache-remote-dependencies-in*
        (values (concatenate 'string "/remote-deps-cache/"
                             (weblocks::md5 url))
                :local)
        (values url
                :remote))))

(defmethod get-path ((dependency remote-dependency))
  "Returns a path to cached file."
  (check-type *cache-remote-dependencies-in* pathname)
  (assert (cl-fad:directory-pathname-p
           *cache-remote-dependencies-in*))

  ;; Here we didn't use get-url method because when caching is
  ;; turned on, it will return a local uri not the original URL
  (let* ((url (slot-value dependency 'url))
         (hash (weblocks::md5 url)))
    (merge-pathnames hash *cache-remote-dependencies-in*)))

