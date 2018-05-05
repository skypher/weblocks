(defpackage #:weblocks/dependencies
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:import-from #:weblocks/utils/misc
                #:md5)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/routes
                #:add-route)
  (:import-from #:weblocks/response
                #:send-script)
  ;; Just a dependency
  (:import-from #:dexador)
  (:import-from #:alexandria
                #:make-keyword)
  
  (:export
   #:dependency
   #:local-dependency
   #:get-content-type
   #:get-path
   #:serve
   #:get-route
   #:get-dependencies
   #:render-in-head
   #:remote-dependency
   #:get-integrity
   #:get-crossorigin
   #:*cache-remote-dependencies-in*
   #:get-url
   #:infer-type-from
   #:make-dependency
   #:get-type
   #:render-in-ajax-response
   #:register-dependencies
   #:with-collected-dependencies
   #:push-dependency
   #:get-collected-dependencies
   #:push-dependencies))
(in-package weblocks/dependencies)


(defvar-unbound *page-dependencies*
  "A list which contains all page dependencies.

Weblocks fills this list during page rendering.")


(defvar *cache-remote-dependencies-in* nil
  "If this variable is set to a pathname, then
remote dependencies will be cached and served as local dependencies.

This pathname should point to a directory where cached dependencies will
be stored.")


(defclass dependency ()
  ((type :type (member :css :js :png :jpg :gif)
         :initarg :type
         :initform (error ":type argument is required.")
         :reader get-type)))


(defgeneric get-route (dependency)
  (:documentation "This method should return a routes:route object
if dependency should ber served from local server.")
  (:method (dependency)
    "By default dependencies aren't served by Lisp."
    (declare (ignore dependency))))


(defclass remote-dependency (dependency)
  ((remote-url :type string
               :initarg :remote-url
               :reader get-remote-url)
   (integrity :type (or string null)
              :initarg :integrity
              :initform nil
              :reader get-integrity
              :documentation "A hash, used by modern browsers for subresource integrity checking.

See more information at: https://www.w3.org/TR/SRI/")
   (crossorigin :type (or string null)
                :initarg :crossorigin
                :initform "anonymous"
                :reader get-crossorigin)))


(defclass local-dependency (dependency)
  ((route :initarg :route
          :initform nil
          :reader get-route)
   (path :type (or pathname null)
         :initarg :path
         :initform nil
         :reader get-path)
   (binary :type bool
           :initarg :binary
           :initform nil
           :reader is-binary)))


(defclass cached-remote-dependency (remote-dependency local-dependency)
  ()
  (:documentation "This is subclass of remote dependency which is
downloaded and stored in the local cache and served by lisp image.

You don't' need to create it manually, just set *cache-remote-dependencies-in*
variable to a path where to store data before all (make-dependency...) will be made,
and they will create cached-remote-dependency instead of remote-dependency
objects automatically."))


(defgeneric get-url (dependency)
  (:documentation "Returns URL of the dependency.

If dependency should be served by the server, second value is :local.
Otherwise it is :external. Also, in first case dependency's URL
should have only path part, like /local/css/bootstrap.css."))


(defmethod print-object ((object local-dependency) stream)
  (print-unreadable-object (object stream :type t)
    (when (get-path object)
      (format stream "path: ~S" (get-path object)))))


(defmethod print-object ((object remote-dependency) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "url: ~S" (get-remote-url object))))


(defun get-content-type (dependency)
  "Returns a MIME content type for given dependency.

It is used when dependency is served locally."
  
  (ecase (get-type dependency)
    (:js "application/javascript")
    (:css "text/css")
    (:png "image/png")
    (:jpg "image/jpeg")
    (:gif "image/gif")))


(defgeneric render-in-head (dependency)
  (:documentation "Renders a piece of html.")
  (:method (dependency)
    (with-html
      (:comment (format nil "Dependency ~S" dependency)))))


(defgeneric render-in-ajax-response (dependency)
  (:documentation "Returns a JS code to dynamically include a CSS or JS dependency
into a webpage on AJAX response.

This makes possible to load new styles and code for widgets which can appear on a page
as a response to some action.")
  (:method (dependency)
    (declare (ignorable dependency))
    (log:warn "No method to handle AJAX" dependency)))


(defmethod render-in-head ((dependency dependency))
  (case (get-type dependency)

    (:js
     (with-html
       (:script :src (get-url dependency)
                :type "text/javascript" "")))

    (:css
     (with-html
       (:link :rel "stylesheet" :type "text/css"
              :href (get-url dependency)
              :media "screen")))))


(defmethod render-in-ajax-response ((dependency dependency))
  (case (get-type dependency)
    (:js
     (let ((script (parenscript:ps* `(include_dom
                                      ,(get-url dependency)))))
       (log:debug "Rendering js dependency in ajax response" dependency)
       (send-script script :before-load)))

    (:css
     (let ((script (parenscript:ps* `(include_css
                                      ,(get-url dependency)))))
       (log:debug "Rendering css dependency in ajax response" dependency)
       (send-script script :before-load)))))


(defmethod render-in-head ((dependency remote-dependency))
  ;; For cached dependencies remote and local urls are different
  ;; to make life easier, we'll render a comment with original
  ;; dependency's URL.
  (unless (string-equal (get-url dependency)
                        (get-remote-url dependency))
    (with-html
      (:comment (get-remote-url dependency))))

  (case (get-type dependency)
    ;; Javascript
    (:js
     (with-html
       (:script :src (get-url dependency)
                :type "text/javascript"
                :integrity (get-integrity dependency)
                :crossorigin (get-crossorigin dependency))))
    ;; CSS
    (:css
     (with-html
       (:link :rel "stylesheet" :type "text/css"
              :href (get-url dependency)
              :media "screen"
              :integrity (get-integrity dependency)
              :crossorigin (get-crossorigin dependency))))))


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


(defmethod initialize-instance :after ((dependency local-dependency)
                                       &rest initargs)
  "Creating a route for the dependency if it is \"local\".

This way it will be possible to serve requests for this dependency from
a browser."
  
  (declare (ignorable initargs))
  (log:debug "Making route for" dependency)

  ;; Each dependency should have an url.
  (let ((url (get-url dependency)))
    ;; And now we will bind a route to dependency and vice-versa.
    (setf (slot-value dependency
                      'route)
          (make-route url
                      dependency))))


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
            (make-route url
                                        dependency)))))


(defgeneric infer-type-from (path-or-url)
  (:documentation "Returns a keyword meaning content type of the dependency
by infering it from URL or a path"))


(defmethod infer-type-from ((path pathname))
  (let* ((type (pathname-type path)))
    
    (if (member type '("css" "js" "png" "jpg" "ico" "gif") :test #'string-equal)
        (intern (string-upcase type)
                :keyword)
        (error "Unable to infer type from ~a" path))))


(defmethod infer-type-from ((url string))
  (let* ((parsed (puri:parse-uri url))
         (url-path (puri:uri-path parsed)))
    (if url-path
        (infer-type-from (pathname url-path))
        (error "Unable to infer type from ~a" url))))



(defun make-dependency (path-or-url &key system
                                      type
                                      integrity
                                      crossorigin)
  "Creates a JavaScript dependency, served from the disk.

If system's name was give, then path is calculated relative
to this system's source root."
  (check-type path-or-url (or string pathname))

  (log:debug "Creating dependency from" path-or-url)

  ;; If a usual string was given, but it is does not look like
  ;; a URL, we'll consider it a path to a file, to make developer's
  ;; life fun and easy.
  (when (and (stringp path-or-url)
             (not (cl-ppcre:scan "^https?://"
                                 path-or-url)))
    (setf path-or-url
          (pathname path-or-url)))
  
  (let ((type (or type
                  (infer-type-from path-or-url))))
    (typecase path-or-url
      (string (make-instance (if *cache-remote-dependencies-in*
                                 'cached-remote-dependency
                                 'remote-dependency)
                             :type type
                             :remote-url path-or-url
                             :integrity integrity
                             :crossorigin crossorigin))
      (pathname (make-instance 'local-dependency
                               :type type
                               :path (if system
                                         (asdf:system-relative-pathname system
                                                                        path-or-url)
                                         path-or-url))))))


(defmethod serve ((dependency local-dependency))
  "Serves local dependency from the disk."
  (values (pathname (get-path dependency))
          (get-content-type dependency)))


(defmethod serve ((dependency cached-remote-dependency))
  "Serves remote dependency from local cache."
  (let ((local-path (pathname (get-path dependency))))
    
    (unless (cl-fad:file-exists-p local-path)
      (let* ((url (get-remote-url dependency))
             ;; игнорируем ошибки пока в самолёте
             (input (handler-case
                        (progn
                          (log:debug "Caching" url)
                          (dex:get url
                                   :want-stream t
                                   :force-binary t))
                      (t ()
                        (log:error "Unable to fetch" url)
                        (values)))))
        
        (ensure-directories-exist local-path)
        
        (when input
          (with-open-file (output local-path
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (cl-fad:copy-stream input output)))))
    
    (values local-path
            (get-content-type dependency))))


(defmethod get-url ((dependency local-dependency))
  "Returns dependency's url and it's type.

URL type is returned as second value and can be :local or :remote.
For local-dependency it is :local."
  
  (let* ((path (get-path dependency))
         (filename (pathname-name path))
         (ext (pathname-type path))
         (prefix (format nil "/static/~a/"
                         ext))
         (filename-with-prefix (concatenate 'string
                                            prefix
                                            filename
                                            "."
                                            ext)))
    filename-with-prefix))


(defmethod get-url ((dependency remote-dependency))
  (get-remote-url dependency))


(defmethod get-url ((dependency cached-remote-dependency))
  (let* ((remote-url (get-remote-url dependency)))
    (concatenate 'string "/remote-deps-cache/"
                 (md5 remote-url))))


(defmethod get-path ((dependency remote-dependency))
  "Returns a path to cached file."
  (check-type *cache-remote-dependencies-in* pathname)
  (assert (cl-fad:directory-pathname-p
           *cache-remote-dependencies-in*))

  (let* ((url (get-remote-url dependency))
         (hash (md5 url)))
    (merge-pathnames hash *cache-remote-dependencies-in*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Routes related part ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: rename to register-routes or may be move this code to push-dependency?
(defun register-dependencies (dependencies)
  "Adds dependencies to the router to make HTTP server handle them."
  (dolist (dependency dependencies)
    (let ((route (get-route dependency)))
      (when route
        (add-route route)))))


(defclass dependency-route (routes:route)
  ((dependency :initform nil
               :initarg :dependency
               :reader get-dependency)))


(defun make-route (uri dependency)
  "Makes a route for dependency.

Automatically adds a prefix depending on current webapp and widget."

  (make-instance 'dependency-route
                 :template (routes:parse-template uri)
                 :dependency dependency))



(defmethod weblocks/routes:serve ((route dependency-route) env)
  (declare (ignorable env))
  
  (let ((dependency (get-dependency route)))
    (multiple-value-bind (content content-type)
        (serve dependency)
              
      (let ((content (typecase content
                       (string (list content))
                       (t content))))
        (list 200
              (list :content-type content-type)
              content)))))


(defmacro with-collected-dependencies (&body body)
  "Use this macro to wrap code which may push new dependencies for
the page or an action."
  `(let (*page-dependencies*)
     ,@body))


(defun push-dependency (dependency)
  "Pushes dependency into the currently collected list of dependencies.

Makes deduplication by comparing dependencies' urls."
  
  (unless (boundp '*page-dependencies*)
    (error "Please, use push-dependency in code, wrapped with with-collected-dependencies macro."))
  (pushnew dependency *page-dependencies*
           :key #'get-url
           :test #'string-equal))


(defun push-dependencies (list-of-dependencies)
  "Same as `push-dependency' but for the list."
  (mapc #'push-dependency
        list-of-dependencies))


(defun get-collected-dependencies ()
  (unless (boundp '*page-dependencies*)
    (error "Please, use push-dependency in code, wrapped with with-collected-dependencies macro."))

  ;; Dependencies returned as reversed list because that way
  ;; they will have same order as they were pushed.
  (reverse *page-dependencies*))
