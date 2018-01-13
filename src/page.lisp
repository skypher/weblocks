(defpackage #:weblocks.page
  (:use #:cl)
  (:export
   #:render
   #:render-body
   #:render-dependencies
   #:render-headers
   #:get-title
   #:get-description
   #:get-keywords
   #:get-language
   #:with-layout))
(in-package weblocks.page)


(defvar *title* nil)
(defvar *description* nil)
(defvar *keywords* nil)
(defvar *language* "en")


(defmacro def-get-set (variable)
  "Generates a function get-<variable> and a corresponding setf part."
  (let ((func-name (alexandria:symbolicate :get- variable))
        (var-name (alexandria:symbolicate :* variable :*)))
    `(progn
       (defun ,func-name ()
         ,var-name)

       (defun (setf ,func-name) (value)
         (setf ,var-name value)))))


(def-get-set title)
(def-get-set description)
(def-get-set keywords)
(def-get-set language)


(defmethod render-headers ((app weblocks:weblocks-webapp))
  "A placeholder to add :meta entries, :expires headers and other 
   header content on a per-webapp basis.  For example, using a dynamic 
   hook on rendering you can bind special variables that are dereferenced 
   here to customize header rendering on a per-request basis.  By default
   this function renders the current content type."
  (spinneret:with-html
    (:meta :http-equiv "Content-type"
           :content weblocks::*default-content-type*)
    
    (when (get-description)
      (:meta :name "description"
             :content (get-description)))
    (when (get-keywords)
      (:meta :name "keywords"
             :content (format nil "~{~A~^,~}" (get-keywords))))

    ;; Headers aren't tags but any code snippets or functions
    ;; returning strings.
    ;; (dolist (header *current-page-headers*)
    ;;   (etypecase header
    ;;     (string (htm (str header)))
    ;;     ;; TODO: Right now (old code) expects that function will write
    ;;     ;; to special stream, but we need to make it return a value
    ;;     ;; to be consistent with string headers.
    ;;     ((or function symbol) (funcall header))))
    ))


(defmethod render-body ((app weblocks:weblocks-webapp) body-string)
  "Default page-body rendering method"
  
  (spinneret:with-html
    (:raw body-string)))


(defmethod render-dependencies ((app weblocks:weblocks-webapp) dependencies)
  (etypecase dependencies
    (list (mapc #'weblocks.dependencies:render-in-head
                 dependencies))
    (string (spinneret:with-html
              (:raw dependencies)))))


(defmethod render ((app weblocks:weblocks-webapp)
                   inner-html
                   &key (dependencies (weblocks.dependencies:get-dependencies
                                       app)))
  "Default page rendering template and protocol."
  (log:debug "Rendering page for" app)

  (weblocks.dependencies:register-dependencies dependencies)

  (let ((weblocks.html:*lang* (get-language)))
    (weblocks.html:with-html
      (:doctype)
      (:html
       (:head
        (:title (get-title))

        ;; It is XXI century, you know?
        ;; All sites should be optimized for mobile screens
        (:meta :name "viewport"
               :content "width=device-width, initial-scale=1")
              
        (render-headers app)
        (render-dependencies app dependencies))
       (:body
        (render-body app inner-html)
        ;; (:script :type "text/javascript"
        ;;          "updateWidgetStateFromHash();")
        )))))


(defmethod render-page-with-widgets ((app weblocks:weblocks-webapp))
  "Renders a full HTML by collecting header elements, dependencies and inner
   HTML and inserting them into the `render' method."
  (log:debug "Special Rendering page for" app)

  ;; At the moment when this method is called, there is already
  ;; rendered page's content in the weblocks.html::*stream*.
  ;; All we need to do now – is to render dependencies in the header
  ;; and paste content into the body.
  (let* ((rendered-html (weblocks.html:get-rendered-chunk))
         (all-dependencies (weblocks.dependencies:get-collected-dependencies)))

    (render app rendered-html :dependencies all-dependencies)))

