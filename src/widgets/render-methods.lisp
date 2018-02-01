(defpackage #:weblocks/widgets/render-methods
  (:use #:cl)
  (:import-from #:weblocks/dependencies
                #:get-collected-dependencies
                #:get-dependencies
                #:push-dependencies
                #:render-in-ajax-response)
  (:import-from #:weblocks/widgets/base
                #:get-css-classes-as-string
                #:get-html-tag
                #:render)
  (:import-from #:weblocks/request
                #:ajax-request-p)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/widgets/dom
                #:dom-id))
(in-package weblocks/widgets/render-methods)


(defmethod render (widget)
  "By default, widget rendered with a text, suggesting to define a rendering method."
  (let ((class-name (class-name (class-of widget))))
    (with-html
      (:p "Please, define:"
          (:pre (format nil
                        "(defmethod weblocks/widget:render ((widget ~a))
    (weblocks/html:with-html
        (:p \"My ~a widget\")))"
                        class-name
                        class-name))))))


(defmethod render :around (widget)
  "This function is intended for internal usage only.
   It renders widget with surrounding HTML tag and attributes."
  (log:debug "Rendering widget" widget "with" (get-collected-dependencies))
  
  (let ((widget-dependencies (get-dependencies widget)))
    ;; Update new-style dependencies
    (push-dependencies widget-dependencies)
    
    (when (ajax-request-p)
      ;; Generate code to embed new dependencies into the page on the fly
      (mapc #'render-in-ajax-response
            widget-dependencies)))
  
  (with-html
    (:tag
     :name (get-html-tag widget)
     :class (get-css-classes-as-string widget)
     :id (dom-id widget)
     (call-next-method))))


