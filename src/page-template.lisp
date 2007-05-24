
(in-package :weblocks)

(export '(render-page-body))

(defun with-page (body-fn)
  "Renders boilerplate XHTML (title, stylesheets, etc.)"
  (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
  (with-html-output (*weblocks-output-stream* nil :prologue t)
    (:html
     (:head
      (:title "Hello!")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/main.css")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/navigation.css")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/form.css")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/data.css")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/table.css")
      (:link :rel "stylesheet" :type "text/css" :href "/pub/layout.css")
      (when *render-debug-toolbar*
	(htm (:link :rel "stylesheet" :type "text/css" :href "/pub/debug-mode.css")))
      (:script :src "/pub/scripts/prototype.js" :type "text/javascript"))
      (:script :src "/pub/scripts/weblocks.js" :type "text/javascript"))
     (:body
      (render-page-body body-fn)
      (when *render-debug-toolbar*
	(render-debug-toolbar))
      (:div :id "ajax-progress" "&nbsp;"))))

(defgeneric render-page-body (body-fn)
  (:documentation "Renders the body of the page (exluding the <body>
tag). The default implementation renders root composite in a wrapper
div along with extra tags. Specialize :before and :after methods to
render extra html prior and post the page wrapper."))

(defmethod render-page-body (body-fn)
  (with-html
    (:div :class "page-wrapper"
	  (with-extra-tags
	    (funcall body-fn)))))
