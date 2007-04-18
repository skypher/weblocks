
(in-package :weblocks)

(defun with-page (body-fn)
  "Renders boilerplate XHTML (title, stylesheets, etc.)"
  (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
  (with-html-output (*weblocks-output-stream* nil :prologue t)
    (:html
     (:head
      (:title "Hello!")
      (:link :rel "stylesheet" :type "text/css" :href "pub/main.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/form.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/data.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/table.css"))
     (:body
      (funcall body-fn)))))

