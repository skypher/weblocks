
(in-package :weblocks)

(export '(render-page-body page-title))

(defun page-title ()
  "Generates a page title from the application name, application
description, and current navigation state."
  (declare (special *current-page-description*))
  (apply #'concatenate 'string (humanize-name *webapp-name*)
	 (cond
	   (*current-page-description* (list " - " *current-page-description*))
	   (*webapp-description* (list " - " *webapp-description*)))))

(defun render-page ()
  "Takes the widget and application dependency information and wraps
the HTML already rendered to *weblocks-output-stream* with boilerplate
page HTML (title, stylesheets, etc.)."
  ; Note, anything that precedes the doctype puts IE6 in quirks mode
  ; (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
  (declare (special *page-dependencies* *application-dependencies*))
  (let ((rendered-html (get-output-stream-string *weblocks-output-stream*))
	(all-dependencies (compact-dependencies (append *application-dependencies*
							*page-dependencies*))))
    (with-html-output (*weblocks-output-stream* nil :prologue t)
      (:html :xmlns "http://www.w3.org/1999/xhtml"
	     (:head
	      (:meta :http-equiv "Content-type" :content *default-content-type*)
	      (:title (str (page-title)))
	      (mapc #'render-dependency-in-page-head all-dependencies))
	     (:body
	      (render-page-body rendered-html)
	      (when *render-debug-toolbar*
		(render-debug-toolbar))
	      (:div :id "ajax-progress" "&nbsp;"))))))

(defgeneric render-page-body (rendered-html)
  (:documentation "Renders the body of the page (exluding the <body>
tag). The default implementation wraps the already rendered HTML in a
wrapper div along with extra tags. Specialize :before and :after
methods to render extra html prior and post the page wrapper."))

(defmethod render-page-body (body-fn)
  (with-html
    (:div :class "page-wrapper"
	  (render-extra-tags "page-extra-top-" 3)
	  (htm (str body-fn))
	  (render-extra-tags "page-extra-bottom-" 3))))
