
(in-package :weblocks)

(export '(render-page-body))

(defun render-page ()
  "Takes the widget and application dependency information and wraps
the HTML already rendered to *weblocks-output-stream* with boilerplate
page HTML (title, stylesheets, etc.)."
  ; Note, anything that precedes the doctype puts IE6 in quirks mode
  ; (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>")
  (declare (special *page-public-dependencies*))
  (let ((rendered-html (get-output-stream-string *weblocks-output-stream*)))
    (with-html-output (*weblocks-output-stream* nil :prologue t)
      (:html :xmlns "http://www.w3.org/1999/xhtml"
	     (:head
	      (:title "Weblocks - a Common Lisp web framework")
	      ; render stylesheets
	      (loop for i in (remove-duplicates (append *application-public-dependencies*
							*page-public-dependencies*)
						:test #'equalp :from-end t)
		 when (equalp (pathname-type i) "css")
		 do (htm (:link :rel "stylesheet" :type "text/css"
				:href (merge-pathnames i (make-pathname :directory
									'(:absolute "pub"))))))
	      (when *render-debug-toolbar*
		(htm (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/debug-mode.css")))
	      ; render scripts
	      ; empty quote in script tags are a fix for w3m
	      (loop for i in *page-public-dependencies*
		 when (equalp (pathname-type i) "js")
		 do (htm (:script :type "text/javascript"
				  :src (merge-pathnames i (make-pathname :directory '(:absolute "pub")))
				  "")))
	      (:script :src "/pub/scripts/prototype.js" :type "text/javascript" "")
	      (:script :src "/pub/scripts/weblocks.js" :type "text/javascript" "")
	      (:script :src "/pub/scripts/scriptaculous.js?load=effects,controls" :type "text/javascript" ""))
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
