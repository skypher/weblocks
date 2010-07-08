
(in-package :weblocks)

(export '(render-page
          render-page-body
          render-page-headers
          application-page-title
          application-meta-description
          application-page-keywords
	  *current-page-description*
          *current-page-keywords*
          *accumulate-page-keywords*))

(defvar *page-dependencies*)
(setf (documentation '*page-dependencies* 'variable)
      "A list of dependencies of the currently rendered page.")

(defvar *current-page-description*)
(setf (documentation '*current-page-description* 'variable)
      "Description of the currently rendered page; used for default
      page title and meta description.")

(defvar *current-page-keywords*)
(setf (documentation '*current-page-keywords* 'variable)
      "Keywords for the currently rendered page; used for default
      page title.")

(defvar *accumulate-page-keywords* t
  "Whether to accumulate widgets' keywords for the keywords
  meta tag or use the most specific keywords only.")

;;
;; Compute the webapp page title, meta description and meta keywords
;;

(defmethod application-page-title ((app weblocks-webapp))
  "The default page-title method generates a page title from the 
   application name, application description, and current navigation state."
  (let ((webapp-description (webapp-description)))
    (apply #'format nil "~A~A~A"
	   (webapp-name)
	   (cond
	     (*current-page-description* (list " - " *current-page-description*))
	     (webapp-description (list " - " webapp-description))
	     (t '("" ""))))))

(defmethod application-meta-description ((app weblocks-webapp))
  (application-page-title app))

(defmethod application-page-keywords ((app weblocks-webapp))
  *current-page-keywords*)

;;
;; Render the current page
;;

(defgeneric render-page (app)
  (:documentation   "Takes the widget and application dependency information and wraps
the HTML already rendered to *weblocks-output-stream* with boilerplate
page HTML (title, stylesheets, etc.).  Can be overridden by subclasses"))

(defmethod render-page ((app weblocks-webapp))
  "Default page rendering template and protocol"
  ; Note, anything that precedes the doctype puts IE6 in quirks mode
  ; (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
  (declare (special *page-dependencies*))
  (let ((rendered-html (get-output-stream-string *weblocks-output-stream*))
	(all-dependencies (timing "compact-dependencies"
                            (compact-dependencies (append (webapp-application-dependencies)
                                                          *page-dependencies*
                                                          (when (weblocks-webapp-debug app)
                                                            (build-local-dependencies
                                                              '((:script "weblocks-debug")
                                                                (:stylesheet "debug-toolbar")))))))))
    (with-html-output (*weblocks-output-stream* nil :prologue t)
      (:html :xmlns "http://www.w3.org/1999/xhtml"
	     (:head
	      (:title (str (application-page-title app)))
	      (render-page-headers app)
	      (mapc #'render-dependency-in-page-head all-dependencies))
	     (:body
	      (render-page-body app rendered-html)
	      (when (weblocks-webapp-debug app)
		(render-debug-toolbar))
	      (:div :id "ajax-progress" "&nbsp;"))))))

;;
;; Render header entries
;;

(defmethod render-page-headers ((app weblocks-webapp))
  "A placeholder to add :meta entries, :expires headers and other 
   header content on a per-webapp basis.  For example, using a dynamic 
   hook on rendering you can bind special variables that are dereferenced 
   here to customize header rendering on a per-request basis.  By default
   this function renders the current content type."
  (with-html 
    (:meta :http-equiv "Content-type" :content *default-content-type*)
    (awhen (application-meta-description app)
      (htm (:meta :name "description" :value it)))
    (awhen (application-page-keywords app)
      (htm (:meta :name "keywords" :value (format nil "~{~A~^,~}" it))))))

;;
;; Render the page body
;;

(defgeneric render-page-body (app rendered-html)
  (:documentation "Renders the body of the page (exluding the <body>
tag). The default implementation wraps the already rendered HTML in a
wrapper div along with extra tags. Specialize :before and :after
methods to render extra html prior and post the page wrapper and override
the main method to replace the generation of the page wrapper and any contents
inside it but outside the rendering of the root widget"))

(defmethod render-page-body ((app weblocks-webapp) body-string)
  "Default page-body rendering method"
  (with-html
    (:div :class "page-wrapper"
	  (render-extra-tags "page-extra-top-" 3)
	  (htm (str body-string))
	  (render-extra-tags "page-extra-bottom-" 3))))
