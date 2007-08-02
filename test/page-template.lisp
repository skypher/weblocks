
(in-package :weblocks-test)

;;; test with-page
(deftest-html with-page-1
    (let ((weblocks::*render-debug-toolbar* nil)
	  (weblocks::*page-public-dependencies* (list "stylesheets/foo.css"
						      "stylesheets/bar.css")))
      (declare (special weblocks::*page-public-dependencies*))
      (with-html
	(:div "test"))
      (weblocks::render-page))
  (htm
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (:html :xmlns "http://www.w3.org/1999/xhtml"
    (:head
     (:title "Weblocks - a Common Lisp web framework")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/layout.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/foo.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/bar.css")
     (:script :src "/pub/scripts/prototype.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/weblocks.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/scriptaculous.js?load=effects,controls" :type "text/javascript" ""))
    (:body
     (:div :class "page-wrapper"
	   (:div :class "page-extra-top-1" "&nbsp;")
	   (:div :class "page-extra-top-2" "&nbsp;")
	   (:div :class "page-extra-top-3" "&nbsp;")
	   (:div "test")
	   (:div :class "page-extra-bottom-1" "&nbsp;")
	   (:div :class "page-extra-bottom-2" "&nbsp;")
	   (:div :class "page-extra-bottom-3" "&nbsp;"))
     (:div :id "ajax-progress" "&nbsp;")))))
