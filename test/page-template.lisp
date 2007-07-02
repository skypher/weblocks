
(in-package :weblocks-test)

;;; test with-page
(deftest-html with-page-1
    (let ((weblocks::*render-debug-toolbar* nil))
      (weblocks::with-page (lambda ()
			     (with-html
			       (:div "test")))))
  (htm
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (:html :xmlns "http://www.w3.org/1999/xhtml"
    (:head
     (:title "Hello!")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/layout.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/flash.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/navigation.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/form.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/suggest.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/isearch.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/data.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/table.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/datagrid.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/gridedit.css")
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
