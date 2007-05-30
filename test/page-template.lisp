
(in-package :weblocks-test)

;;; test with-page
(deftest-html with-page-1
    (let ((weblocks::*render-debug-toolbar* nil))
      (weblocks::with-page (lambda ()
			     (with-html
			       (:div "test")))))
  (htm
   (str "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>")
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (:html
    (:head
     (:title "Hello!")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/flash.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/navigation.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/form.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/data.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/table.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/layout.css")
     (:script :src "/pub/scripts/prototype.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/weblocks.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/scriptaculous.js?load=effects" :type "text/javascript" ""))
    (:body
     (:div :class "page-wrapper"
	   (:div :class "extra-top-1" "&nbsp;")
	   (:div :class "extra-top-2" "&nbsp;")
	   (:div :class "extra-top-3" "&nbsp;")
	   (:div "test")
	   (:div :class "extra-bottom-1" "&nbsp;")
	   (:div :class "extra-bottom-2" "&nbsp;")
	   (:div :class "extra-bottom-3" "&nbsp;"))
     (:div :id "ajax-progress" "&nbsp;")))))
