
(in-package :weblocks-test)

;;; test with-page
(deftest-html with-page-1
    (let ((weblocks::*render-debug-toolbar* nil))
      (weblocks::with-page (lambda ()
			     (with-html
			       (:div "test")))))
  (htm
   (str "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (:html
    (:head
     (:title "Hello!")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/navigation.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/form.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/data.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/table.css"))
    (:body
     (:div "test")))))
