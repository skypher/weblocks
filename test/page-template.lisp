
(in-package :weblocks-test)



;;; test page-title
(deftest page-title-1
    (with-webapp (:description "webapp-desc" :name "Webapp Name")
      (let ((*current-page-description* "current-page-desc"))
	(declare (special *current-page-description*))
	(page-title (weblocks::current-webapp))))
  "Webapp Name - current-page-desc")

(deftest page-title-2
    (with-webapp (:name "Webapp Name" :description "webapp-desc")
      (let ((*current-page-description* nil))
	(declare (special *current-page-description*))
	(page-title (weblocks::current-webapp))))
  "Webapp Name - webapp-desc")

(deftest page-title-3
    (with-webapp (:name "Webapp Name")
      (let ((*current-page-description* nil))
	(declare (special *current-page-description*))
	(page-title (weblocks::current-webapp))))
  "Webapp Name")

;;; test with-page
(deftest-html with-page-1
    (let ((weblocks::*render-debug-toolbar* nil)
	  (weblocks::*page-dependencies* (mapcar (curry #'apply
							(curry-after #'make-local-dependency :do-not-probe t))
						 '((:stylesheet "foo")
						   (:stylesheet "bar"))))
	  (weblocks::*webapp-name* 'some-name)
	  (*current-page-description* "Some Page"))
      (declare (special weblocks::*page-dependencies*
			*current-page-description* weblocks::*webapp-name*))
      (with-html
	(:div "test"))
      (weblocks::render-page))
  (htm
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (fmt "~%")
   (:html :xmlns "http://www.w3.org/1999/xhtml"
    (:head
     (:meta :http-equiv "Content-type" :content "text/html; charset=utf-8")
     (:title "Some Name - Some Page")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/layout.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/dialog.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/foo.css")
     (:link :rel "stylesheet" :type "text/css" :href "/pub/stylesheets/bar.css")
     (:script :src "/pub/scripts/prototype.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/scriptaculous.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/shortcut.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/weblocks.js" :type "text/javascript" "")
     (:script :src "/pub/scripts/dialog.js" :type "text/javascript" ""))
    (:body
     (:div :class "page-wrapper"
	   (:div :class "page-extra-top-1" "<!-- empty -->")
	   (:div :class "page-extra-top-2" "<!-- empty -->")
	   (:div :class "page-extra-top-3" "<!-- empty -->")
	   (:div "test")
	   (:div :class "page-extra-bottom-1" "<!-- empty -->")
	   (:div :class "page-extra-bottom-2" "<!-- empty -->")
	   (:div :class "page-extra-bottom-3" "<!-- empty -->"))
     (:div :id "ajax-progress" "&nbsp;")))))
