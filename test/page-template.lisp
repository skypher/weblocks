
(in-package :weblocks-test)


(defwebapp some-name
    :js-backend :prototype
    :bundle-dependency-types nil
    :version-dependency-types nil
    :gzip-dependency-types nil)

;;; test with-page
(deftest-html with-page-1
    (with-test-webapp (:class-name 'some-name)
      (let ((weblocks::*page-dependencies*
             (mapcar (curry #'apply
                            (curry-after #'make-local-dependency :do-not-probe t))
                     '((:stylesheet "foo")
                       (:stylesheet "bar"))))
            (*current-page-description* "Some Page")
      (*current-page-title*  "Some Page")
      (*current-page-keywords* nil)
      (*current-page-headers* nil))
        (declare (special weblocks::*page-dependencies*
                          *current-page-description* *current-page-title* *current-page-keywords* *current-page-headers*))
        (with-html
          (:div "test"))
        (weblocks::render-page (weblocks::current-webapp))))
  (htm
   (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ")
   (str "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
   (fmt "~%")
   (:html :xmlns "http://www.w3.org/1999/xhtml"
    (:head
     (:title "some-name - Some Page")
     (:meta :http-equiv "Content-type" :content "text/html; charset=utf-8")
     (:meta :name "description" :content "Some Page")
     (:link :rel "stylesheet" :type "text/css" :href "/some-name/pub/stylesheets/layout.css")
     (:link :rel "stylesheet" :type "text/css" :href "/some-name/pub/stylesheets/main.css")
     (:link :rel "stylesheet" :type "text/css" :href "/some-name/pub/stylesheets/dialog.css")
     (:link :rel "stylesheet" :type "text/css" :href "/some-name/pub/stylesheets/foo.css")
     (:link :rel "stylesheet" :type "text/css" :href "/some-name/pub/stylesheets/bar.css")
     (:script :src "/some-name/pub/scripts/prototype-backend/prototype.js" :type "text/javascript" "")
     (:script :src "/some-name/pub/scripts/prototype-backend/scriptaculous.js" :type "text/javascript" "")
     (:script :src "/some-name/pub/scripts/prototype-backend/weblocks.js" :type "text/javascript" "")
     (:script :src "/some-name/pub/scripts/prototype-backend/dialog.js" :type "text/javascript" ""))
    (:body
     (:div :class "page-wrapper"
           (:div :class "page-extra-top-1" "<!-- empty -->")
           (:div :class "page-extra-top-2" "<!-- empty -->")
           (:div :class "page-extra-top-3" "<!-- empty -->")
           (:div "test")
           (:div :class "page-extra-bottom-1" "<!-- empty -->")
           (:div :class "page-extra-bottom-2" "<!-- empty -->")
           (:div :class "page-extra-bottom-3" "<!-- empty -->"))
     (:div :id "ajax-progress" "&nbsp;")
     (:script :type "text/javascript"
      (fmt "~%// <![CDATA[~%")
      (fmt "updateWidgetStateFromHash();")
      (fmt "~%// ]]>~%"))))))
