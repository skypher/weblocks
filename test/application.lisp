
(in-package :weblocks-test)

(defwebapp hello-webapp
  :description "foo bar")

;;; test defwebapp
(deftest defwebapp-1
    (with-webapp (:class-name 'hello-webapp)
      (values (webapp-name)
	      (mapcar (curry #'format nil "~A")
		      (mapcar #'dependency-url (webapp-application-dependencies)))
	      (webapp-description)))
  "hello-webapp"
  ("/pub/stylesheets/layout.css"
   "/pub/stylesheets/main.css"
   "/pub/stylesheets/dialog.css"
   "/pub/scripts/prototype.js"
   "/pub/scripts/scriptaculous.js"
   "/pub/scripts/shortcut.js"
   "/pub/scripts/weblocks.js"
   "/pub/scripts/dialog.js")
  "foo bar")
