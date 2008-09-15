
(in-package :weblocks-test)

(defwebapp hello-webapp
  :description "foo bar")

;;; test defwebapp
(addtest defwebapp-1
  (with-webapp (:class-name 'hello-webapp)
    (ensure-same (webapp-name) "hello-webapp" :test string-equal)
    (ensure-same (mapcar (curry #'format nil "~A")
			 (mapcar #'dependency-url (webapp-application-dependencies)))
		 '("/pub/stylesheets/layout.css"
		   "/pub/stylesheets/main.css"
		   "/pub/stylesheets/dialog.css"
		   "/pub/scripts/prototype.js"
		   "/pub/scripts/scriptaculous.js"
		   "/pub/scripts/shortcut.js"
		   "/pub/scripts/weblocks.js"
		   "/pub/scripts/dialog.js")
		 :test set-equal-equal)
    (ensure-same (webapp-description) "foo bar")))
