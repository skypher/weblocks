
(in-package :weblocks-test)

(defwebapp hello-webapp)

;;; test defwebapp
(addtest defwebapp-simple
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


(defwebapp hello2-webapp
           :prefix "/")

(addtest defwebapp-prefix
  (dolist (app '(hello2-webapp))
    (with-webapp (:class-name app)
      (ensure-same (webapp-prefix) "/")
      (ensure-same (make-webapp-uri "foo") "/foo"))))


(defwebapp hello3-webapp
           :prefix "/foo")
(defwebapp hello4-webapp
           :prefix "/foo/")

(addtest defwebapp-prefix-2
  (dolist (app '(hello3-webapp hello4-webapp))
    (with-webapp (:class-name app)
      (ensure-same (webapp-prefix) "/foo")
      (ensure-same (make-webapp-uri "bar") "/foo/bar"))))


(defwebapp hello5-webapp
           :prefix "/"
           :public-files-uri-prefix "/pub") 
(defwebapp hello6-webapp
           :prefix "/"
           :public-files-uri-prefix "/pub/") 

(addtest defwebapp-pub-prefix
  (dolist (app '(hello5-webapp hello6-webapp))
    (with-webapp (:class-name app)
      (ensure-same (webapp-public-files-uri-prefix) "/pub")
      (ensure-same (make-webapp-public-file-uri "foo.css") "/pub/foo.css"))))


(defwebapp hello7-webapp
           :prefix "/foo"
           :public-files-uri-prefix "/pub")

(addtest defwebapp-pub-prefix-2
  (with-webapp (:class-name 'hello7-webapp)
    (ensure-same (webapp-public-files-uri-prefix) "/foo/pub")
    (ensure-same (make-webapp-public-file-uri "foo.css") "/foo/pub/foo.css")))

