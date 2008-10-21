
(in-package :weblocks-test)

(deftestsuite .application-suite (weblocks-suite)
  ())

(defwebapp hello-webapp)

;;; test defwebapp
(addtest defwebapp-simple
  (with-webapp (:class-name 'hello-webapp)
    (ensure-same (webapp-name) "hello-webapp" :test string-equal)
    (ensure-same (mapcar #'dependency-url (webapp-application-dependencies))
		 '(#U"/hello-webapp/pub/stylesheets/layout.css"
		   #U"/hello-webapp/pub/stylesheets/main.css"
		   #U"/hello-webapp/pub/stylesheets/dialog.css"
		   #U"/hello-webapp/pub/scripts/prototype.js"
		   #U"/hello-webapp/pub/scripts/scriptaculous.js"
		   #U"/hello-webapp/pub/scripts/shortcut.js"
		   #U"/hello-webapp/pub/scripts/weblocks.js"
		   #U"/hello-webapp/pub/scripts/dialog.js")
		 :test set-equal-uri=)
    (ensure-null (webapp-description))))


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
    (ensure-same (webapp-prefix) "/foo")
    (ensure-same (webapp-public-files-uri-prefix) "/pub")
    (ensure-same (compute-webapp-public-files-uri-prefix
		  (weblocks::current-webapp)) "/foo/pub")
    (ensure-same (make-webapp-public-file-uri "foo.css") "/foo/pub/foo.css")))

(addtest session-value
  (with-webapp ()
    (setf (webapp-session-value 'foo) 'bar)
    (ensure-same (webapp-session-value 'foo) 'bar)))

