
(in-package :weblocks-test)

(deftestsuite .application-suite (weblocks-suite)
  ())

(defwebapp hello-webapp
    :bundle-dependency-types nil
    :js-backend :prototype
    :version-dependency-types nil
    :gzip-dependency-types nil)

;;; test defwebapp
(addtest defwebapp-simple
  (with-test-webapp (:class-name 'hello-webapp)
    (ensure-same (webapp-name) "hello-webapp" :test string-equal)
    (ensure-same (mapcar #'dependency-url (webapp-application-dependencies))
                 '(#U"/hello-webapp/pub/stylesheets/layout.css"
                   #U"/hello-webapp/pub/stylesheets/main.css"
                   #U"/hello-webapp/pub/stylesheets/dialog.css"
                   #U"/hello-webapp/pub/scripts/prototype-backend/prototype.js"
                   #U"/hello-webapp/pub/scripts/prototype-backend/scriptaculous.js"
                   #U"/hello-webapp/pub/scripts/prototype-backend/weblocks.js"
                   #U"/hello-webapp/pub/scripts/prototype-backend/dialog.js")
                 :test set-equal-uri=)
    (ensure-null (webapp-description))))


;;; webapp session values
(addtest session-value
  (with-test-webapp ()
    (setf (webapp-session-value 'foo) 'bar)
    (ensure-same (webapp-session-value 'foo) 'bar)))


;;; uri prefix
(defwebapp hello2-webapp
           :js-backend :prototype
           :prefix "/")

(addtest defwebapp-prefix
  (dolist (app '(hello2-webapp))
    (with-test-webapp (:class-name app)
      (ensure-same (webapp-prefix) "/")
      (ensure-same (make-webapp-uri "foo") "/foo"))))


(defwebapp hello3-webapp
           :js-backend :prototype
           :prefix "/foo")
(defwebapp hello4-webapp
           :js-backend :prototype
           :prefix "/foo/")

(addtest defwebapp-prefix-2
  (dolist (app '(hello3-webapp hello4-webapp))
    (with-test-webapp (:class-name app)
      (ensure-same (webapp-prefix) "/foo")
      (ensure-same (make-webapp-uri "bar") "/foo/bar"))))


;;; public files' uri prefix
(defwebapp hello5-webapp
           :js-backend :prototype
           :prefix "/"
           :public-files-uri-prefix "/pub") 
(defwebapp hello6-webapp
           :prefix "/"
           :js-backend :prototype
           :public-files-uri-prefix "/pub/") 

(addtest defwebapp-pub-prefix
  (dolist (app '(hello5-webapp hello6-webapp))
    (with-test-webapp (:class-name app)
      (ensure-same (webapp-public-files-uri-prefix) "/pub")
      (ensure-same (make-webapp-public-file-uri "foo.css") "/pub/foo.css"))))


(defwebapp hello7-webapp
           :prefix "/foo"
           :js-backend :prototype
           :public-files-uri-prefix "/pub")

(addtest defwebapp-pub-prefix-2
  (with-test-webapp (:class-name 'hello7-webapp)
    (ensure-same (webapp-prefix) "/foo")
    (ensure-same (webapp-public-files-uri-prefix) "/pub")
    (ensure-same (compute-webapp-public-files-uri-prefix
                  (weblocks::current-webapp)) "/foo/pub")
    (ensure-same (make-webapp-public-file-uri "foo.css") "/foo/pub/foo.css")))


;;; public files' path
(defwebapp hello8-webapp
           :js-backend :prototype
           :public-files-path "./pub") 
(defwebapp hello9-webapp
           :js-backend :prototype
           :public-files-path "./pub/") 
(defwebapp hello10-webapp
           :js-backend :prototype
           :public-files-path #P"pub")

(addtest defwebapp-pub-path
  (dolist (app '(hello8-webapp hello9-webapp))
    (with-test-webapp (:class-name app)
      (ensure-same (webapp-public-files-path) "./pub/")))
  (with-test-webapp (:class-name 'hello10-webapp)
    (ensure-same (webapp-public-files-path) #P"pub/")))

;;; interaction of hostname and prefix dispatching
(defwebapp host-1
           :js-backend :prototype
           :hostnames '("foo.com")
           :prefix "/foo")

(defwebapp host-2
           :js-backend :prototype
           :hostnames '("foo.com")
           :prefix "/")

(defwebapp host-3
           :js-backend :prototype
           :hostnames nil
           :prefix "/foo")

(defwebapp host-4
           :js-backend :prototype
           :hostnames nil
           :prefix "/")

(addtest webapp-order
  (let ((host-1 (make-instance 'host-1))
        (host-2 (make-instance 'host-2))
        (host-3 (make-instance 'host-3))
        (host-4 (make-instance 'host-4)))
  (ensure-same (list host-1 host-2 host-3 host-4)
               (weblocks::sort-webapps (list host-4 host-2 host-1 host-3)))))

