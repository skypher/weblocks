(defpackage #:weblocks.default.init
  (:use #:cl))
(in-package weblocks.default.init)


(defmethod weblocks.session:init ((app t))
  (let ((quickstart-url "http://40ants.com/weblocks/quickstart.html"))
    (weblocks.widgets.string-widget:make-string-widget
     (weblocks.html:with-html-string
       (:h1 "No weblocks.session:init method defined.")
       (:p "Please define a method weblocks.session:init to initialize a session.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         "(defmethod weblocks.session:init ((app your-app-class))
            \"Hello world!\")"))

       (:p ("Read more in [documentaion]()."
            quickstart-url)))
     :escape-p nil)))
