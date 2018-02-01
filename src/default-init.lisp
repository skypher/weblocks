(defpackage #:weblocks/default-init
  (:use #:cl)
  (:import-from #:weblocks/session
                #:init)
  (:import-from #:weblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:weblocks/html
                #:with-html-string)
  (:import-from #:weblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:weblocks/app
                #:*current-app*))
(in-package weblocks/default-init)


(defmethod init ((app t))
  (let ((quickstart-url "http://40ants.com/weblocks/quickstart.html"))
    (make-string-widget
     (with-html-string
       (:h1 "No weblocks/session:init method defined.")
       (:p "Please define a method weblocks.session:init to initialize a session.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         ("(defmethod weblocks/session:init ((app ~A))  
            \"Hello world!\")" (string-downcase
                                (type-of *current-app*)))))

       (:p ("Read more in [documentaion]()."
            quickstart-url)))
     :escape-p nil)))


(defmethod init :around ((app t))
  "If init function returned not object inherited from widget, it calls
   create-widget-from method, to transform value into the widget."

  (let ((root (call-next-method)))
    (create-widget-from root)))
