
(in-package :weblocks)

(defwebapp weblocks-default 
    :description "A default welcome application for weblocks"
    :init-user-session 'init-user-session
    :prefix "/weblocks-default"
    :autostart nil)

(defmethod render-page-body :after ((app weblocks-default) rendered-html)
  (with-html
    (:div :class "footer"
	  (:p "Running on "
	      (str (concatenate
		    'string (server-type) " " (server-version)))
	      " (" (str (concatenate 'string (lisp-implementation-type) " "
				     (lisp-implementation-version))) ")")
	  (:img :src (make-webapp-public-file-uri "images/footer/valid-xhtml11.png") :alt "This site has valid XHTML 1.1.")
	  (:img :src (make-webapp-public-file-uri "images/footer/valid-css.png") :alt "This site has valid CSS."))))

(defwidget webapp-control ()
  ())

(defmethod render-widget-body ((widget webapp-control) &rest args)
  (declare (ignore args))
  (flet ((remove-classname (cname list)
           (remove-if (lambda (app)
                        (eq (class-name (class-of app))
                            cname))
                      list)))
    (with-html
      (:h3 "Currently running webapps:")
      (render-list (remove-classname 'weblocks-default *active-webapps*)
                   :render-fn (lambda (app)
                                (with-html
                                  (:a :href (make-webapp-uri "/" app)
                                      (esc (format nil "~A"
                                                   (class-name (class-of app))))))))
      (:h3 "Registered webapps:")
      (render-list (remove-if (curry #'eq 'weblocks-webapp) *registered-webapps*)
                   :render-fn (lambda (appname)
                                (with-html
                                  (esc (format nil "~A" appname))
                                  " "
                                  (if (find appname *active-webapps*
                                            :key (compose #'class-name #'class-of))
                                    (render-link (f_% (stop-webapp appname)
                                                      (mark-dirty widget))
                                                 "Stop")
                                    (render-link (f_% (start-webapp appname)
                                                      (mark-dirty widget)) "Start"))))))))


(defun init-user-session (root)
  (setf (widget-children root)
        (mapcar #'make-widget
                (list
                  (f_%
                    (with-html
                      (:div :class "header"
                            (with-extra-tags))
                      (:h1 "Welcome to " (:em "Weblocks!"))
                      (:p "To learn more on how to get started
                          writing " (:em "Weblocks") " applications, please see the "
                          (:a :href "http://trac.common-lisp.net/cl-weblocks/wiki/UserManual" "user
                              manual.")
                          (:br) "For general information
                          about " (:em "Weblocks") ", information on how to get
                          support, find documentation, etc. please
                          start " (:a :href "http://common-lisp.net/project/cl-weblocks" "here") ".")
                          (:h2 "How did I get here?")
                          (:p "If you expected to see your application here you probably didn't supply
                              the PREFIX keyword argument to DEFWEBAPP (try " (:code "PREFIX \"/\"") ").")))
                  (make-instance 'webapp-control)
                  (f_% (with-html (:h3 (:em "Happy hacking!"))))))))

