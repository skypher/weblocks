
(in-package :weblocks)

(export '(handle-http-error handle-error-condition))

(defun error-page-html-wt (&key title heading description content &allow-other-keys)
  (with-html-to-string
    (:html
     (:head
      (:title (str title))
      (:link :rel "stylesheet" :type "text/css" :href "/weblocks-common/pub/stylesheets/error-page.css"))
     (:body                             ; TODO date
      (:h1 (esc heading))
      (when description
        (htm (:h2 "Description")
             (:p (:tt (esc description)))))
      (str content)
      (when (or description content)
        (htm (:hr)))
      (:table :class "footer"
              :width "100%"
              (:tr :valign "center"
                   (:td :width "10"
                    (:img :src "http://40ants.com/img/made-with-lisp.svg"
                          :width "60"))
                   (:td 
                    "This is the " (:a :href "http://weblocks-framework.info/" "Weblocks Application Framework")
                    " running on " (esc (format nil "~A:~A"
                                                (weblocks.request:get-host)
                                                (weblocks.request:get-port))))))))))

(deftemplate :error-page-html-wt 'error-page-html-wt)

(defmacro with-error-page-html ((title heading &optional description) &body body)
  `(render-wt 
     :error-page-html-wt 
     nil 
     :title ,title
     :heading ,heading
     :description ,description
     :content (capture-weblocks-output 
                (with-html ,@body))))

(defmethod handle-http-error ((app weblocks-webapp) code &optional condition)
  (with-error-page-html ((escape-string (format nil "~A ~A" code (reason-phrase code)))
                         (escape-string (format nil "~A ~A" code (reason-phrase code))))))

