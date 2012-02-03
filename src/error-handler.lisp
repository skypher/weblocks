
(in-package :weblocks)

(export '(handle-http-error handle-error-condition print-trivial-backtrace))

(defmacro with-error-page-html ((title heading &optional description) &body body)
  `(with-html-to-string
    (:html
      (:head
        (:title (str ,title))
        (:link :rel "stylesheet" :type "text/css" :href "/weblocks-common/pub/stylesheets/error-page.css"))
      (:body ; TODO date
        (:h1 (:img :src "/weblocks-common/pub/images/weblocks-alien-small.png")
             (str ,heading))
        ,@(when description
           (append '(:h2 "Description") `(:p (:tt (str ,description)))))
        ,@body
        ,(when (or description body)
           '(:hr))
        (:div :class "footer"
            "This is the " (:a :href "http://weblocks.viridian-project.de/" "Weblocks Application Framework")
            " running on " (str (hunchentoot::address-string)))))))

(defmethod handle-http-error ((app weblocks-webapp) code &optional condition)
  (with-error-page-html ((escape-string (format nil "~A ~A" code (reason-phrase code)))
                         (escape-string (format nil "~A ~A" code (reason-phrase code))))))


;;; 500 errors deserve special attention
(defun print-trivial-backtrace (c)
  (trivial-backtrace:print-backtrace c :output nil))

(defmethod handle-error-condition ((app weblocks-webapp) c)
  "Print a pretty platform-specific backtrace if possible;
otherwise just call TRIVIAL-BACKTRACE to get a basic stack report."
  ;; TODO: active webapp, link to control center
  (setf (return-code*) +http-internal-server-error+)
  (with-error-page-html ("500 Weblocks Error" "Weblocks caught an error"
                         (escape-string (format nil "~A: ~A" (type-of c) c)))
        (:h2 "Actions") ; FIXME: this should be a bar at the top of the page
        (:ul
          (:li (render-link (f_% (reset-webapp-session)) "Reset this web application's session")))
        (:h2 "Session data")
        (:p "TODO")
        (:h2 "Restarts")
        (:p "TODO")
        (:h2 "Backtrace")
        #-sbcl
        (:pre (esc (format nil "~A" (print-trivial-backtrace c))))
        #+sbcl
        (let ((frames (sb-debug:backtrace-as-list))
              (*print-circle* t))
          (htm
            (:table
              (:thead
                (:tr
                  (:th "") (:th "Function") (:th "Arguments")))
              (:tbody
                (loop for frame in frames
                      for i from (length frames) downto 0
                      for parity = (if (oddp i) "odd" "even")
                      do (htm
                           (:tr :class parity
                             (:td :class "frame-number" (esc (format nil "~D" i)))
                             (:td :class "frame-call" (:code (esc (format nil "~A" (car frame)))))
                             (:td :class "frame-args"
                               (:ol
                                 (dolist (arg (cdr frame))
                                   (htm (:li (:code (esc (format nil "~A" arg)))))))))))))))))

