
(in-package :weblocks)

(setf hunchentoot:*handle-http-errors-p* nil)

(defun print-trivial-backtrace (c)
  (if (not (nth-value 1 (ignore-errors (asdf:oos 'asdf:load-op :trivial-backtrace))))
    (funcall (find-symbol "PRINT-BACKTRACE" :trivial-backtrace) c :output nil)
    "Please install TRIVIAL-BACKTRACE to get a simple backtrace on your platform."))

(defun handle-error-condition (c)
  "Print a pretty platform-specific backtrace if possible;
otherwise just call TRIVIAL-BACKTRACE to get a basic stack report."
  ;; TODO: active webapp, link to control center
  (with-html-to-string
    (:html
      (:head
        (:title "500 Weblocks Error")
        (:link :rel "stylesheet" :type "text/css" :href "/weblocks-common/pub/stylesheets/error-page.css"))
      (:body ; TODO date
        (:h1 (:img :src "/weblocks-common/pub/images/weblocks-alien-small.png")
             "Weblocks caught an error")
        (:h2 "Description")
        (:p (:tt (esc (format nil "~A: ~A" (type-of c) c))))
        (:h2 "Actions") ; FIXME: this should be a bar at the top of the page
        (:ul
          (:li (render-link (f_% (reset-webapp-session)) "Reset this web application's session")))
        (:h2 "Session data")
        (:p "TODO")
        (:h2 "Backtrace")
        #-sbcl
        (:pre (esc (format nil "~A" (print-trivial-backtrace c))))
        #+sbcl
        (let ((frames (sb-debug:backtrace-as-list)))
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
                                   (htm (:li (:code (esc (format nil "~A" arg)))))))))))))))
        (:hr)
        (:div :class "footer"
            "This is the " (:a :href "http://weblocks.viridian-project.de/" "Weblocks Application Framework")
            " running on " (str (hunchentoot::address-string)))))))

