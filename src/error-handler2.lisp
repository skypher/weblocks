(defpackage #:weblocks.error-handler
  (:use #:cl)
  (:export
   #:on-error))
(in-package weblocks.error-handler)


(defgeneric on-error (app)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call weblocks.response:abort-processing like this:

                       \(weblocks.response:abort-processing
                           \"Unhandled condition\"
                           :code 500
                           :content-type \"text/plain\"\)

"))


(defmethod on-error (app)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  (weblocks.response:abort-processing "Unhandled condition"
                                      :code 500
                                      :content-type "text/plain"))



;; TODO: rethink this method implementation
(defmethod weblocks::handle-error-condition ((app weblocks:weblocks-webapp) c)
  "Print a pretty platform-specific backtrace if possible;
otherwise just call TRIVIAL-BACKTRACE to get a basic stack report."

  ;; (let ((html (if *show-lisp-errors-p*
  ;;                 ;; when verbose error should be reported
  ;;                 (with-error-page-html ("500 Weblocks Error" "Weblocks caught an error"
  ;;                                                             (escape-string (format nil "~A: ~A" (type-of c) c)))
  ;;                   (:h2 "Actions") ; FIXME: this should be a bar at the top of the page
  ;;                   (:ul
  ;;                    (:li (render-link (f_% (reset-webapp-session)) "Reset this web application's session")))
  ;;                   (:h2 "Session data")
  ;;                   (:table
  ;;                    (:thead
  ;;                     (:tr
  ;;                      (:th "") (:th "Key") (:th "Value")))
  ;;                    (:tbody
  ;;                     (let ((session-data weblocks.session::*session*))
  ;;                       (loop for i from 1
  ;;                             for key being the hash-key of session-data 
  ;;                             for value being the hash-value of session-data 
  ;;                             for parity = (if (oddp i) "odd" "even")
  ;;                             do 
  ;;                                (htm 
  ;;                                 (:tr :class parity
  ;;                                      (:td (str i))
  ;;                                      (:td (str key))
  ;;                                      (:td (esc (prin1-to-string value)))))))))
  ;;                   (:h2 "Restarts")
  ;;                   (:p "TODO")
  ;;                   (:h2 "Backtrace")
  ;;                   #-sbcl
  ;;                   (:pre (esc (format nil "~A"  (print-trivial-backtrace c :output nil))))
  ;;                   #+sbcl
  ;;                   (let ((frames (sb-debug:backtrace-as-list))
  ;;                         (*print-circle* t))
  ;;                     (htm
  ;;                      (:table
  ;;                       (:thead
  ;;                        (:tr
  ;;                         (:th "") (:th "Function") (:th "Arguments")))
  ;;                       (:tbody
  ;;                        (loop for frame in frames
  ;;                              for i from (length frames) downto 0
  ;;                              for parity = (if (oddp i) "odd" "even")
  ;;                              do (htm
  ;;                                  (:tr :class parity
  ;;                                       (:td :class "frame-number" (esc (format nil "~D" i)))
  ;;                                       (:td :class "frame-call" (:code (esc (format nil "~A" (car frame)))))
  ;;                                       (:td :class "frame-args"
  ;;                                            (:ol
  ;;                                             (dolist (arg (cdr frame))
  ;;                                               (htm (:li (:code (esc (format nil "~A" arg))))))))))))))))

  ;;                 ;; otherwise - just render a short message
  ;;                 (weblocks.error-handler:on-error )))))

  (let ((traceback (trivial-backtrace:print-backtrace c :output nil)))
    (log:error "Returning 500 error to user" traceback))
  
  ;; TODO: return code with stack trace rendering
  (weblocks.error-handler:on-error app))
