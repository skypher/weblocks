(defpackage #:weblocks/js/jquery
  (:use #:cl)
  (:import-from #:weblocks/js
                #:js-backend
                #:make-js-backend)
  (:import-from #:weblocks/dependencies
                #:get-dependencies
                #:make-dependency)
  ;; Just dependencies
  (:import-from #:log))
(in-package weblocks/js/jquery)


(defclass jquery-backend (js-backend)
  ())


(defmethod make-js-backend ((name (eql :jquery)))
  (make-instance 'jquery-backend))


(defvar *js-dependencies*
    (list (make-dependency "https://code.jquery.com/jquery-1.8.2.js"
                           ;; :integrity "sha256-ZosEbRLbNQzLpnKIkEdrPv7lOy9C27hHQ+Xp8a4MxAQ="
                           )
          (make-dependency "src/js/jquery/jquery.js"
                           :system :weblocks)

          (make-dependency "src/js/jquery/jquery.ba-bbq.js"
                           :system :weblocks)

          (make-dependency "src/js/jquery/jquery-seq.js"
                           :system :weblocks)

          (make-dependency "src/js/jquery/progress.gif"
                           :system :weblocks)))


(defmethod get-dependencies ((self jquery-backend))
  (log:debug "Returning dependencies for jquery backend.")
  (append *js-dependencies*
          (call-next-method)))
