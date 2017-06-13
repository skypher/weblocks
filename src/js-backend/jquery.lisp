(defpackage #:weblocks.js.jquery
  (:use #:cl))
(in-package weblocks.js.jquery)


(defclass jquery-backend (weblocks.js:js-backend)
  ())


(defmethod weblocks.js:make-js-backend ((name (eql :jquery)))
  (make-instance 'jquery-backend))


(defmethod weblocks.dependencies:get-dependencies ((self jquery-backend))
  (log:debug "Returning new-style dependencies for jquery backend.")


  (list (weblocks.dependencies:make-remote-js-dependency
         "https://code.jquery.com/jquery-1.8.2.js"
         ;; :integrity "sha256-ZosEbRLbNQzLpnKIkEdrPv7lOy9C27hHQ+Xp8a4MxAQ="
         )
        ;; depends on src/utils/misc.lisp
        (weblocks.dependencies:make-static-js-dependency
         "src/js-backend/jquery/jquery.js"
         :system :weblocks)

        (weblocks.dependencies:make-static-js-dependency
         "src/js-backend/jquery/jquery.ba-bbq.js"
         :system :weblocks)

        (weblocks.dependencies:make-static-image-dependency
         "src/js-backend/jquery/progress.gif"
         :system :weblocks)))
