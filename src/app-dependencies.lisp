(defpackage #:weblocks/app-dependencies
  (:use #:cl)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:import-from #:weblocks/app
                #:get-js-backend
                #:app))
(in-package weblocks/app-dependencies)


(defmethod get-dependencies ((self app))
  (log:debug "Returning new-style dependencies for base application class.")
  
  (get-dependencies
   (get-js-backend self)))


