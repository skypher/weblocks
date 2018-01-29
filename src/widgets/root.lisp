(defpackage #:weblocks/widgets/root
  (:use #:cl)
  (:shadow #:get)
  ;; Just dependencies
  (:import-from #:weblocks/session)
  
  (:export #:get))
(in-package weblocks/widgets/root)


(defun get ()
  (weblocks/session:get-value 'root-widget))


(defun (setf get) (value)
    (setf (weblocks/session:get-value 'root-widget)
     value))
