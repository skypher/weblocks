
(in-package :{APPNAME})

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *{APPNAME}-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :{APPNAME})))

