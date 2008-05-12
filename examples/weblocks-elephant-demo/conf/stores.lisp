
(in-package :weblocks-elephant-demo)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *elephant-store* :elephant 
  :spec `(:BDB ,(namestring (merge-pathnames (make-pathname :directory '(:relative "data"))
					     (asdf-system-directory :weblocks-elephant-demo)))))

