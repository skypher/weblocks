
(in-package :simple-blog)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *blog-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :simple-blog)))

