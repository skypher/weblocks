
(in-package :weblocks-scripts)

(export '(document-weblocks))

(defun compute-documentation-path ()
  "Computes the directory where generated documentation should
reside."
  (merge-pathnames
   (make-pathname :directory '(:relative "docs" "gen"))
   (make-pathname :directory
		  (pathname-directory (truename (asdf:system-definition-pathname
                                                 (asdf:find-system :weblocks)))))))

(defun document-package (package &key (system package) (ignore-errors-p t))
  (flet ((load-system (s) (if ignore-errors-p
                            (ignore-errors (asdf:operate 'asdf:load-op s))
                            (asdf:operate 'asdf:load-op s))))
    (let (errorp)
      (unless (find-package package)
        (setf errorp (not (load-system system))))
      (unless errorp
        (document-package/tinaa package)))))
  
(defun document-package/tinaa (package)
  ;; dynamically invoke 'document-system' (since reader has no access to tinaa)
  (funcall (symbol-function (find-symbol (symbol-name '#:document-system) (find-package 'tinaa)))
	   'package package
           (compute-documentation-path)
	   :show-parts-without-documentation? t))

(defun document-weblocks ()
  ; lazily load necessary systems
  (unless (find-package :tinaa)
    (asdf:operate 'asdf:load-op 'tinaa))
  (document-package :weblocks :ignore-errors-p nil)
  (document-package :weblocks-prevalence)
  (document-package :weblocks-elephant)
  (document-package :weblocks-memory)
  (document-package :weblocks-clsql))

