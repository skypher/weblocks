
(in-package :weblocks-scripts)

(export '(document-weblocks))

(defun compute-documentation-path ()
  "Computes the directory where generated documentation should
reside."
  (merge-pathnames
   (make-pathname :directory '(:relative :up "docs" "gen"))
   (make-pathname :directory
		  (pathname-directory (truename (asdf:system-definition-pathname
                                                 (asdf:find-system :weblocks)))))))

(defun document-weblocks ()
  ; lazily load necessary systems
  (asdf:operate 'asdf:load-op 'tinaa)
  (asdf:operate 'asdf:load-op 'weblocks)
  ; dynamically invoke 'document-system' (since reader has no access to tinaa)
  (funcall (symbol-function (find-symbol (symbol-name '#:document-system) (find-package 'tinaa)))
	   'package
	   :weblocks (compute-documentation-path)
	   :show-parts-without-documentation? t))

