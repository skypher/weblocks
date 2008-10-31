
(in-package :weblocks-demo-popover)

;;; Custom copying from stores to sandbox users
(defun init-sandbox-store ()
  "Copies the fixtures from the disk store to a sandbox store (to
ensure users have their own non-peristant sandboxes)."
  (let ((sandbox-store (open-store :memory))
	(fixtures-store (open-store :prevalence
				    (merge-pathnames (make-pathname :directory '(:relative "data"))
						     (asdf-system-directory :weblocks-demo-popover)))))
    (unwind-protect
	 (progn
	   (persist-objects sandbox-store (find-persistent-objects fixtures-store 'employee))
	   (persist-objects sandbox-store (find-persistent-objects fixtures-store 'company))
	   (setf (sandbox-store) sandbox-store))
      (close-store fixtures-store))))

;;; Instances of our model should be obtained from the sandbox store
(defmethod class-store ((class-name (eql 'employee)))
  (sandbox-store))

(defmethod class-store ((class-name (eql 'company)))
  (sandbox-store))

