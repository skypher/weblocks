(in-package :cm)

(defparameter *template-temporary-validation-errors* nil)
(defparameter *template-temporary-intermediate-values* nil)
(defparameter *out-of-band-template-vars* nil)

;============ utils
(defmacro string+ (&rest args)
  `(concatenate 'string ,@args))

(defun fill-template-widget (name &key (language "it") assoc assoc2)
  (declare (special *out-of-band-template-vars*))
  (setf html-template:*string-modifier* #'CL:IDENTITY)
  ;(warn (format nil "filltemplatewidget norml ~A" assoc))
  (warn (format nil "filltemplatewidget oob ~A" assoc2))
  
  (let ((filename (merge-pathnames (make-pathname :directory '(:relative "templates") :name name :type language )
				   *public-files-path*)))
    (make-instance 'html-template :file filename :vars (append assoc assoc2))))

(defun make-main-page-employee ()
  (with-html (:p "You are an employee")))

; from template-form-view.lisp put this into utils -- same as weblocks alist->plist, but converts clos object to obj-class-name
(defun my-alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
		   (intern (symbol-name (car i)) keyword-package)
		   "DONTCARE")
       collect (cdr i))))

;; (if (symbolp (car i))
;; 		   (intern (symbol-name (car i)) keyword-package)
;; 		   (intern (string-upcase (car i)) keyword-package))

; was to be used in conjunction with (clos->string (car i))
; in the original alist->plist above (in place of string-upcase (car i))
(defun clos->string (some)
  (ecase (class-of some)
    (:templform-view-field (view-field-name some))))
