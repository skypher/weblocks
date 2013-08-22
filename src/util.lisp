(defpackage #:weblocks-util
  (:use :cl :metabang.utilities :anaphora :hunchentoot :c2mop :f-underscore)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
                          #:standard-generic-function #:ensure-generic-function
                          #:standard-class #:typep #:subtypep #:standard-method)
  (:shadow #:slot-value-by-path)
  (:documentation "General Lisp utilities traditionally exported
                   with Weblocks."))

(in-package :weblocks-util)

(defun wexport (symbols-designator &optional (package-specs t))
  "Export SYMBOLS-DESIGNATOR from PACKAGE-SPECS.  Over `export',
PACKAGE-SPECS can be a list of packages, and the name designators
therein are interpreted by prepending \"WEBLOCKS-\".  In the latter
case, the symbols will be imported first if need be."
  (dolist (pkg (ensure-list package-specs))
    (multiple-value-bind (pkg import-first?)
	(typecase pkg
	  (boolean '#:weblocks-util)
	  (symbol (values (concatenate 'string (symbol-name '#:weblocks-)
				       (symbol-name pkg))
			  t))
	  (string (values (concatenate 'string (symbol-name '#:weblocks-util-) pkg)
			  t))
	  (otherwise pkg))
      (when import-first?
	(import symbols-designator pkg))
      (export symbols-designator pkg))))
