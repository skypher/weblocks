;;; Code shared accross the entire weblocks framework

(in-package :weblocks)

; re-export external symbols from cl-cont
(do-external-symbols (s (find-package :cont))
  (export (list s)))

(export '(str with-javascript with-javascript-to-string))

(defun wexport (symbols-designator &optional (package-specs t))
  "Export SYMBOLS-DESIGNATOR from PACKAGE-SPECS.  Over `export',
PACKAGE-SPECS can be a list of packages, and the name designators
therein are interpreted by prepending \"WEBLOCKS-\".  In the latter
case, the symbols will be imported first if need be."
  (dolist (pkg (ensure-list package-specs))
    (multiple-value-bind (pkg import-first?)
        (typecase pkg
          (boolean '#:weblocks)
          (symbol (values (concatenate 'string (symbol-name '#:weblocks-)
                                       (symbol-name pkg))
                          t))
          (string (values (concatenate 'string (symbol-name '#:weblocks-) pkg)
                          t))
          (otherwise pkg))
      (when import-first?
        (import symbols-designator pkg))
      (export symbols-designator pkg))))

(defvar *dirty-widgets* "Top-level value. Causes widget-dirty-p to error."
  "Contains a list of dirty widgets at the current point in rendering
  cycle. This is a special variable modified by the actions that
  change state of widgets.")

;;; This turns off a regex optimization that eats A LOT of memory
(setq cl-ppcre:*use-bmh-matchers* nil)
