
(in-package :weblocks)

;;; This function processes actions that perform operations on
;;; datagrid items. It's in a separate file because there is an issue
;;; with CMUCL and compiling this function transformed through
;;; CL-CONT. CMUCL interpreter, however, works. We'll just add this to
;;; ASDF for all implementations except CMUCL, and for CMUCL this file
;;; will be loaded without compilation.
;;; This function is tested as part of render-widget-body for
;;; datagrid.
(defun/cc item-ops-action (obj &rest args)
  (declare (ignore args))
  (datagrid-clear-selection obj)
  (loop for i in (request-parameters)
     when (string-starts-with (car i) "item-")
     do (datagrid-select-item obj (substring (car i) 5)))
  (loop for i in (datagrid-item-ops obj)
     when (member (car i) (request-parameters)
		  :key #'car
		  :test #'string-equal)
     do (funcall (cdr i) obj (datagrid-selection obj)))
  (datagrid-clear-selection obj))

