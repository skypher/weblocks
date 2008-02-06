
(in-package :weblocks)

;;; This function processes actions that perform operations on
;;; gridedit items. It's in a separate file because there is an issue
;;; with CMUCL and compiling this function transformed through
;;; CL-CONT. CMUCL interpreter, however, works. We'll just add this to
;;; ASDF for all implementations except CMUCL, and for CMUCL this file
;;; will be loaded without compilation.  This function is tested as
;;; part of gridedit.
(defun/cc gridedit-delete-items (grid items)
  "If 'on-delete-items' is specified, simply calls
'on-delete-items'. Otherwise, if 'data' is a sequence, deletes the
specified items from the sequence. The 'items' parameter is similar to
datagrid's 'selection' slot. Specialize this function to modify
standard behavior for deleting items from a sequence."
  (when (datagrid-selection-empty-p items)
    (flash-message (datagrid-flash grid) "Please select items to delete.")
    (mark-dirty grid)
    (return-from gridedit-delete-items))
  (let ((initial-items-count (datagrid-data-count grid :totalp t))
	deleted-items-count)
    (if (gridedit-on-delete-items grid)
	(funcall (gridedit-on-delete-items grid) grid items)
	(progn
	  (unless (eq :yes (do-confirmation (let ((item-count (ecase (car items)
								;; (:all ...)
								(:none (length (cdr items))))))
					      (format nil "Delete ~A ~A?"
						      item-count
						      (proper-number-form item-count "item")))
			     :type :yes/no))
	    (return-from gridedit-delete-items))
	  (ecase (car items)
	    ;; (:all ...)
	    (:none (dolist (item (cdr items))
		     (gridedit-delete-item grid (parse-integer item)))))))
    (setf deleted-items-count (- initial-items-count (datagrid-data-count grid :totalp t)))
    (mark-dirty grid)
    (flash-message (datagrid-flash grid)
		   (format nil "~A ~A deleted."
			   deleted-items-count
			   (proper-number-form deleted-items-count "item")))))

