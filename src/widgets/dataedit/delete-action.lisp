
(in-package :weblocks)

(export '(dataedit-delete-item dataedit-delete-items-flow))

(defun dataedit-delete-item (widget item-id)
  "Should be called by dataedit widgets to delete a single
item. Deletes the item, and if 'dataedit-cascade-delete-mixins-p' is
set to true, deletes mixin objects.

obj - the dataedit object.
item-id - the integer id of the object to be deleted."
  ;; delete mixin objects
  (if (dataedit-cascade-delete-mixins-p widget)
      (labels ((delete-mixin-objects (view obj)
		 (map-mixin-fields
		  (lambda (field-info)
		    (let* ((field (field-info-field field-info))
			   (mixin-obj (obtain-view-field-value field obj)))
		      (delete-mixin-objects (mixin-view-field-view field) mixin-obj)))
		  view obj)
		 (delete-persistent-object (dataseq-class-store widget) obj)))
	(delete-mixin-objects (dataseq-view widget)
			      (find-persistent-object-by-id (dataseq-class-store widget)
							    (dataseq-data-class widget)
							    item-id)))
      (delete-persistent-object-by-id (dataseq-class-store widget)
				      (dataseq-data-class widget)
				      item-id)))

;;; This function processes actions that perform operations on dataseq
;;; items. It's in a separate file because there is an issue with
;;; CMUCL and compiling this function transformed through
;;; CL-CONT. CMUCL interpreter, however, works. We'll just add this to
;;; ASDF for all implementations except CMUCL, and for CMUCL this file
;;; will be loaded without compilation.  This function is tested as
;;; part of gridedit.
(defun/cc dataedit-delete-items-flow (obj items)
  "If 'dataedit-on-delete-items' is specified, simply calls
'dataedit-on-delete-items'. Otherwise, if 'data' is a sequence,
deletes the specified items from the sequence. The 'items' parameter
is similar to datagrid's 'selection' slot. Specialize this function to
modify standard behavior for deleting items from a sequence."
  (let ((item-name (humanize-name (dataseq-data-class obj))))
    (when (dataseq-selection-empty-p items)
      (flash-message (dataseq-flash obj)
		     (format nil "Please select ~A to delete."
			     (pluralize (string-downcase item-name))))
      (mark-dirty obj)
      (return-from dataedit-delete-items-flow))
    (let ((initial-items-count (dataseq-data-count obj))
	  deleted-items-count)
      (unless (eq :yes (do-confirmation (let ((item-count (ecase (car items)
							    ;; (:all ...)
							    (:none (length (cdr items))))))
					  (format nil "Delete ~A ~A?"
						  item-count
						  (proper-number-form item-count item-name)))
			 :type :yes/no))
	(return-from dataedit-delete-items-flow))
      (if (dataedit-on-delete-items obj)
	  (funcall (dataedit-on-delete-items obj) obj items)
	  (ecase (car items)
	    ;; (:all ...)
	    (:none (dolist (item (cdr items))
		     (dataedit-delete-item obj item)))))
      (setf deleted-items-count (- initial-items-count (dataseq-data-count obj)))
      (mark-dirty obj :propagate t)
      (flash-message (dataseq-flash obj)
		     (format nil "Deleted ~A ~A."
			     deleted-items-count
			     (proper-number-form deleted-items-count item-name)))
      (safe-funcall (dataedit-on-delete-items-completed obj) obj items))))

