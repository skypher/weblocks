
(defpackage #:weblocks-clsql
  (:use :cl :metabang.utilities :clsql :weblocks)
  (:shadowing-import-from :metabang.utilities #:format-date #:filter
			  #:print-date)
  (:documentation
   "A driver for weblocks backend store API that connects to CLSQL."))

(in-package :weblocks-clsql)

(export '(order-by-expression range-to-offset range-to-limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :clsql)) &rest args)
  (setf *default-caching* nil)
  (setf *default-store* (apply #'make-instance 'fluid-database
			       :connection-spec args)))

(defmethod close-store ((store database))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (disconnect :database store))

(defmethod clean-store ((store database))
  (dolist (seq (list-sequences :database store))
    (drop-sequence seq :database store))
  (dolist (table (list-tables :database store))
    (delete-records :from table :database store)))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store database))
  (start-transaction :database store))

(defmethod commit-transaction ((store database))
  (when (in-transaction-p :database store)
    (commit :database store)))

(defmethod rollback-transaction ((store database))
  (when (in-transaction-p :database store)
    (rollback :database store)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod persist-object ((store database) object &key)
  ;; Note, we persist new objects in three steps, this should be
  ;; optimized into a single query later
  (let* ((class-name (class-name (class-of object)))
	 (current-id (object-id object))
	 (sequence-name (intern (concatenate 'string
					     (symbol-name class-name)
					     (symbol-name '#:-seq))
				(symbol-package class-name))))
    (unless current-id
      ;; Create sequence if necessary
      (unless (sequence-exists-p sequence-name :database store :owner :all)
	(create-sequence sequence-name :database store))
      ;; Set the id to next sequence number
      (setf (object-id object)
	    (sequence-next sequence-name :database store)))
    ;; Persist object
    (let (success)
      (unwind-protect
	   (progn
	     (update-records-from-instance object :database store)
	     (setf success t)
             object)
	(when (and (not success)
		   (null current-id))
	  (setf (object-id object) nil))))))

(defmethod delete-persistent-object ((store database) object)
  (delete-instance-records object))

(defmethod delete-persistent-object-by-id ((store database) class-name object-id)
  #.(locally-enable-sql-reader-syntax) 
  (delete-records :from class-name
		  :where [= (sql-expression :attribute (class-id-slot-name class-name))
		            object-id]
		  :database store)
  #.(restore-sql-reader-syntax-state))

;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;
(defun slot-db-info (class slot-name)
  "Returns clsql db-info structure."
  (clsql-sys::view-class-slot-db-info (find-slot-esd class slot-name)))

(defun class-order-by-join-classes (class-name order-by)
  "Returns a list of class names that need to be selected to find
instances of 'class-name' and order them with 'order-by'."
  (flet ((slot-join-class (class slot-name)
	   (gethash :join-class (slot-db-info class slot-name))))
    (loop
       for slot in (cons nil (drop-last (ensure-list (car order-by))))
       for class = class-name then (slot-join-class class slot)
       collect class)))

(defun class-order-by-join-where (class-name order-by)
  "Returns a 'where' expression that joins classes determined by
'class-order-by-join-classes' in order to find instances of
'class-name' and order them with 'order-by'."
  (when (and (car order-by)
	     (listp (car order-by))
	     (second (car order-by)))
    (flet ((slot-home-key (class slot-name)
	     (gethash :home-key (slot-db-info class slot-name)))
	   (slot-foreign-key (class slot-name)
	     (gethash :foreign-key (slot-db-info class slot-name))))
      (apply #'sql-operation 'and
	     (remove nil
		     (maplist (lambda (classes slots)
				(let ((c1 (first classes))
				      (c2 (second classes))
				      (slot (car slots)))
				  (when (and c1 c2)
				    (sql-operation '=
						   (sql-operation 'slot-value c1
								  (slot-home-key c1 slot))
						   (sql-operation 'slot-value c2
								  (slot-foreign-key c1 slot))))))
			      (class-order-by-join-classes class-name order-by)
			      (drop-last (car order-by))))))))

(defun order-by-expression (class-name order-by)
  "Converts the 'order-by' argument to a SQL expression."
  (let ((order-path (car order-by)))
    (when order-by
      (list (list (if (listp order-path)
		      (sql-operation 'slot-value
				     (last-element (class-order-by-join-classes class-name order-by))
				     (last-element order-path))
		      (sql-expression :attribute order-path))
		  (cdr order-by))))))

(defun range-to-offset (range)
  "Converts the 'range' argument to SQL OFFSET."
  (when range
    (car range)))

(defun range-to-limit (range)
  "Converts the 'range' argument to SQL LIMIT."
  (when range
    (- (cdr range) (car range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store database) class-name object-id)
  #.(locally-enable-sql-reader-syntax) 
  (car (select class-name
	       :where [= (sql-expression :attribute (class-id-slot-name class-name))
	                 object-id]
	       :flatp t :caching nil :database store))
  #.(restore-sql-reader-syntax-state))

(defmethod find-persistent-objects ((store database) class-name &key
				    order-by range where &allow-other-keys)
  (let ((order-by-join (class-order-by-join-where class-name order-by)))
    (select class-name
	    :order-by (order-by-expression class-name order-by)
	    :offset (range-to-offset range)
	    :limit (range-to-limit range)
	    :where (if (and order-by-join where)
		       (sql-operation 'and order-by-join where)
		       (or order-by-join where))
	    :flatp t :caching nil :database store)))

(defmethod count-persistent-objects ((store database) class-name
				     &key where
				     &allow-other-keys)
  (let (count)
    #.(locally-enable-sql-reader-syntax) 
    (setf count (car (select [count [*]]
			     :from (mapcar (lambda (table)
					     (make-instance 'clsql-sys::sql-ident-table :name table))
					   (adjoin (view-table (find-class class-name))
						   (mapcar (lambda (ident)
							     (slot-value ident 'clsql-sys::name))
							   (clsql-sys::collect-table-refs where))))
			     :where where
			     :flatp t :caching nil :database store)))
    #.(restore-sql-reader-syntax-state)
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLSQL/Weblocks Oddities ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod class-visible-slots-impl
    ((cls (eql (find-class 'clsql-sys::standard-db-object))) &key &allow-other-keys)
  "Hide slots on `standard-db-object'."
  '())
