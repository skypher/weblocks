(defpackage #:weblocks-postmodern
  (:use :cl :postmodern :weblocks)
  (:shadowing-import-from :postmodern #:text)
  (:shadowing-import-from :weblocks #:commit-transaction)
  (:documentation
   "A driver for weblocks backend store API that connects to Postmodern."))

(in-package :weblocks-postmodern)

(export '())

(defvar *transaction* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :postmodern)) &rest args)
  ;; You /really/ want to use thread pools. They're good for you.
  (let ((pooled-args (unless (getf args :pooled-p)
		       (append args '(:pooled-p t)))))
    (setf *default-store* (apply #'connect pooled-args))
    (setf *database* *default-store*)))

(defmethod close-store ((store database-connection))
  (when (eq *default-store* store)
    (disconnect *default-store*)
    (setf *default-store* nil)
    (setf *database* nil)))

(defmethod clean-store ((store database-connection))
  (dolist (seq (list-sequences))
    (query (:drop-sequence seq)))
  (dolist (view (list-views))
    (query (:drop-view view)))
  (dolist (table (list-tables))
    (query (:delete-from table))))


;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store database-connection))
  (setf *transaction* (make-instance 'postmodern::transaction-handle))
  (execute "BEGIN"))

(defmethod commit-transaction ((store database-connection))
  (commit-transaction *transaction*))

(defmethod rollback-transaction ((store database-connection))
  (abort-transaction *transaction*))

(defmethod dynamic-transaction ((store database-connection) proc)
  (with-transaction ()
    (funcall proc)))

(defmethod use-dynamic-transaction-p ((store database-connection))
  ;; For now...
  (declare (ignore store))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod persist-object ((store database-connection) object &key)
  (save-dao object)) ;; use save-dao/transaction instead?

(defmethod delete-persistent-object ((store database-connection) object)
  (delete-dao object))

(defmethod delete-persistent-object-by-id ((store database-connection) class-name object-id)
  (delete-dao (get-dao class-name object-id)))


;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;
(defmethod class-id-slot-name ((class dao-class))
  ;; Returns a list of the column names which compose the primary key.
  (dao-keys class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store database-connection) class-name object-id)
  (get-dao class-name object-id))

(defmethod find-persistent-objects ((store database-connection) class-name
				    &key order-by range where
				    &allow-other-keys)
  (let* ((base-expr `(:select '* :from ,class-name ,@(when where (list :where where))))
	 (order-expr (or `(,@(when order-by
			       `(:order-by ,base-expr ,(car order-by)))) base-expr))
	 (sql-expr (or `(,@(when range
			     `(:limit ,order-expr ,(cdr range) ,(car range)))) order-expr)))
    (query-dao class-name (sql-compile sql-expr))))

(defmethod count-persistent-objects ((store database-connection) class-name
				     &key where &allow-other-keys)
  (let ((sql-expr `(:select (:count '*) :from ,class-name
			    ,@(when where (list :where where)))))
    (query (sql-compile sql-expr) :single)))
