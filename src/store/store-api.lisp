
(in-package :weblocks)

(export '(open-store close-store clean-store *default-store*
	  supports-filter-p begin-transaction commit-transaction
	  rollback-transaction persist-object delete-persistent-object
	  delete-persistent-object-by-id find-persistent-objects
	  count-persistent-objects))

;;; Store initialization and finalization
(defgeneric open-store (store-type &rest args)
  (:documentation "Opens a connection to a store specified by
  'store-type'. This function must return the instance of the
  connection to the store. Methods can accept any number of custom
  keyword parameters. Additionally, the function must set
  *default-store* to the value of newly created store."))

(defgeneric close-store (store)
  (:documentation "Closes a connection to the store. If the value of
  *default-store* is equal to 'store', *default-store* must be set to
  NIL."))

(defgeneric clean-store (store)
  (:documentation "Cleans all the data in the store. This function
  should erase data, but not necessarily any schema information (like
  tables, etc.)"))

(defparameter *default-store* nil
  "The default store to which objects are persisted.")

;;; Store information
(defgeneric supports-filter-p (store)
  (:documentation "Because full text search is often prohibitively
  difficult to fully implement, backends must specialize this function
  to let weblocks query for filtering support."))

;;; Transactions
(defgeneric begin-transaction (store)
  (:documentation "Begins a transaction on 'store'. Note, if the given
  store does not have transaction support, this function should return
  NIL without signalling errors."))

(defgeneric commit-transaction (store)
  (:documentation "Commits a transaction started on 'store'. Note, if
  the given store does not have transaction support, or the store
  isn't in a transaction, this function should return NIL without
  signalling errors."))

(defgeneric rollback-transaction (store)
  (:documentation "Rolls back a transaction started on 'store'. Note,
  if the given store does not have transaction support, or the store
  isn't in a transaction, this function should return NIL without
  signalling errors."))

;;; Creating and deleting persistent objects
(defgeneric persist-object (store object)
  (:documentation "Persists 'object' into 'store'. If the object does
  not have a unique ID (see 'object-id'), persist 'object' into store
  and set its unique ID via (see '(setf object-id)'). If the object
  has a unique ID, find relevant entry in the store and update it with
  'object'."))

(defgeneric delete-persistent-object (store object)
  (:documentation "Deletes the persistent object from 'store'. After
  deleting the persistent object, set unique ID of 'object' to
  NIL (see '(setf object-id)')."))

(defgeneric delete-persistent-object-by-id (store class-name object-id)
  (:documentation "Similar to 'delete-persistent-object', but instead
  deletes object with the specified 'object-id'."))

;;; Querying persistent objects
(defgeneric find-persistent-objects (store class-name &key filter
					   filter-args order-by range)
  (:documentation "Looks up and returns objects of appropriate
  'class-name' in the 'store' bound by the given keyword
  parameters.

  If 'filter' is specified, filters the returned objects according to
  the string provided. This is expected to be a full text search.

  'filter-args' are implementation dependent argument for the
  filtering engine that may configure the behavior of the query.

  If 'order-by' is specified, orders the returned objects by the given
  slot in the given order. If 'order-by' is not NIL, it is expected to
  be a cons cell with slot name as 'car' and :asc or :desc as
  'cdr'.

  If 'range' is specified, returns only the specified range of
  objects. The CAR of 'range' is the index of the initial
  object (inclusive) and CDR is the index past the last object. Note,
  the range should be applied after the objects have been filtered and
  ordered if necessary."))

(defgeneric count-persistent-objects (store class-name &key filter
					    filter-args order-by range)
  (:documentation "Returns the number of persistent objects stored in
  'store' of 'class-name', bound by the given keyword parameters. For
  documentation of keyword parameters, see
  'find-persistent-objects'."))

