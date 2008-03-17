
; Operations that support memory-backed stores
(in-package :weblocks-memory)

(export '(make-scratch-store objects-from-scratch-store
	  filter-objects-in-memory object-satisfies-search-p
	  order-objects-in-memory strictly-less-p equivalentp
	  range-objects-in-memory))

;;;;;;;;;;;;;;;;;;;;;;
;;; Scratch stores ;;;
;;;;;;;;;;;;;;;;;;;;;;
(defun make-scratch-store (&optional list)
  "Accepts an optional list of objects, and creates a memory store
from them."
  (let ((store (make-instance 'memory-store)))
    (dolist (obj list store)
      (persist-object store obj))))

(defun objects-from-scratch-store (store)
  "Accepts a memory store and returns a list of objects stored in
it."
  (loop for table being the hash-values in (memory-store-root-objects store)
       append (loop for i being the hash-values in (persistent-objects-of-class-by-id table)
		 collect i)))

;;;;;;;;;;;;;;
;;; Filter ;;;
;;;;;;;;;;;;;;
(defgeneric object-satisfies-search-p (search-regex obj view)
  (:documentation
   "Determines if a view of 'obj' satisfies a search regex. Default
implementation applies 'search-regex' to string representations of
each view field value (obtained via calling 'print-view-field-value'),
and if one matches returns true.

'search-regex' - regular expression to be applied.
'obj' - the object that contains the slot in question.
'view' - name of the slot.")
  (:method (search-regex obj view)
    (when (null view)
      (setf view (find-view (list 'data (class-name (class-of obj))))))
    (some (lambda (field-info)
	    (let ((field (field-info-field field-info))
		  (obj (field-info-object field-info)))
	      (not (null (ppcre:scan search-regex (print-view-field-value
						   (obtain-view-field-value field obj)
						   (view-field-presentation field)
						   field view nil obj))))))
	  (get-object-view-fields obj view))))

(defun filter-objects-in-memory (seq filter view)
  "Filters objects in 'seq' presented with view according to
'filter'."
  (if filter
      (remove nil
	      (mapcar (lambda (item)
			(when (funcall #'object-satisfies-search-p (make-isearch-regex filter) item view)
			  item))
		      seq))
      seq))

;;;;;;;;;;;;;
;;; Order ;;;
;;;;;;;;;;;;;
(defgeneric strictly-less-p (a b)
  (:documentation
   "Returns true if 'a' is strictly less than 'b'. This function is
used by the framework for sorting data.")
  (:method (a b)
    (strictly-less-p (format nil "~A" a) (format nil "~A" b)))
  (:method ((a number) (b number))
    (< a b))
  (:method ((a string) (b string))
    (not (null (string-lessp a b))))
  (:method ((a null) (b null))
    nil)
  (:method (a (b null))
    t)
  (:method ((a null) b)
    nil))

(defgeneric equivalentp (a b)
  (:documentation
   "Returns true if 'a' is in some sense equivalent to 'b'. This
function is used by the framework for sorting data.")
  (:method (a b)
    (equalp a b)))

(defun order-objects-in-memory (seq order-by)
  "Orders objects in 'seq' according to 'order-by'."
  (if order-by
      (stable-sort seq
		   (if (equalp (cdr order-by) :asc)
		       #'strictly-less-p
		       (lambda (a b)
			 (and (not (strictly-less-p a b))
			      (not (equivalentp a b)))))
		   :key (curry-after #'slot-value-by-path (car order-by)))
      seq))

;;;;;;;;;;;;;
;;; Range ;;;
;;;;;;;;;;;;;
(defun range-objects-in-memory (seq range)
  "Selects only the objects in 'range' from 'seq'."
  (if range
      (subseq seq (car range) (cdr range))
      seq))

