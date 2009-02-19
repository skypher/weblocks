
; Operations that support memory-backed stores
(in-package :weblocks-memory)

(export '(make-scratch-store objects-from-scratch-store
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
  (if (and seq
           order-by)
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
  (if (and seq
           range)
      (let ((len (length seq)))
        (subseq seq (min len (car range)) (min len (cdr range))))
      seq))

