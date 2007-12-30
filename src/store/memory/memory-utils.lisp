
; Operations that support memory-backed stores
(in-package :weblocks-memory)

(export '(filter-objects-in-memory object-satisfies-search-p
	  order-objects-in-memory strictly-less-p equivalentp
	  range-objects-in-memory))

;;;;;;;;;;;;;;
;;; Filter ;;;
;;;;;;;;;;;;;;
(defgeneric object-satisfies-search-p (search-regex obj slot-name slot-type slot-value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Determines if 'slot-value' satisfies a search regex. Default
implementation applies 'search-regex' to string representations of
each slot value obtained via calling 'data-print-object', and if one
matches returns true.

'obj' - the object that contains the slot in question.
'slot-name' - name of the slot.
'slot-type' - declared type of the slot.
'slot-value' - the value to be tested."))

(defslotmethod object-satisfies-search-p (search-regex obj slot-name slot-type (slot-value standard-object)
						       &rest args)
  (if (render-slot-inline-p obj slot-name)
      (some (compose #'not #'null)
	    (flatten
	     (apply #'visit-object-slots slot-value (curry #'object-satisfies-search-p search-regex)
		    :call-around-fn-p nil args)))
      (not (null (ppcre:scan search-regex (object-name slot-value))))))

(defslotmethod object-satisfies-search-p (search-regex obj slot-name slot-type slot-value
						       &rest args)
  (not (null
	(ppcre:scan search-regex (apply #'data-print-object
					obj slot-name slot-type slot-value args)))))

(defun filter-objects-in-memory (seq filter &optional args)
  "Filters objects in 'seq' according to 'filter'."
  (if filter
      (remove nil
	      (mapcar (lambda (item)
			(when (apply #'object-satisfies-search-p (make-isearch-regex filter) nil nil t item
				     args)
			  item))
		      seq))
      seq))

;;;;;;;;;;;;;
;;; Order ;;;
;;;;;;;;;;;;;
(defgeneric strictly-less-p (a b)
  (:documentation
   "Returns true if 'a' is strictly less than 'b'. This function is
used by the framework for sorting data."))

(defmethod strictly-less-p ((a number) (b number))
  (< a b))

(defmethod strictly-less-p ((a string) (b string))
  (not (null (string-lessp a b))))

(defmethod strictly-less-p ((a symbol) (b symbol))
  (not (null (string-lessp a b))))

(defmethod strictly-less-p ((a string) (b symbol))
  (not (null (string-lessp a b))))

(defmethod strictly-less-p ((a symbol) (b string))
  (not (null (string-lessp a b))))

(defmethod strictly-less-p ((a (eql nil)) (b (eql nil)))
  nil)

(defmethod strictly-less-p (a (b (eql nil)))
  t)

(defmethod strictly-less-p ((a (eql nil)) b)
  nil)

(defmethod strictly-less-p ((a symbol) (b (eql nil)))
  t)

(defgeneric equivalentp (a b)
  (:documentation
   "Returns true if 'a' is in some sense equivalent to 'b'. This
function is used by the framework for sorting data."))

(defmethod equivalentp (a b)
  (equalp a b))

(defun order-objects-in-memory (seq order-by)
  "Orders objects in 'seq' according to 'order-by'."
  (if order-by
      (stable-sort seq
		   (if (equalp (cdr order-by) :asc)
		       #'strictly-less-p
		       (lambda (a b)
			 (and (not (strictly-less-p a b))
			      (not (equivalentp a b)))))
		   :key (curry-after #'slot-value-by-path (car order-by) :observe-inline-p t))
      seq))

;;;;;;;;;;;;;
;;; Range ;;;
;;;;;;;;;;;;;
(defun range-objects-in-memory (seq range)
  "Selects only the objects in 'range' from 'seq'."
  (if range
      (subseq seq (car range) (cdr range))
      seq))

