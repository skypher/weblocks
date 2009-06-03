;;; http://groups.google.com/group/weblocks/browse_thread/thread/bec1e1507905116
(in-package #:weblocks)

(defmethod class-store (class-name) :around
           (typecase (find-class class-name)
             (db.allegrocache::persistent-class
              db.allegrocache:*allegrocache*)
             (t (call-next-method))))

(defmethod object-id (obj)
  (db.allegrocache:db-object-oid obj))

(defmethod count-persistent-objects ((store db.allegrocache::database)
                                     class-name  &key &allow-other-keys)
  (let ((val 0))
    (let ((db.allegrocache:*allegrocache* store))
      (db.allegrocache:doclass* (x (find-class class-name))
        (incf val)))
    val))

(defmethod find-persistent-object-by-id ((store db.allegrocache::database)
                                     class-name id)
  (db.allegrocache:oid-to-object class-name id :db store))

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

(defmethod find-persistent-objects ((store db.allegrocache::database) class-name &key order-by range &allow-other-keys)
  (order-objects-in-memory
   (range-objects-in-memory
    (let (val)
      (let ((db.allegrocache:*allegrocache* store))
        (db.allegrocache:doclass* (x (find-class class-name))
          (push x val)))
      val) range) order-by)) 

