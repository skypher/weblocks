;;; diff-sexp.lisp -- diffs s-expressions based on Levenshtein-like edit distance.

;; Author:	Michael Weber <michaelw@foldr.org>
;; Date:	2005-09-03
;; Modified:	2005-09-04
;; Modified:	2005-09-07
;; Modified:	2005-09-15
;;
;; This code is in the Public Domain.

;;; Description:

;; DIFF-SEXP computes a diff between two s-expressions which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals |#+new|, |#-new|.

;;; Examples:

;; DIFF-SEXP> (diff-sexp  
;; 	  	'(DEFUN F (X) (+ (* X 2) 1)) 
;; 	  	'(DEFUN F (X) (- (* X 2) 3 1)))
;; ((DEFUN F (X) (|#-new| + |#+new| - (* X 2) |#+new| 3 1)))
;; DIFF-SEXP> (diff-sexp  
;; 	  	'(DEFUN F (X) (+ (* X 2) 4 1)) 
;; 	  	'(DEFUN F (X) (- (* X 2) 5 3 1)))
;; ((DEFUN F (X) (|#-new| + |#+new| - (* X 2) |#-new| 4 |#+new| 5 |#+new| 3 1)))
;; DIFF-SEXP> (diff-sexp  
;; 	  	'(DEFUN F (X) (+ (* X 2) 4 4 1)) 
;; 	  	'(DEFUN F (X) (- (* X 2) 5 5 3 1)))
;; ((DEFUN F (X) |#-new| (+ (* X 2) 4 4 1) |#+new| (- (* X 2) 5 5 3 1)))
;; DIFF-SEXP> 

;;; Todo:

;; * Support for moved subtrees
;; * The algorithm treats vectors, arrays, etc. as opaque objects
;; * Test harness
;; * Factor for better configurability
;; * This article might describe a better method (unchecked):
;;   Hélène Touzet: "A linear tree edit distance algorithm for similar ordered trees"
;;   LIFL - UMR CNRS 8022 - Université Lille 1
;;   59 655 Villeneuve d'Ascq cedex, France
;;   Helene.Touzet@lifl.fr


;;; Code:

(defpackage #:mw-diff-sexp
  (:use #:cl)
  (:export #:diff-sexp))

(in-package #:mw-diff-sexp)


(defun tree-size (tree)
  "Computes the number of atoms contained in TREE."
  (if (atom tree) 
      1 
      (reduce #'+ tree :key #'tree-size)))


(defclass edit-record ()
  ((edit-distance :reader edit-distance)))

(defclass unchanged-record (edit-record)
  ((change :reader change :initarg :change)))

(defclass deletion-record (edit-record)
  ((change :reader change :initarg :change)))

(defclass insertion-record (edit-record)
  ((change :reader change :initarg :change)))

(defclass update-record (edit-record)
  ((old :reader update-record-old :initarg :old)
   (new :reader update-record-new :initarg :new)))

(defclass compound-record (edit-record)
  ((changes :reader changes :initarg :changes))
  (:default-initargs 
   :changes ()))


(defmethod initialize-instance :after ((record deletion-record) &key)
  (setf (slot-value record 'edit-distance)
	(1+ (tree-size (slot-value record 'change)))))

(defmethod initialize-instance :after ((record insertion-record) &key)
  (setf (slot-value record 'edit-distance)
	(1+ (tree-size (slot-value record 'change)))))

(defmethod initialize-instance :after ((record update-record) &key)
  (setf (slot-value record 'edit-distance)
	(+ 1 (tree-size (update-record-old record))
	   1 (tree-size (update-record-new record)))))

(defmethod initialize-instance :after ((record unchanged-record) &key)
  (setf (slot-value record 'edit-distance)
	(tree-size (slot-value record 'change))))

(defmethod initialize-instance :after ((record compound-record) &key)
  (setf (slot-value record 'edit-distance)
	(reduce #'+ (slot-value record 'changes) :key #'edit-distance)))


(defun insertion-record (change)
  (make-instance 'insertion-record :change change))

(defun deletion-record (change)
  (make-instance 'deletion-record :change change))

(defun update-record (old new)
  (make-instance 'update-record :old old :new new))

(defun unchanged-record (change)
  (make-instance 'unchanged-record :change change))

(defun empty-compound-record ()
  (make-instance 'compound-record))

(defun extend-compound-record (r0 record)
  (make-instance 'compound-record :changes (list* record (changes r0))))


(defgeneric render-difference (record))

(defmethod render-difference ((record insertion-record))
  (list :|#+new| (change record)))

(defmethod render-difference ((record deletion-record))
  (list :|#-new| (change record)))

(defmethod render-difference ((record update-record))
  (list :|#-new| (update-record-old record) 
	:|#+new| (update-record-new record)))

(defmethod render-difference ((record unchanged-record))
  (list (change record)))

(defmethod render-difference ((record compound-record))
  (list (loop for r in (reverse (changes record))
	      append (render-difference r))))

(defun min/edit (record &rest records)
  "Returns record with minimum edit distance."
  (flet ((min/edit/2 (a b)
	   (if (<= (edit-distance a)
                   (edit-distance b))
               a b)))
    (declare (dynamic-extent #'min/edit/2))
    (reduce #'min/edit/2 records :initial-value record)))


(defun initial-distance (function list)
  "Prepares initial data vectors for Levenshtein algorithm from LIST."
  (loop with seq = (make-sequence '(vector edit-record) (1+ (length list)) 
				  :initial-element (empty-compound-record))
	for i from 0
	for elt in list do
	(setf (elt seq (1+ i))
              (extend-compound-record (elt seq i) (funcall function elt)))
	finally (return seq)))

(defun levenshtein-tree-edit (old-tree new-tree)
  "Calculates the minimal edits needed to transform OLD-TREE into NEW-TREE.
It minimizes the number of atoms in the result tree, also counting
edit conditionals."
  (cond
    ((equal old-tree new-tree)
     (unchanged-record old-tree))
    ((or (atom old-tree) (atom new-tree))
     (update-record old-tree new-tree))
    (t
     (min/edit
      (update-record old-tree new-tree)
      (loop with row = (initial-distance #'deletion-record old-tree)
	    and  col = (initial-distance #'insertion-record new-tree)
	    and  best-edit
	    for new-part in new-tree
	    for current across (subseq col 1) do
	    (loop for old-part in old-tree
		  for row-idx from 0 do
		  (setq best-edit 
			(min/edit (extend-compound-record (elt row (1+ row-idx))
							  (insertion-record new-part))
				  (extend-compound-record current
							  (deletion-record old-part))
				  (extend-compound-record (elt row row-idx) 
							  (levenshtein-tree-edit old-part new-part))))
		  (shiftf (elt row row-idx) current best-edit))
	    (setf (elt row (1- (length row))) best-edit)
	    finally (return best-edit))))))

(defun diff-sexp (old-tree new-tree)
  "Computes a diff between OLD-TREE and NEW-TREE which minimizes the
number of atoms in the result tree, also counting inserted edit conditionals
|#+new|, |#-new|."
  (render-difference (levenshtein-tree-edit old-tree new-tree)))

;;; Tests

#||
(defun n-ary-tree (arity depth gen)
  (if (> depth 0)
      (loop repeat arity collect (n-ary-tree arity (1- depth) gen))
      (funcall gen)))

(defun time/diff-sexp (n &key (arity 2))
  (let ((x 1))
    (flet ((generator ()
	     (prog1 x (incf x))))
      (let ((t1 (n-ary-tree arity n #'generator))
	    (t2 (n-ary-tree arity n #'generator)))
	(time (diff-sexp t1 t2))))))
||#
