
(in-package :weblocks)

(wexport '(safe-subseq
           alist->plist
           insert-after
           insert-at
           drop-last
           intersperse
           list-starts-with
           stable-set-difference
           safe-getf
	   remove-keyword-parameter
           remove-keyword-parameters)
	 '(t util))

(defun safe-subseq (sequence start &optional end)
  "A safe alternative to subseq that automatically adjust indices."
  (let ((length (length sequence)))
    (when (> start length)
      (setf start length))
    (when (and end (> end length))
      (setf end length))
    (subseq sequence start end)))

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
		   (intern (symbol-name (car i)) keyword-package)
		   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun insert-after (newelt list index) 
  "Destructively inserts 'newelt' into 'list' after 'index'."
  (push newelt (cdr (nthcdr index list))) 
  list)

(defmacro insert-at (newelt list index) 
  "Destructively inserts 'newelt' into 'list' before 'index'."
  `(if (zerop ,index)
       (push ,newelt ,list)
       (insert-after ,newelt ,list (1- ,index))))

(defun ninsert (list thing pos)
    (if (zerop pos)
        (cons thing list)
        (let ((old-tail (nthcdr (1- pos) list)))
          (setf (cdr old-tail) (cons thing (cdr old-tail)))
          list)))

(defun drop-last (list)
  "Returns a copy of the list without the last element."
  (reverse (cdr (reverse list))))

(defun intersperse (list delimeter &key (last delimeter))
  "Intersperses a list with a delimeter.

If 'last' is specified, it will be used for the last delimeter,
instead of 'delimeter'.

\(intersperse '(1 2 3 4 5) 0)
=> (1 0 2 0 3 0 4 0 5)"
  (cond
    ((null list) list)
    ((null (cdr list)) list)
    ((null (cddr list)) (list (car list)
			      last
			      (cadr list)))
    (t (cons (car list)
	     (cons delimeter
		   (intersperse (cdr list) delimeter :last last))))))

(defun find-all (sequence predicate &key (key #'identity))
  "Returns a sequence of all elements found in 'sequence' that match
'predicate'. If 'key' is provides, each it is used to retreive each
item before passing it to 'predicate'."
  (loop for i in sequence
       when (funcall predicate (funcall key i))
       collect i))

(defun list-starts-with (list elements &key (test 'eq))
  "Determines if a list starts with the given elements."
  (let ((elements (ensure-list elements)))
    (if elements
	(when (funcall test (car list) (car elements))
	  (list-starts-with (cdr list) (cdr elements) :test test))
	t)))

(defun stable-set-difference (list-1 list-2 &key (test #'eql) (key #'identity))
  "Returns a list of element of 'list-1' that do not appear in 'list-2'. "
  (loop for i in list-1
       unless (find (funcall key i) list-2 :test test :key key)
       collect i))

(defun safe-getf (list name)
  "Like GETF but copes with odd argument lists.
Extracts the first value whose predecessor matches NAME.
Returns NIL as second value if the key wasn't found at
all."
  (let* ((result (if (evenp (length list))
                   (getf list name :not-found)
                   (aif (member name list :test #'eq)
                        (second it)
                        :not-found)))
         (foundp (not (eq result :not-found))))
    (if foundp
      (values result t)
      (values nil nil))))

(defun remove-keyword-parameter (parameter-list keyword)
  "Removes a keyword parameter from a parameter-list.
\(remove-keyword-parameter '(1 2 3 :a 1 :b 2 :c 3) :b)
=> (1 2 3 :a 1 :c 3)"
  (let (remove)
    (loop for i in parameter-list
          when (eql i keyword)
            do (setf remove t)
          else when remove
            do (setf remove nil)
          else collect i)))

(defun remove-keyword-parameters (parameter-list &rest keywords)
  "Removes all parameters with keys in 'keywords' from
'parameter-list'."
  (loop for argument in keywords
        with i = parameter-list
        do (setf i (remove-keyword-parameter i argument))
        finally (return i)))

(defun list->assoc (lst &key (map #'identity))
  "Nondestructively convert a list of elements to an association
list If an element of a list is a cons cell, it is left as
is. Otherwise, it is replaced with a cons cell whose 'car' is the
element and whose 'cdr' is a result of 'map' applied to the
element. The 'map' is an identity by default.

Ex:
\(list->assoc '(name age (city . location))) => ((name . name) (age . age) (city . location))
\(list->assoc '(1 (2 . 2) 3) :map #'1+) => ((1 . 2) (2 . 2) (3 . 4))"
  (mapcar (lambda (i)
	    (if (consp i) i (cons i (funcall map i))))
	  lst))

