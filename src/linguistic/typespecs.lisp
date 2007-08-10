
(in-package :weblocks)

(export '(value-required-p typespec-compound-only-p
	  atomic-typespec-to-error compound-typespec-to-error))

;;; Required fields
(defun value-required-p (typespec)
  "Determines if 'typespec' accepts null values by checking if nil is
of type specified by 'typespec'."
  (not (typep nil typespec)))

;;; Compound-only typespecs
(defun typespec-compound-only-p (typespec-name)
  (not (null (member typespec-name '(and eql member mod not or satisfies values)))))

;;; Typespecs that accept adjectives
(defun typespec-accepts-adjectives-p (typespec)
  (unless (member (if (listp typespec)
		      (car typespec)
		      typespec)
		  '(satisfies not eql member))
    t))

;;; Error message generation
(defun typespec-to-error (typespec)
  "Converts a typespec to an error message suitable for displaying to
users in a form \"must be ...\"."
  (format nil "must be ~A" (typespec-to-error-aux typespec)))

(defun typespec-to-error-aux (typespec &key adjectives)
  "Calls either 'atomic-typespec-to-error' or
'compound-typespec-to-error' depending on the 'typespec'."
  (if (listp typespec)
      (compound-typespec-to-error (car typespec) (cdr typespec) :adjectives adjectives)
      (atomic-typespec-to-error typespec :adjectives adjectives)))

;;; Atomic specifiers
(defgeneric atomic-typespec-to-error (typespec &key adjectives)
  (:documentation
   "Converts an atomic 'typespec' to a subset of an error message
suitable for displaying to users. The subset must be of form useable
in a message \"Field must be [message subset], [message subset],
etc\"."))

(defmethod atomic-typespec-to-error ((typespec cons) &key adjectives)
  (error "~A is a not an atomic type specifier." typespec))

(defmethod atomic-typespec-to-error ((typespec symbol)  &key adjectives)
  (let ((downcased-humanized-spec (string-downcase (humanize-name typespec))))
    (if (and adjectives (not (eql adjectives t)))
	(concatenate 'string (articlize adjectives) " " downcased-humanized-spec)
	(if (eql adjectives t)
	    downcased-humanized-spec
	    (articlize downcased-humanized-spec)))))

(defmethod atomic-typespec-to-error ((typespec (eql 'null)) &key adjectives)
  (concatenate 'string adjectives
	       (when adjectives
		 ", and ")
	       "empty"))

;;; Compoound specifiers
(defgeneric compound-typespec-to-error (typespec args &key adjectives)
  (:documentation
   "Converts a compound 'typespec' to a subset of an error message
suitable for displaying to users. 'typespec' is the 'car' of the
compound list, and 'args' are the 'cdr- of the list."))

(defmethod compound-typespec-to-error ((typespec (eql 'integer)) args &key adjectives)
  (apply #'concatenate 'string (if adjectives
				   (articlize adjectives)
				   "an ")
	 (when adjectives " ") "integer"
	 (destructuring-bind (a &optional b) args
	   (remove nil
		   (list
		    (unless (eql a '*)
		      (if (and b (not (eql b '*)))
			  (format nil " between ~A" a)
			  (format nil " greater than or equal to ~A" a)))
		    (unless (or (eql b '*) (not b))
		      (if (not (eql a '*))
			  (format nil " and ~A" b)
			  (format nil " less than or equal to ~A" b))))))))

(defmethod compound-typespec-to-error ((typespec (eql 'satisfies)) args &key adjectives)
  (assert (null adjectives))
  (destructuring-bind (p) args
    (check-type p symbol)
    (let ((attributized-predicate (attributize-name p)))
      (string-downcase (humanize-name (or (string-remove-right attributized-predicate "-p")
					  (string-remove-right attributized-predicate "p")))))))

(defmethod compound-typespec-to-error ((typespec (eql 'or)) args &key adjectives)
  (case (length args)
    (0 (error "~A is an impossible type." (cons typespec args)))
    (1 (typespec-to-error-aux (car args) :adjectives adjectives))
    (otherwise (apply #'concatenate 'string (if adjectives
						(format nil "~A " adjectives)
						"either ")
		      (intersperse (mapcar (curry-after #'typespec-to-error-aux :adjectives (when adjectives
											      t))
					   (remove 'null args))
				   ", " :last (if (eql (length args) 2) " or " ", or "))))))

(defmethod compound-typespec-to-error ((typespec (eql 'and)) args &key adjectives)
  (flet ((typespecs-to-adjectives (typespecs)
	   (apply #'concatenate 'string
		  (intersperse (mapcar #'typespec-to-error-aux
				       (remove 'null typespecs)) ", "))))
    (case (length args)
      (0 "anything you want")
      (1 (typespec-to-error-aux (car args)))
      (otherwise (let ((result (loop for i in args
				     with predicates
				  if (eql (car-safe i) 'satisfies)
				    do (push-end i predicates)
				  else if (typespec-accepts-adjectives-p i)
				         collect (typespec-to-error-aux
					          i :adjectives (if predicates
							            (typespecs-to-adjectives predicates)
							            (when adjectives
								      t)))
				           into results
				       else
				         append (mapcar #'typespec-to-error-aux predicates) into results and
				         collect (typespec-to-error-aux i) into results
				       end
				       and do (setf predicates nil)
				  end
				  finally (return (append results 
							  (mapcar #'typespec-to-error-aux predicates))))))
		   (apply #'concatenate 'string
			  (when adjectives
			      (format nil "~A " adjectives))
			  (intersperse result ", " :last (if (eql (length result) 2) " and " ", and "))))))))

(defmethod compound-typespec-to-error ((typespec (eql 'eql)) args &key adjectives)
  (assert (null adjectives))
  (format nil "equal to ~A" (car args)))

(defmethod compound-typespec-to-error ((typespec (eql 'not)) args &key adjectives)
  (assert (null adjectives))
  (format nil "not ~A" (typespec-to-error-aux (car args))))

(defmethod compound-typespec-to-error ((typespec (eql 'mod)) args &key adjectives)
  (concatenate 'string (if adjectives
			   (articlize adjectives)
			   "a ")
	       (when adjectives ", ")
	       (format nil "non-negative integer less than ~A" (car args))))

(defmethod compound-typespec-to-error ((typespec (eql 'member)) args &key adjectives)
  (assert (null adjectives))
  (case (length args)
    (0 (error "~A is an impossible type." (cons typespec args)))
    (1 (format nil "~A" (car args)))
    (otherwise (apply #'concatenate 'string "either "
		      (intersperse (mapcar (curry #'format nil "~A") (remove 'null args))
				   ", " :last (if (eql (length args) 2) " or " ", or "))))))

(defmethod compound-typespec-to-error ((typespec (eql 'values)) args &key adjectives)
  (error "Not Implemented!"))

(defmethod compound-typespec-to-error (typespec args &key adjectives)
  ;; by default, just output the car
  (typespec-to-error-aux typespec))

