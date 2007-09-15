
(in-package :weblocks)

(export '(humanize-typespec humanize-atomic-typespec-aux
	  humanize-compound-typespec-aux))

(defun humanize-typespec (typespec &key adjectives)
  "Calls either 'humanize-atomic-typespec-aux' or
'humanize-compound-typespec-aux' depending on the 'typespec'."
  (if (listp typespec)
      (humanize-compound-typespec-aux (car typespec) (cdr typespec) :adjectives adjectives)
      (humanize-atomic-typespec-aux typespec :adjectives adjectives)))

;;; Typespecs that accept adjectives
(defun typespec-accepts-adjectives-p (typespec)
  (unless (member (if (listp typespec)
		      (car typespec)
		      typespec)
		  '(satisfies not eql member))
    t))

;;; Atomic specifiers
(defgeneric humanize-atomic-typespec-aux (typespec &key adjectives)
  (:documentation
   "Converts an atomic 'typespec' to a subset of a message suitable
for displaying to users. The subset is of form suitable for message
\"Field must be [message subset], [message subset], etc\"."))

(defmethod humanize-atomic-typespec-aux ((typespec cons) &key adjectives)
  (error "~A is a not an atomic type specifier." typespec))

(defmethod humanize-atomic-typespec-aux ((typespec symbol)  &key adjectives)
  (let ((downcased-humanized-spec (string-downcase (humanize-name typespec))))
    (if (and adjectives (not (eql adjectives t)))
	(concatenate 'string (articlize adjectives) " " downcased-humanized-spec)
	(if (eql adjectives t)
	    downcased-humanized-spec
	    (articlize downcased-humanized-spec)))))

(defmethod humanize-atomic-typespec-aux ((typespec (eql 'null)) &key adjectives)
  (concatenate 'string adjectives
	       (when adjectives
		 ", and ")
	       "empty"))

;;; Compound specifiers
(defgeneric humanize-compound-typespec-aux (typespec args &key adjectives)
  (:documentation
   "Converts a compound 'typespec' to a subset of a message suitable
for displaying to users. 'typespec' is the 'car' of the compound list,
and 'args' are the 'cdr- of the list."))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'integer)) args &key adjectives)
  (declare (optimize (safety 3)))
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

(defmethod humanize-compound-typespec-aux ((typespec (eql 'satisfies)) args &key adjectives)
  (declare (optimize (safety 3)))
  (assert (null adjectives))
  (destructuring-bind (p) args
    (check-type p symbol)
    (let ((attributized-predicate (attributize-name p)))
      (string-downcase (humanize-name (or (string-remove-right attributized-predicate "-p")
					  (string-remove-right attributized-predicate "p")))))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'or)) args &key adjectives)
  (when (>= (length args) 2)
    (setf args (remove 'null args)))
  (case (length args)
    (0 (error "~A is an impossible type." (cons typespec args)))
    (1 (humanize-typespec (car args) :adjectives adjectives))
    (otherwise (apply #'concatenate 'string (if adjectives
						(format nil "~A " adjectives)
						"either ")
		      (intersperse (mapcar (curry-after #'humanize-typespec :adjectives (when adjectives
											      t))
					   (remove 'null args))
				   ", " :last (if (eql (length args) 2) " or " ", or "))))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'and)) args &key adjectives)
  (flet ((typespecs-to-adjectives (typespecs)
	   (apply #'concatenate 'string
		  (intersperse (mapcar #'humanize-typespec
				       (remove 'null typespecs)) ", "))))
    (case (length args)
      (0 "anything you want")
      (1 (humanize-typespec (car args)))
      (otherwise (let ((result (loop for i in args
				     with predicates
				  if (eql (car-safe i) 'satisfies)
				    do (push-end i predicates)
				  else if (typespec-accepts-adjectives-p i)
				         collect (humanize-typespec
					          i :adjectives (if predicates
							            (typespecs-to-adjectives predicates)
							            (when adjectives
								      t)))
				           into results
				       else
				         append (mapcar #'humanize-typespec predicates) into results and
				         collect (humanize-typespec i) into results
				       end
				       and do (setf predicates nil)
				  end
				  finally (return (append results 
							  (mapcar #'humanize-typespec predicates))))))
		   (apply #'concatenate 'string
			  (when adjectives
			      (format nil "~A " adjectives))
			  (intersperse result ", " :last (if (eql (length result) 2) " and " ", and "))))))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'eql)) args &key adjectives)
  (assert (null adjectives))
  (format nil "equal to ~A" (car args)))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'not)) args &key adjectives)
  (assert (null adjectives))
  (format nil "not ~A" (humanize-typespec (car args))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'mod)) args &key adjectives)
  (concatenate 'string (if adjectives
			   (articlize adjectives)
			   "a ")
	       (when adjectives ", ")
	       (format nil "non-negative integer less than ~A" (car args))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'member)) args &key adjectives)
  (assert (null adjectives))
  (case (length args)
    (0 (error "~A is an impossible type." (cons typespec args)))
    (1 (humanize-name (format nil "~A" (car args))))
    (otherwise (apply #'concatenate 'string "either "
		      (intersperse (mapcar (compose #'humanize-name (curry #'format nil "~A"))
					   (remove 'null args))
				   ", " :last (if (eql (length args) 2) " or " ", or "))))))

(defmethod humanize-compound-typespec-aux ((typespec (eql 'values)) args &key adjectives)
  (error "Not Implemented!"))

(defmethod humanize-compound-typespec-aux (typespec args &key adjectives)
  ;; by default, just output the car
  (humanize-typespec typespec))

