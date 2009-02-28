(in-package :weblocks)

(export '(uri-tokens all remaining consumed
	  peek-at-token tokens-fully-consumed-p get-tokens consume-tokens
	  *uri-tokens*))

(defvar *uri-tokens* "Bound to an uri-tokens object in a request.")

(defclass uri-tokens ()
  ((all-tokens :accessor all
	       :initarg :all-tokens
	       :documentation "The full URI path split into tokens.")
   (remaining-tokens :accessor remaining
		     :documentation "A list of tokens that haven't been
		     consumed yet.")
   (consumed-tokens :accessor consumed
		    :initform nil
		    :documentation "A list of tokens that have already
		    been consumed."))
  (:documentation "An object representing an URI split into tokens (path
  components). Maintains state of the URI as it is being consumed by the
  widgets to update a widget tree."))

(defmethod initialize-instance :after ((tokens uri-tokens) &rest args)
  (declare (ignore args))
  (setf (remaining tokens) (all tokens)))

(defgeneric peek-at-token (tokens)
  (:method ((tokens uri-tokens))
    (first (remaining tokens))))

(defgeneric tokens-fully-consumed-p (tokens)
  (:method ((tokens uri-tokens))
    (not (remaining tokens))))

(defgeneric get-tokens (tokens &optional how-many)
  (:method ((tokens uri-tokens) &optional (how-many 1))
    (unless (tokens-fully-consumed-p tokens)
      (let ((current-tokens (firstn how-many (remaining tokens))))
	(setf (consumed tokens) (append (consumed tokens) current-tokens))
	(setf (remaining tokens) (safe-subseq (remaining tokens) how-many))
	current-tokens))))

(defgeneric consume-tokens (tokens token-list)
  (:method ((tokens uri-tokens) token-list)
    (setf (consumed tokens) (append (consumed tokens) token-list))
    token-list))

