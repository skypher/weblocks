(in-package :weblocks)

(export '(uri-tokens
          all-tokens
          remaining-tokens
          consumed-tokens
          peek-at-token
          tokens-fully-consumed-p
          pop-tokens
          pop-token
          consume-tokens
          uri-tokens-to-string
          uri-tokens-start-with
          *uri-tokens*))

(defvar *uri-tokens*)
(setf (documentation '*uri-tokens* 'variable)
      "Bound to an URI-TOKENS object in a request.")

(defclass uri-tokens ()
  ((remaining-tokens :accessor remaining-tokens
		     :documentation "A list of tokens that haven't been
		     consumed yet.")
   (consumed-tokens :accessor consumed-tokens
		    :initform nil
		    :documentation "A list of tokens that have already
		    been consumed."))
  (:documentation "An object representing an URI split into tokens (path
  components). Maintains state of the URI as it is being consumed by the
  widgets to update a widget tree.

  Use the :TOKENS special initarg to initialize the initial list of tokens.

  Use the function ALL-TOKENS to access the whole list of tokens."))

(defmethod initialize-instance :around ((obj uri-tokens) &rest initargs
                                                         &key tokens &allow-other-keys)
  (assert (listp tokens))
  (remf initargs :tokens)
  (setf (remaining-tokens obj) tokens)
  (apply #'call-next-method obj initargs))

(defmethod print-object ((obj uri-tokens) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (remaining-tokens obj))))

(defgeneric all-tokens (tokens)
  (:method ((tokens list)) tokens)
  (:method ((tokens uri-tokens))
    (append (consumed-tokens tokens) (remaining-tokens tokens))))

(defgeneric peek-at-token (tokens)
  (:documentation "Return the first remaining token without consuming it.")
  (:method ((tokens uri-tokens))
    (first (remaining-tokens tokens))))

(defgeneric tokens-fully-consumed-p (tokens)
  (:method ((tokens uri-tokens))
    (not (remaining-tokens tokens))))

(defgeneric pop-tokens (tokens &optional how-many)
  (:documentation "Consume HOW-MANY of the remaining tokens.")
  (:method ((tokens uri-tokens) &optional (how-many 1))
    (unless (tokens-fully-consumed-p tokens)
      (let ((current-tokens (firstn how-many (remaining-tokens tokens))))
        (symbol-macrolet ((remaining-tokens (remaining-tokens tokens))
                          (consumed-tokens (consumed-tokens tokens)))
          (setf consumed-tokens (append consumed-tokens current-tokens))
          (setf remaining-tokens (safe-subseq remaining-tokens how-many)))
        current-tokens))))

(defun pop-token (tokens)
  "Convenience function -- pop exactly one token to be returned as an atom."
  (car (pop-tokens tokens 1)))

(defgeneric consume-tokens (tokens token-list)
  (:method ((tokens uri-tokens) token-list)
    (setf (consumed-tokens tokens) (append (consumed-tokens tokens) token-list))
    token-list))

(defun uri-tokens-to-string (tokens)
  "Encodes and concatenates uri tokens into a url string. Note that
the string will not contain separator slashes at the beginning or
end."
  (string-downcase ; XXX uh, should we really?
    (apply #'concatenate 'string
           (intersperse
             (mapcar #'url-encode (ensure-list tokens)) "/"))))

(defgeneric uri-tokens-start-with (uri-tokens match-tokens)
  (:documentation "Returns true if URI-TOKENS start with MATCH-TOKENS.")
  (:method ((tokens list) match-tokens)
    (or (and match-tokens
             (list-starts-with tokens match-tokens :test #'string=))
        (and (null match-tokens)
             (null tokens)))))

