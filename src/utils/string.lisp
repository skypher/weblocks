
(in-package :weblocks)

(wexport '(humanize-name
           attributize-name
           string-whitespace-p
           string-remove-left
           string-remove-right
	   string-invert-case
           remove-spurious-slashes)
         '(t util))

(defgeneric humanize-name (name)
  (:documentation "Convert objects to a human-readable string suitable
for presentation. Default implementations beautify strings and
symbols.

Ex:
\(humanize-name 'hello-world) => \"Hello World\"
\(humanize-name \"HELLO-WORLD\") => \"Hello World\"")
  (:method ((name string))
    (string-capitalize (substitute #\Space #\- name)))
  (:method ((name symbol))
    (humanize-name (string-downcase (symbol-name name)))))

(defgeneric attributize-name (name)
  (:documentation "Convert objects to a format suitable for
serialization (in particular for markup languages like HTML).

Ex:
\(attributize-name 'hello-world) => \"hello-world\"")
  (:method ((name null))
    "")
  (:method ((name string))
    (string-downcase (substitute #\- #\Space name)))
  (:method ((name symbol))
    (attributize-name (symbol-name name)))
  (:method ((name integer))
    (format nil "~A" name)))

(defun unattributized-name (name &optional (type-marker 'uname))
  "Call `attributize-name' with NAME and modify the result to be a
variant that is still usable in all cases where `attributize-name'
results can be used, but is extremely unlikely to ever be returned by
`attributize-name'.

TYPE-MARKER is just a label for human discernment of the different
users of this function."
  (format nil "~:@(~A~)-~A" type-marker (attributize-name name)))

(defun string-whitespace-p (str)
  "Returns true if every character in a string is a whitespace
character, nil otherwise."
  (loop for c across str
     always (whitespacep c)))

(defun string-remove-left (str prefix &key ignore-case-p)
  "If string 'str' starts with 'prefix', remove 'prefix' from the
start of 'str'."
  (when (string-starts-with str prefix
			    :test (if ignore-case-p #'char-equal #'char=))
    (subseq str (length prefix))))

(defun string-remove-right (str suffix &key ignore-case-p)
  "If string 'str' ends with 'suffix', remove 'suffix' from the end of
'str'."
  (when (string-ends-with str suffix
			  :test (if ignore-case-p #'char-equal #'char=))
    (subseq str 0 (- (length str)
		     (length suffix)))))

(defun string-invert-case (str)
  (map 'string (lambda (char)
		 (if (upper-case-p char)
		     (char-downcase char)
		     (char-upcase char)))
       (etypecase str
	 (string str)
	 (symbol (symbol-name str)))))

;;; working with those pesky slashes
(defun remove-spurious-slashes (str)
  "Condense multiple consecutively occuring slashes in STR
into a single slash.

ex:

(remove-spurious-slashes \"/ab/////c///\")
=> \"/ab/c/\""
  (with-output-to-string (s)
    (loop for c across str
          with last-char
          unless (and (eql c #\/) (eql last-char #\/))
            do (princ c s)
          do (setf last-char c))
    s))

(defun strip-trailing-slashes (str)
  "Relentlessly strip all trailing slashes from STR.
A string solely consisting of slashes is no special case.

Returns the number of stripped slashes as second value."
  ;; we just count them and return an appropriate subsequence
  (let ((n 0))
    (loop for c across (reverse str)
          while (eql c #\/)
          do (incf n))
    (values (subseq str 0 (- (length str) n)) n)))

(defun maybe-add-trailing-slash (s)
  "Supply a trailing slash if needed."
  (typecase s
    (pathname (fad:pathname-as-directory s))
    (otherwise
       (if (equal (subseq s (1- (length s))) "/")
	   s
	   (concatenate 'string s "/")))))

