
(in-package :weblocks)

(export '(pluralize singularize proper-number-form vowelp consonantp
          proper-indefinite-article articlize *current-locale* current-locale 
          russian-proper-number-form noun-vocative-to-genitive *debug-words-forms* *debug-words-genders* determine-gender))

(defvar *current-locale* :en)
(defun current-locale ()
  *current-locale*)

(defmethod locale-number-forms ((locale (eql :en)))
  (list :one :many))

(defmethod locale-number-forms ((locale (eql :ru)))
  (list :one :few :many))

(defmethod pluralize-with-locale ((locale (eql :en)) word)
  (cond
    ((string-equal word "is") "are")
    ((string-ends-with word "s") (concatenate 'string word "es"))
    ((string-ends-with word "y") (concatenate 'string (substring word 0 (1- (length word))) "ies"))
    (t (concatenate 'string word "s"))))

(defmethod pluralize-with-locale :around ((locale (eql :en)) word)
  (cond 
    ((string= word  "There is ~S validation error:")
     "There are ~S validation errors:")
    (t (call-next-method))))

(defmethod pluralize-with-locale ((locale (eql :ru)) word)
  (translate word :plural t))

(defun pluralize (word &optional (locale (current-locale)))
  "Pluralizes word. Converts 'thing' to 'things'."
  (pluralize-with-locale locale word))

(defmethod singularize-with-locale ((locale (eql :en)) word)
  (if (string-equal word "are")
    (return-from singularize-with-locale "is"))
  (cond
    ((string-ends-with word "ies") (concatenate 'string (substring word 0 (- (length word) 3)) "y"))
    ((string-ends-with word "es") (substring word 0 (- (length word) 2)))
    ((string-ends-with word "s") (substring word 0 (- (length word) 1)))
    (t word)))

(defun singularize (word &optional (locale (current-locale)))
  "Singularizes word. Converts 'things' to 'thing'."
  (singularize-with-locale locale word))

(defun english-number-form-type (number)
  "Returns :one or :many depending on number. Returns number if it is a keyword."

  (if (keywordp number) 
    (return-from english-number-form-type number))

  (if (= number 1)
    :one 
    :many))

(defmethod proper-number-form-with-locale ((locale (eql :en)) number singular-word)
  "Turns 'singular-word' into proper number form. If 'number' is 1,
   leaves 'singular-word' as is, otherwise pluralizes it."
  (if (equal :one (english-number-form-type number))
    (translate singular-word)
    (pluralize singular-word locale)))

(defun russian-number-form-type (number)
  "Returns :one :few or :many depending on number. Returns number if it is a keyword."

  (if (keywordp number)
    (return-from russian-number-form-type number))

  (let* ((n (mod (abs number) 100))
         (n1 (mod n 10)))
    (cond 
      ((and (> n 10) (< n 20)) :many)
      ((and (> n1 1) (< n1 5)) :few)
      ((= n1 1) :one)
      (t :many))))

(defun russian-proper-number-form (number single few many)
  (case (russian-number-form-type number)
    (:one single)
    (:few few)
    (:many many)))

(defmethod number-form-type-with-locale ((locale (eql :ru)) number)
  (russian-number-form-type number))

(defmethod number-form-type-with-locale ((locale (eql :en)) number)
  (english-number-form-type number))

(defmethod proper-number-form-with-locale ((locale (eql :ru)) number singular-word)
  (translate singular-word 
             :items-count (russian-number-form-type number)))

(defun proper-number-form (number singular-word &optional (locale (current-locale)))
  (proper-number-form-with-locale locale number singular-word))

(defun vowelp (char)
  "Checks if a character is a vowel."
  (when (member (char-downcase char)
                '(#\a #\e #\i #\o #\u))
    t))

(defun consonantp (char)
  "Checks if a character is a consonant."
  (when (member (char-downcase char)
                '(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t #\v #\w #\x #\y #\z))
    t))

(defun proper-indefinite-article (word)
  "Returns a proper indefinite article for a word. Returns 'an' for
'apple' and 'a' for 'table'."
  (if (and (vowelp (aref word 0))
           (not (eq (char-downcase (aref word 0)) #\u)))
      "an"
      "a"))

(defun articlize (word)
  "Prepends a word with a proper indefinite article. Returns 'an
apple' for 'apple' and 'a table' for 'table'."
  (concatenate 'string (proper-indefinite-article word) " " word))

(defvar *debug-words-forms* nil)

(defmethod noun-vocative-to-genitive ((obj string))
  "Needs to be overriden for custom needs"
  (if *debug-words-forms*
    (format nil "... genitive form of ~A ..." obj)
    obj))

(defmethod noun-vocative-to-accusative ((obj string))
  "Needs to be overriden for custom needs"
  (if *debug-words-forms*
    (format nil "... accusative form of ~A ..." obj)
    obj))

(defvar *debug-words-genders* nil)

(defmethod determine-gender ((obj string))
  (if *debug-words-genders* 
    (error "Cannot determine gender of ~A" obj)
    :masculine))

(defun default-translation-function (string &key plural-p genitive-form-p items-count accusative-form-p &allow-other-keys)
  (declare (ignore args))

  (when plural-p  
    (setf string (pluralize string)))

  (when genitive-form-p 
    (setf string (noun-vocative-to-genitive string)))

  (when accusative-form-p 
    (setf string (noun-vocative-to-accusative string)))

  (when items-count
    (setf string (proper-number-form items-count string)))

  string)
