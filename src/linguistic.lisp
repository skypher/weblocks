
(in-package :weblocks)

(export '(pluralize singularize proper-number-form vowelp consonantp
	  proper-indefinite-article articlize))

(defun pluralize (word)
  (if (string-equal word "is")
      (return-from pluralize "are"))
  (if (string-ends-with word "s")
      (concatenate 'string word "es")
      (concatenate 'string word "s")))

(defun singularize (word)
  (if (string-equal word "are")
      (return-from singularize "is"))
  (cond
    ((string-ends-with word "es") (substring word 0 (- (length word) 2)))
    ((string-ends-with word "s") (substring word 0 (- (length word) 1)))
    (t word)))

(defun proper-number-form (number singular-word)
  (if (= number 1)
      singular-word
      (pluralize singular-word)))

(defun vowelp (char)
  (when (member (char-downcase char)
		'(#\a #\e #\i #\o #\u))
    t))

(defun consonantp (char)
  (when (member (char-downcase char)
		'(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t #\v #\w #\x #\y #\z))
    t))

(defun proper-indefinite-article (word)
  (if (vowelp (aref word 0))
      "an"
      "a"))

(defun articlize (word)
  (concatenate 'string (proper-indefinite-article word) " " word))
