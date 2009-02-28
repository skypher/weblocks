
(in-package :weblocks)

(export '(pluralize singularize proper-number-form vowelp consonantp
	  proper-indefinite-article articlize))

(defun pluralize (word)
  "Pluralizes word. Converts 'thing' to 'things'."
  (cond
    ((string-equal word "is") "are")
    ((string-ends-with word "s") (concatenate 'string word "es"))
    ((string-ends-with word "y") (concatenate 'string (substring word 0 (1- (length word))) "ies"))
    (t (concatenate 'string word "s"))))

(defun singularize (word)
  "Singularizes word. Converts 'things' to 'thing'."
  (if (string-equal word "are")
      (return-from singularize "is"))
  (cond
    ((string-ends-with word "ies") (concatenate 'string (substring word 0 (- (length word) 3)) "y"))
    ((string-ends-with word "es") (substring word 0 (- (length word) 2)))
    ((string-ends-with word "s") (substring word 0 (- (length word) 1)))
    (t word)))

(defun proper-number-form (number singular-word)
  "Turns 'singular-word' into proper number form. If 'number' is 1,
leaves 'singular-word' as is, otherwise pluralizes it."
  (if (= number 1)
      singular-word
      (pluralize singular-word)))

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
