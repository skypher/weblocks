
(in-package :weblocks-test)

(deftestsuite .weblocks-suite (weblocks-suite)
  ())

(addtest wexport
  (labels ((pkg-name (suffix)
	     (format nil "~A~A" (symbol-name '#:weblocks-temp-) suffix))
	   (used? (suffix)
	     (find-package (pkg-name suffix)))
	   (sym-exported? (pkg sym)
	     (eq :external (nth-value 1 (find-symbol (symbol-name sym) pkg))))
	   (wexport (&rest args)
	     (apply #'weblocks::wexport args)))
    (let* ((pkg (make-package (pkg-name (loop for n from 0 to 420
					      unless (used? n)
						return n))))
	   (suffix (subseq (package-name pkg) 9)))
      (unwind-protect
	   (progn 
	     (wexport 'a1 suffix)
	     (ensure (sym-exported? pkg 'a1))
	     (wexport '(b2 c3) (list (make-symbol suffix)))
	     (ensure (sym-exported? pkg 'b2))
	     (ensure (sym-exported? pkg 'c3)))
	(delete-package pkg)))))

;;; testing root-composite
(deftest root-composite-1
    (with-request :get nil
      (root-composite))
  nil nil)

(deftest root-composite-2
    (with-request :get nil
      (setf (root-composite) 'foobar)
      (multiple-value-bind (res present-p)
	  (root-composite)
	(values res (not (null present-p)))))
  foobar t)

(addtest with-javascript-1
    (set-sensible-suite)
    (ensure-same
      (with-output-to-string (*weblocks-output-stream*)
        (with-javascript
          "foo~A" "bar"))
      (with-javascript-to-string "foo~A" "bar")))

