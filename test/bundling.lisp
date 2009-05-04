(in-package :weblocks-test)

(deftestsuite bundling-suite (weblocks-suite print-upcase-suite)
  ())

(defparameter *temp-bundles-folder* (princ-to-string (compute-public-files-path "weblocks" "test/temp-bundles")))

(addtest merge-files-with-newline
  (cl-fad:delete-directory-and-files *temp-bundles-folder* :if-does-not-exist :ignore)
  (let ((path1 (merge-pathnames "test1" *temp-bundles-folder*))
	(path2 (merge-pathnames "test2" *temp-bundles-folder*))
	(path3 (merge-pathnames "test3" *temp-bundles-folder*)))
    (weblocks::with-file-write (stream path1)
      (write-stream "test1" stream))
    (weblocks::with-file-write (stream path2)
      (write-stream "test2" stream))
    (weblocks::merge-files-with-newline (list path1 path2) path3)
    (ensure-same (weblocks::slurp-file path3)
		 "test1
test2")))

(defun make-test-dependencies-1 ()
  (let ((test-deps (mapcar (lambda (x) (apply #'make-local-dependency x))
			   '((:stylesheet "isearch")
			     (:script "weblocks-debug")
			     (:stylesheet "datagrid-import" :import-p t)
			     (:stylesheet "datagrid")
			     (:script "sound")))))
    (push (make-instance 'stylesheet-dependency :url #P"http://external.css")
	  test-deps)
    (push (make-instance 'script-dependency :url #P"http://external.js")
	  test-deps)
    test-deps))

(defun make-test-dependencies-2 ()
  (mapcar (lambda (x) (apply #'make-local-dependency x))
	  '((:stylesheet "suggest")
	    (:script "dialog")
	    (:stylesheet "isearch")
	    (:script "sound"))))

(defun make-test-dependencies-3 ()
  (mapcar (lambda (x) (apply #'make-local-dependency x))
	  '((:script "sound")
	    (:script "dialog"))))

(defun make-test-dependencies-4 ()
  (mapcar (lambda (x) (apply #'make-local-dependency x))
	  '((:stylesheet "isearch")
	    (:stylesheet "suggest"))))

(defun make-temp-bundles (dependencies)
  (weblocks::bundle-dependencies dependencies
				 :bundle-folder *temp-bundles-folder*
				 :bundle-types '(:stylesheet :script)))

(defun merged-with-newline-equal (part-paths merged-path)
  (let ((result (weblocks::slurp-file merged-path)))
    (dolist (path part-paths)
      (setf result (cl-ppcre:regex-replace (list :sequence (weblocks::slurp-file path))
					   result "")))
    (and (= (length result)
	    (1- (length part-paths)))
	 (zerop (length (remove #\Newline result))))))

(addtest bundling-test-1
  (cl-fad:delete-directory-and-files *temp-bundles-folder* :if-does-not-exist :ignore)
  (let* ((test-deps (make-test-dependencies-1))
	 (temp-bundles (make-temp-bundles test-deps)))
    ;; uri test
    (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
				      temp-bundles))
		 (values "/pub/bundles/2.js" "/pub/bundles/1.css"
			 "/external.js" "/external.css"))
    ;; local-path test
    (ensure-same (values-list (mapcar #'weblocks::local-path temp-bundles))
		 (values (concatenate 'string *temp-bundles-folder* "2.js")
			 (concatenate 'string *temp-bundles-folder* "1.css")
			 nil nil))
    (let ((tally (weblocks::get-bundle-tally :bundle-folder *temp-bundles-folder*)))
      (destructuring-bind ((js-merged . js-parts) (css-merged . css-parts))
	  (weblocks::composition-list tally)
	;; composition-list test
	(ensure-same js-merged "2.js")
	(ensure-same css-merged "1.css")
	(ensure-same (values-list (append js-parts css-parts))
		     (values '("weblocks-debug" "js")
			     '("sound" "js")
			     '("datagrid-import" "css")
			     '("isearch" "css")
			     '("datagrid" "css"))
		     :test (lambda (x y) (cl-ppcre:scan (subseq (apply #'make-versioned-regex y) 1)
							x)))
	;; merged file test
	(ensure-same (values js-parts css-parts)
		     (values js-merged css-merged)
		     :test (lambda (x y)
			     (merged-with-newline-equal x
							(merge-pathnames y (weblocks::bundle-folder tally)))))
	;; import rule first test
	(ensure-same (cl-ppcre:scan (list :sequence (weblocks::slurp-file (car css-parts)))
				    (weblocks::slurp-file (merge-pathnames css-merged
									   (weblocks::bundle-folder tally))))
		     0)))

    ;; bundling different files
    (setf test-deps (make-test-dependencies-2))
    (setf temp-bundles (make-temp-bundles test-deps))
    ;; uri test
    (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
				      temp-bundles))
		 (values "/pub/bundles/4.js" "/pub/bundles/3.css"))
    ;; local-path test
    (ensure-same (values-list (mapcar #'weblocks::local-path temp-bundles))
		 (values (concatenate 'string *temp-bundles-folder* "4.js")
			 (concatenate 'string *temp-bundles-folder* "3.css")))
    ;; composition-list test
    (destructuring-bind (js-parts css-parts)
	(loop for bundle in (weblocks::composition-list (weblocks::get-bundle-tally :bundle-folder *temp-bundles-folder*))
	   if (or (string= (car bundle) "4.js")
		  (string= (car bundle) "3.css"))
	   collect (cdr bundle))
      (ensure-same (values-list (append js-parts css-parts))
		   (values '("dialog" "js")
			   '("sound" "js")
			   '("suggest" "css")
			   '("isearch" "css"))
		   :test (lambda (x y) (cl-ppcre:scan (subseq (apply #'make-versioned-regex y) 1)
							x))))

    ;; no css bundle
    (setf test-deps (make-test-dependencies-3))
    (setf temp-bundles (make-temp-bundles test-deps))
    ;; uri test
    (ensure-same (puri:uri-path (dependency-url (car temp-bundles)))
		 "/pub/bundles/5.js")

    ;; no js bundle
    (setf test-deps (make-test-dependencies-4))
    (setf temp-bundles (make-temp-bundles test-deps))
    ;; uri test
    (ensure-same (puri:uri-path (dependency-url (car temp-bundles)))
		 "/pub/bundles/6.css")))