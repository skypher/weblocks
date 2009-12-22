(in-package :weblocks-test)

(deftestsuite bundling-suite (weblocks-suite print-upcase-suite)
  ()
  (:setup 
    (cl-fad:delete-directory-and-files *temp-bundles-folder*
                                       :if-does-not-exist :ignore)))

(defparameter *temp-bundles-folder* (princ-to-string (compute-public-files-path "weblocks" "test/temp-bundles")))

(addtest merge-files-with-newline
  (let ((path1 (merge-pathnames "test1" *temp-bundles-folder*))
	(path2 (merge-pathnames "test2" *temp-bundles-folder*))
	(path3 (merge-pathnames "test3" *temp-bundles-folder*)))
    (weblocks::with-file-write (stream path1)
      (write-string "test1" stream))
    (weblocks::with-file-write (stream path2)
      (write-string "test2" stream))
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
    (push (make-instance 'stylesheet-dependency :url #U"http://example.com/external.css")
	  test-deps)
    (push (make-instance 'script-dependency :url #U"http://example.com/external.js")
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

(defmacro with-bundle-setup (&body body)
  `(let* ((test-deps (make-test-dependencies-1))
          (temp-bundles (make-temp-bundles test-deps)))
     ,@body))

(addtest bundling.uri
  (with-bundle-setup
    (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
                                      temp-bundles))
                 (values "/pub/bundles/2.js" "/pub/bundles/1.css"
                         "/external.js" "/external.css"))))

(addtest bundling.local-path
  (with-bundle-setup
    (ensure-same (values-list (mapcar #'weblocks::local-path temp-bundles))
		 (values (merge-pathnames "2.js" *temp-bundles-folder*)
			 (merge-pathnames "1.css" *temp-bundles-folder*)
			 nil nil))))

(defmacro with-tally-setup (&body body)
  `(with-bundle-setup
     (let ((tally (weblocks::get-bundle-tally :bundle-folder *temp-bundles-folder*)))
       (destructuring-bind ((js-merged . js-parts) (css-merged . css-parts))
           (weblocks::composition-list tally)
         ,@body))))


(addtest bundling.tally.composition-list
  (with-tally-setup
    (ensure-same js-merged "2.js")
    (ensure-same css-merged "1.css")
    (ensure-same (values-list (append js-parts css-parts))
                 (values '("weblocks-debug" "js")
                         '("sound" "js")
                         '("datagrid-import" "css")
                         '("isearch" "css")
                         '("datagrid" "css"))
                 :test (lambda (x y)
                         (cl-ppcre:scan (subseq (apply #'make-versioned-regex y) 1)
                                        (puri:uri-path (puri:uri x)))))))

(addtest bundling.tally.merged-file
  (with-tally-setup
    (ensure-same (values js-parts css-parts)
                 (values js-merged css-merged)
                 :test (lambda (x y)
                         (merged-with-newline-equal x
                                                    (merge-pathnames y (weblocks::bundle-folder tally)))))))

(addtest bundling.tally.import-rule
  (with-tally-setup
    (ensure-same (cl-ppcre:scan (list :sequence (weblocks::slurp-file (car css-parts)))
                                (weblocks::slurp-file (merge-pathnames css-merged
                                                                       (weblocks::bundle-folder tally))))
                 0)))

(addtest bundling.tally.bundle-different.uri-test
  (with-tally-setup
    (setf test-deps (make-test-dependencies-2))
    (setf temp-bundles (make-temp-bundles test-deps))
    ;; uri test
    (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
				      temp-bundles))
		 (values "/pub/bundles/4.js" "/pub/bundles/3.css"))))

(addtest bundling.tally.bundle-different.local-path
  (with-tally-setup
    (setf test-deps (make-test-dependencies-2))
    (setf temp-bundles (make-temp-bundles test-deps))
    (ensure-same (values-list (mapcar #'weblocks::local-path temp-bundles))
		 (values (merge-pathnames "4.js" *temp-bundles-folder*)
			 (merge-pathnames "3.css" *temp-bundles-folder*)))))

(addtest bundling.tally.bundle-different.composition-list
  (with-tally-setup
    (setf test-deps (make-test-dependencies-2))
    (setf temp-bundles (make-temp-bundles test-deps))
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
		   :test (lambda (x y)
                           (cl-ppcre:scan (subseq (apply #'make-versioned-regex y) 1)
                                          (puri:uri-path (puri:uri x))))))))

(addtest bundling.tally.bundle-different.no-css-bundling.uri-test
  (with-tally-setup
    (setf test-deps (make-test-dependencies-3))
    (setf temp-bundles (make-temp-bundles test-deps))
    (ensure-same (puri:uri-path (dependency-url (car temp-bundles)))
		 "/pub/bundles/3.js")))


(addtest bundling.tally.bundle-different.no-js-bundling.uri-test
  (with-tally-setup
    (setf test-deps (make-test-dependencies-4))
    (setf temp-bundles (make-temp-bundles test-deps))
    (ensure-same (puri:uri-path (dependency-url (car temp-bundles)))
		 "/pub/bundles/3.css")))

