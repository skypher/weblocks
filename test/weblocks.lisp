
(in-package :weblocks-test)

;;; test defwebapp
(deftest defwebapp-1
    (let (weblocks::*webapp-name* weblocks::*application-public-dependencies*)
      (declare (special weblocks::*webapp-name* weblocks::*application-public-dependencies*))
      (defwebapp 'hello :public-dependencies '("a" "b" "c"))
      (values weblocks::*webapp-name*
	      weblocks::*application-public-dependencies*))
  hello ("a" "b" "c"))

(deftest defwebapp-2
    (let (weblocks::*webapp-name* weblocks::*application-public-dependencies*)
      (declare (special weblocks::*webapp-name* weblocks::*application-public-dependencies*))
      (defwebapp 'hello)
      (values weblocks::*webapp-name*
	      (format nil "~A" weblocks::*application-public-dependencies*)))
  hello "(stylesheets/layout.css stylesheets/main.css)")

;;; test public-file-relative-path
(deftest public-file-relative-path-1
    (format nil "~A" (public-file-relative-path :stylesheet "foo"))
  "stylesheets/foo.css")

(deftest public-file-relative-path-2
    (format nil "~A" (public-file-relative-path :script "bar"))
  "scripts/bar.js")

;;; test public-files-relative-paths
(deftest public-files-relative-paths-1
    (format nil "~A" (public-files-relative-paths
		      '(:stylesheet . "foo")
		      '(:script . "bar")))
  "(stylesheets/foo.css scripts/bar.js)")

