
(in-package :weblocks-test)

;;; test defwebapp
(deftest defwebapp-1
    (let (weblocks::*webapp-name*
	  weblocks::*application-public-dependencies*
	  *webapp-description*)
      (declare (special weblocks::*webapp-name*
			weblocks::*application-public-dependencies*
			*webapp-description*))
      (defwebapp 'hello :description "foo bar")
      (values weblocks::*webapp-name*
	      (mapcar (curry #'format nil "~A") weblocks::*application-public-dependencies*)
	      *webapp-description*))
  hello
  ("stylesheets/layout.css"
   "stylesheets/main.css"
   "scripts/prototype.js"
   "scripts/weblocks.js"
   "scripts/scriptaculous.js")
  "foo bar")

(deftest defwebapp-2
    (let (weblocks::*webapp-name*
	  weblocks::*application-public-dependencies*
	  (*webapp-description* "foo bar"))
      (declare (special weblocks::*webapp-name*
			weblocks::*application-public-dependencies*
			*webapp-description*))
      (defwebapp 'hello)
      *webapp-description*)
  "foo bar")

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

