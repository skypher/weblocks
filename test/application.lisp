
(in-package :weblocks-test)

;;; test defwebapp
(deftest defwebapp-1
    (let (weblocks::*webapp-name*
	  weblocks::*application-dependencies*
	  *webapp-description*)
      (declare (special weblocks::*webapp-name*
			weblocks::*application-dependencies*
			*webapp-description*))
      (defwebapp 'hello :description "foo bar")
      (values weblocks::*webapp-name*
	      (mapcar (curry #'format nil "~A") (mapcar #'dependency-url weblocks::*application-dependencies*))
	      *webapp-description*))
  hello
  ("/pub/stylesheets/layout.css"
   "/pub/stylesheets/main.css"
   "/pub/stylesheets/dialog.css"
   "/pub/scripts/prototype.js"
   "/pub/scripts/scriptaculous.js"
   "/pub/scripts/shortcut.js"
   "/pub/scripts/weblocks.js"
   "/pub/scripts/dialog.js")
  "foo bar")

(deftest defwebapp-2
    (let (weblocks::*webapp-name*
	  weblocks::*application-dependencies*
	  (*webapp-description* "foo bar"))
      (declare (special weblocks::*webapp-name*
			weblocks::*application-dependencies*
			*webapp-description*))
      (defwebapp 'hello)
      *webapp-description*)
  "foo bar")

