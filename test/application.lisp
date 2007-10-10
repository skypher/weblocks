
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
   "scripts/scriptaculous.js"
   "scripts/shortcut.js"
   "scripts/weblocks.js")
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

