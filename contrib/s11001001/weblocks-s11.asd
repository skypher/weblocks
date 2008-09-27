;;; weblocks-s11.asd: Another ASDF system definition.

(in-package #:cl-user)

(asdf:defsystem "weblocks-s11"
  :description "Stephen Compall's extensions to Weblocks."
  :version "0.1"
  :author "Stephen Compall <scompall@nocandysw.com>"
  :licence "LLGPL"
  :depends-on ("arnesi" "weblocks")
  :components ((:file "package")
	       (:file "dataedit" :depends-on ("package"))
	       (:file "persist-children" :depends-on ("package"))
	       (:file "presentations" :depends-on ("package"))))

;;; weblocks-s11.asd ends here
