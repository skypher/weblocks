;;; weblocks-yarek.asd: Another ASDF system definition.

(in-package #:cl-user)

(asdf:defsystem "weblocks-yarek"
  :description "Yarek Kowalik's extensions to Weblocks."
  :version "0.1"
  :author "Yarek Kowalik <yarek.kowalik@gmail.com>"
  :licence "LLGPL"
  :depends-on ("weblocks")
  :components ((:file "package")
	       (:module widgets
                :components ((:file "popover-gridedit"))
                :depends-on ("package"))))

;;; weblocks-yarek.asd ends here
