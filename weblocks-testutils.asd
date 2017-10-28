(defpackage #:weblocks-testutils-asd
  (:use :cl :asdf))
(in-package :weblocks-testutils-asd)


(asdf:defsystem weblocks-testutils
  :version      "0.1.0"
  :description  "description"
  :author       "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :serial       t
  :license      "LLGPL"
  :components   ((:module t
                  :components ((:file "utils"))))
  :depends-on   (:weblocks
                 :prove
                 :lack-test))
