
(defpackage #:weblocks-scripts
  (:use :cl :weblocks :tinaa :rt)
  (:export #:document-weblocks))

(in-package :weblocks-scripts)

(defun document-weblocks ()
  (document-system 'package :weblocks "../docs/gen/" :show-parts-without-documentation? t))


