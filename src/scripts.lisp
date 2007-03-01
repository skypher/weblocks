
(in-package :weblocks)

(defun document-weblocks ()
  (tinaa:document-system 'package :weblocks "../docs/gen/" :show-parts-without-documentation? t))
