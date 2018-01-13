(defpackage #:weblocks.t.weblocks
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.weblocks)

(plan 1)

(subtest "with-javascript-1"
    (is
      (weblocks.html:with-html-string
        (with-javascript
          "foo~A" "bar"))
      (with-javascript-to-string "foo~A" "bar")
      
      "Macro with-javascript should write into weblocks.html:*stream*"))


(finalize)
