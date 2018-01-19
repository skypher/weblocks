(defpackage #:weblocks/t/weblocks
  (:use #:cl
        #:rove
        #:weblocks/t/utils))
(in-package weblocks/t/weblocks)


(deftest with-javascript-1
  (ok
   (equal (weblocks.html:with-html-string
            (weblocks::with-javascript
              "foo~A" "bar"))
          (weblocks::with-javascript-to-string "foo~A" "bar"))
      
   "Macro with-javascript should write into weblocks.html:*stream*"))

