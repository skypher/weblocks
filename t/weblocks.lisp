(defpackage #:weblocks.t.weblocks
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.weblocks)

(plan 3)

(subtest "root-composite-1"
  (with-session
    (is-values (weblocks::root-composite)
               (list nil nil)
               "root-composite by default returns two nil values")))


(subtest "root-composite-2"
  (with-session
    (setf (weblocks::root-composite) 'foobar)
    (is-values (weblocks::root-composite)
               (list 'foobar t)
               "setf for root-composite should put any value to the session hash")))


(subtest "with-javascript-1"
    (is
      (with-output-to-string (*weblocks-output-stream*)
        (with-javascript
          "foo~A" "bar"))
      (with-javascript-to-string "foo~A" "bar")
      
      "Macro with-javascript should write into *weblocks-output-stream*"))


(finalize)
