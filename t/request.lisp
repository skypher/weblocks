(defpackage #:weblocks.t.request
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.request)


(plan 1)


(subtest "refresh-request-p-1"
  (with-session
    (with-request ("/foo/bar")
      (weblocks.session:set-value 'weblocks::last-request-uri
                                  '("foo" "bar"))
      (is (weblocks.request:refresh-request-p)
          t))))


(finalize)
