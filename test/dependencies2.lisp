(defpackage #:weblocks.test.dependencies
  (:use #:cl
        #:weblocks.dependencies
        #:prove
        #:hamcrest.matchers))
(in-package weblocks.test.dependencies)


(subtest "Create dependency"
    (let ((dependency (make-static-js-dependency "/tmp/some.js")))
      (is (get-path dependency)
          "/tmp/some.js")))


(subtest "Dependency should have a route"
  (let* ((dependency (make-static-js-dependency "/tmp/some.js"))
         (route (get-route dependency)))
    (is (routes:route-template route)
        (list "static" "css" "some.js"))))
