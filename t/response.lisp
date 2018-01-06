(defpackage #:weblocks.t.response
  (:use #:cl
        #:prove
        #:weblocks.t.utils))
(in-package weblocks.t.response)


(plan 2)

(subtest "make-uri"
  (with-session
    (with-request ("http://example.com:10050/foo/bar")
      (is (weblocks.response:make-uri "/login")
          "http://example.com:10050/login"
          "Absolute URI should substitute whole path in original URL.")

      (is (weblocks.response:make-uri "/login?code=100500")
          "http://example.com:10050/login?code=100500"
          "Target URI can contain parameters."))))


(subtest "make-uri relative"
  (with-session
    (with-request ("http://example.com:10050/foo/bar")
      (is (weblocks.response:make-uri "minor")
          "http://example.com:10050/foo/minor"
          "Relative path substitutes top level path component if it is not ended with slash")))

  (with-session
    (with-request ("http://example.com:10050/foo/bar/")
      (is (weblocks.response:make-uri "minor")
          "http://example.com:10050/foo/bar/minor"
          "Relative path added to the base path if it is ended with slash")))

  (with-session
    (with-request ("http://example.com:10050/foo/bar/")
      (is (weblocks.response:make-uri "../minor")
          "http://example.com:10050/foo/minor"
          "Two dots can be used to go to the upper level of the path"))))

(finalize)
