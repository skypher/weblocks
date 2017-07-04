(defpackage #:weblocks.test.dependencies
  (:use #:cl
        #:cl-mock
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


(subtest "Remote dependency can be served from local cache directory."

  (let* ((original-url "http://ya.ru/")
         (dependency (make-remote-js-dependency
                      original-url)))
    
    (let* ((random-string (cl-fad::generate-random-string))
           (*cache-remote-dependencies-in*
             (pathname (format nil "/tmp/weblocks-cache/unittests/~A/"
                               random-string)))
           (resulting-path (get-path dependency))
           (resulting-url (get-url dependency)))
      
      (unwind-protect
           (progn (is (pathname-directory resulting-path)
                      `(:absolute "tmp" "weblocks-cache" "unittests" ,random-string)
                      "When *cache-remote-dependencies-in* is set to a path, get-path should return local path.")

                  (is resulting-url
                      (concatenate 'string
                                   "/remote-deps-cache/"
                                   (pathname-name resulting-path))
                      "And URL should be local too.")

                  (with-mocks ()
                    (flexi-streams:with-input-from-sequence (remote-file
                                                             (babel:string-to-octets "File content"))
                      ;; dexador should return a stream
                      (answer dex:get remote-file)

                      (let ((result (serve dependency)))
                        (is (pathname-directory result)
                            `(:absolute "tmp" "weblocks-cache" "unittests" ,random-string)
                            "Call to (serve) should return a path to a cached file.")

                        (is (length (invocations 'dex:get))
                            1
                            "First call should \"download\" content.")

                        (is (not (null (cl-fad:file-exists-p result)))
                            t
                            "File should exists in a cache directory.")))

                    ;; Now call it second time
                    (serve dependency)

                    ;; And check if it was downloaded again
                    (is (length (invocations 'dex:get))
                        1
                        "Dependency shouldn't fetch file from the internet again.")))
        ;; Clean up tmp directory
        (cl-fad:delete-directory-and-files *cache-remote-dependencies-in*)))
    

    ;; Now check with *cache-remote-dependencies-in* set to nil
    
    (is-condition (get-path dependency)
                  error
                  "When *cache-remote-dependencies-in* is nil, get-path should raise a condition.")

    (is (get-url dependency)
        original-url
        "And URL should point to remote resource.")))


