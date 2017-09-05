(defpackage #:weblocks.test.dependencies
  (:use #:cl
        #:cl-mock
        #:weblocks.dependencies
        #:prove
        #:hamcrest.matchers
        #:weblocks.t.utils))
(in-package weblocks.test.dependencies)


(plan 8)


(subtest "Infer type"
  (is (infer-type-from "/some/path/to.css")
      :css
      "Inferring type from path to css file")

  (is (infer-type-from "/some/path/to.js")
      :js
      "Inferring type from path to js file")

  (is-error (infer-type-from "/some/path/unknown")
            error
            "Inferring type from path to unknown file should raise an error")

  (is (infer-type-from "http://example.com/some/file.css")
      :css
      "Inferring type from url to css file")

  (is (infer-type-from "http://example.com/some/file.js")
      :js
      "Same for javascript URL"))


(subtest "Create local javascript dependency"
  (let ((dependency (make-dependency #P"/tmp/some.js")))
    (is (get-path dependency)
        #P"/tmp/some.js"
        "Method get-path should return a local pathname.")
    (is (get-type dependency)
        :js
        "Dependency's type should be :js")
    (is (get-content-type dependency)
        "application/javascript"
        "Content type should be application/javascript")

    (let ((route (get-route dependency)))
      (isnt route nil
            "Local dependency should have a lisp webserver's route.")
      (when route
        (is (routes:route-template route)
            (list "static" "js" "some.js")
            "Route should point to a same filename in a special root for static files."))))

  (let ((dependency (make-dependency "/tmp/some.js")))
    (is (get-path dependency)
        #P"/tmp/some.js"
        "Also, local dependency can be created using a string.")))


(subtest "If *cache-remote-dependencies-in* is nil, then:."
  (let* ((*cache-remote-dependencies-in* nil)
         (original-url "http://ya.ru/some.css")
         (dependency (make-dependency
                       original-url)))
    (is-condition (get-path dependency)
                  error
                  "Method get-path should raise a condition.")

    (is (get-url dependency)
        original-url
        "And URL should point to remote resource.")))


(subtest "Remote dependency can be served from local cache directory."
  (let* ((original-url "http://ya.ru/some.css")
         (dependency (make-dependency
                       original-url))
         (random-string (cl-fad::generate-random-string))
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
      (cl-fad:delete-directory-and-files *cache-remote-dependencies-in*))))


(subtest "Render CSS dependency"
  (let ((dependency (make-dependency "some.css")))
    (is-html (render-in-head dependency)
             "<link rel='stylesheet' type='text/css' href='/static/css/some.css' media='screen' />"
             "Local CSS dependency should be rendered as a link tag."))

  (let* ((*cache-remote-dependencies-in* nil)
         (dependency (make-dependency "https://example.com/some.css")))
    (is-html (render-in-head dependency)
             "<link rel='stylesheet' type='text/css' href='https://example.com/some.css' media='screen' />"
             "Remote CSS dependency should be rendered with remote url if caching is turned off."))

  (let* ((*cache-remote-dependencies-in* "/tmp/cache/")
         (dependency (make-dependency "https://example.com/some.css")))
    (is-html (render-in-head dependency)
             "<link rel='stylesheet' type='text/css' href='/remote-deps-cache.*' media='screen' />"
             "Remote CSS dependency should be rendered with local if caching is turned on.")))


(subtest "Render JS dependency"
  (let ((dependency (make-dependency "some.js")))
    (is-html (render-in-head dependency)
             "<script src='/static/js/some.js' type='text/javascript'></script>"
             "JS dependency should be rendered as a script tag.")))


(subtest "Png dependency should not be rendered"
  (let ((dependency (make-dependency "some.png")))
    (is-html (render-in-head dependency)
             ""
             "Methods render-in-header works only for js/css dependencies.")))


(subtest "Cached remote dependency should define a route"
  (let* ((*cache-remote-dependencies-in* nil)
         (dependency (make-dependency "https://example.com/some.css")))
    (is-type (get-route dependency)
             'null
             "Non cached dependency shouldn't have a route."))
  
  (let* ((*cache-remote-dependencies-in* "/tmp/cache/")
         (dependency (make-dependency "https://example.com/some.css")))
    (isnt (get-route dependency)
          nil
          "Cached dependency should have a route.")))


(finalize)
