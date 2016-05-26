(in-package :weblocks-test)

(deftestsuite dependencies-suite (weblocks-suite)
  ())

(addtest dependencies-by-symbol
  (ensure-same (remove nil (weblocks::dependencies-by-symbol 'non-existent-widget-name))
               nil)
  (ensure-same (values-list
                (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
                        (remove nil (weblocks::dependencies-by-symbol 'navigation))))
               (make-versioned-regex "navigation" "css")
               :test (lambda (x y) (cl-ppcre:scan y x))))



(addtest make-local-dependency
  (ensure-same (make-local-dependency :stylesheet "non-existing-file-name")
               nil))

(addtest probe-dependencies
         (ensure-same (values-list (mapcar (lambda (x) (dependency-url (apply #'make-local-dependency x)))
                                           '((:stylesheet "non-existing-file-name" :do-not-probe t)
                                             (:script "weblocks" :do-not-probe t))))
                      (values (puri:uri "/pub/stylesheets/non-existing-file-name.css")
                              (puri:uri "/pub/scripts/weblocks.js"))
                      :test puri:uri=)
         (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
                                           (list (make-local-dependency :stylesheet "main")
                                                 (make-local-dependency :script "prototype-backend/weblocks" :do-not-probe t))))
                      (values-list (mapcar (lambda (x) (apply #'make-versioned-regex x))
                                           '(("main" "css")
                                             ("weblocks" "backend-js"))))
                      :test (lambda (x y) (cl-ppcre:scan y x))))

(addtest stylesheet-dependency-compare
  (ensure-different (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
                    (make-instance 'stylesheet-dependency :url "http://boing.com/bcd")
                    :test dependencies-equalp)
  (ensure-same (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
               (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
               :test dependencies-equalp))

(addtest script-dependency-compare
  (ensure-different (make-instance 'script-dependency :url "http://boing.com/abc")
                    (make-instance 'script-dependency :url "http://boing.com/bcd")
                    :test dependencies-equalp)
  (ensure-same (make-instance 'script-dependency :url "http://boing.com/abc")
               (make-instance 'script-dependency :url "http://boing.com/abc")
               :test dependencies-equalp))



(addtest stylesheet-dependency-position
  (ensure (not (dependencies-lessp
                (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
                (make-instance 'stylesheet-dependency :url "http://boing.com/bcd")))))

(addtest stylesheet-vs-script-position
  (ensure (not (dependencies-lessp
                (make-instance 'script-dependency :url "http://boing.com/abc")
                (make-instance 'stylesheet-dependency :url "http://boing.com/bcd"))))
  (ensure (dependencies-lessp
           (make-instance 'stylesheet-dependency :url "http://boing.com/bcd")
           (make-instance 'script-dependency :url "http://boing.com/abc")))
  (ensure (not (dependencies-lessp nil nil))))

(addtest script-dependency-position
  (ensure (not (dependencies-lessp
                (make-instance 'script-dependency :url "http://boing.com/bcd")
                (make-instance 'script-dependency :url "http://boing.com/abc")))))



(addtest per-class-dependencies-1
  (ensure-null (per-class-dependencies "a string")))

(addtest per-class-dependencies-2
  (ensure-null (per-class-dependencies (lambda ()))))

(addtest per-class-dependencies-3
  (ensure-null (per-class-dependencies 'some-symbol)))

(addtest per-class-dependencies-4
  (ensure-same (values-list (remove nil (per-class-dependencies
                                         (make-instance 'navigation))))
               (values-list
                (mapcar (curry #'make-local-dependency :stylesheet)
                        '("menu" "navigation")))
               :test dependencies-equalp))



(addtest dependencies-1
  (ensure-null (dependencies 'some-symbol)))

(addtest dependencies-2
  (ensure-null (dependencies "some-string")))

(addtest dependencies-3
  (ensure-null (dependencies (lambda ()))))

(addtest dependencies-4
  (ensure-null (dependencies (make-instance 'widget :name "abc123"))))

(addtest dependencies-5
  (ensure-same (values-list (dependencies "main"))
               (make-local-dependency :stylesheet "main")
               :test dependencies-equalp))

(addtest dependencies-6
  (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
                                    (dependencies (make-instance 'navigation))))
               (values-list (mapcar (lambda (x) (apply #'make-versioned-regex x))
                                    '(("menu" "css")
                                      ("navigation" "css"))))
               :test (lambda (x y) (cl-ppcre:scan y x))))


(addtest prune-dependencies-1
  (ensure-same
   (values-list (mapcar #'dependency-url
                        (weblocks::prune-dependencies
                         (list (make-instance 'stylesheet-dependency
                                 :url "http://boing.com/abc.css")
                               (make-instance 'stylesheet-dependency
                                 :url "http://boing.com/bcd.css")))))
   (values (puri:uri "http://boing.com/abc.css") (puri:uri "http://boing.com/bcd.css"))
   :test puri:uri=))

(addtest prune-dependencies-2
  (ensure-same
   (values-list
    (mapcar #'dependency-url
            (weblocks::prune-dependencies
             (list (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")))))
   (values (puri:uri "http://boing.com/abc.css") (puri:uri "http://boing.com/bcd.css"))
   :test puri:uri=))


(addtest sort-dependencies-by-type-1
  (ensure-same
   (values-list
    (mapcar #'dependency-url
            (weblocks::sort-dependencies-by-type
             (list (make-instance 'script-dependency :url "http://boing.com/abc.js")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'script-dependency :url "http://boing.com/bcd.js")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/aaa.css")
                   ))))
   (values-list
    (mapcar #'puri:uri
            '("http://boing.com/bcd.css" "http://boing.com/bcd.css"
              "http://boing.com/aaa.css" "http://boing.com/abc.js"
              "http://boing.com/bcd.js")))
   :test puri:uri=))


(addtest compact-dependencies-1
  (ensure-same
   (values-list
    (mapcar #'dependency-url
            (compact-dependencies
             (list (make-instance 'script-dependency :url "http://boing.com/abc.js")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'script-dependency :url "http://boing.com/bcd.js")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
                   (make-instance 'stylesheet-dependency :url "http://boing.com/aaa.css")
                   ))))
  (values-list (mapcar #'puri:uri
                       '("http://boing.com/bcd.css" "http://boing.com/aaa.css"
                         "http://boing.com/abc.js" "http://boing.com/bcd.js")))
  :test puri:uri=))



(addtest render-dependency-in-page-head-1
  (ensure-html-output
   (render-dependency-in-page-head
    (make-instance 'script-dependency :url "http://boing.com/abc.js"))
   (:script :src "http://boing.com/abc.js" :type "text/javascript" "")))


(addtest render-dependency-in-page-head-2
  (ensure-html-output
   (render-dependency-in-page-head
    (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css"))
   (:link :rel "stylesheet" :type "text/css" :href "http://boing.com/abc.css")))

