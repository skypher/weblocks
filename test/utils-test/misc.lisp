
(in-package :weblocks-test)

(deftestsuite utils/misc-suite (weblocks-suite)
  ())

;;; Test humanize-name function
(deftest humanize-name-1
    (humanize-name 'hello-world)
  "Hello World")

(deftest humanize-name-2
    (humanize-name "HELLO-WORLD")
  "Hello World")

;;; Test attributize-name function
(deftest attributize-name-1
    (attributize-name 'hello-world)
  "hello-world")

(deftest attributize-name-2
    (attributize-name nil)
  "")

(deftest attributize-name-3
    (attributize-name 1)
  "1")

;;; Test list->assoc function
(deftest list->assoc-1
    (weblocks::list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

;;; Introspection helper
(defun class-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
          (apply #'weblocks-stores:class-visible-slots (class-of obj) args)))

;;; Test slot-value-by-path
(deftest slot-value-by-path-1
    (slot-value-by-path *joe* '(address street))
  "100 Broadway")

(deftest slot-value-by-path-2
    (slot-value-by-path *joe* 'name)
  "Joe")

(deftest slot-value-by-path-3
    (slot-value-by-path *joe* '(nil name))
  "Joe")

(deftest slot-value-by-path-4
    (address-city (slot-value-by-path *joe* '(address)))
  "New York")

;;; test safe-apply
(deftest safe-apply-1
    (safe-apply #'identity '(5))
  5)

(deftest safe-apply-2
    (safe-apply nil '(5))
  nil)

;;; test safe-funcall
(deftest safe-funcall-1
    (safe-funcall #'identity 5)
  5)

(deftest safe-funcall-2
    (safe-funcall nil 5)
  nil)

;;; test request-parameter
(deftest request-parameter-1
    (with-request :get '(("a" . 1) ("b" . 2))
      (request-parameter "a"))
  1)

(deftest request-parameter-2
    (with-request :post '(("a" . 1) ("b" . 2))
      (request-parameter "b"))
  2)

;;; test request-parameters
(deftest request-parameters-1
    (with-request :get '(("a" . 1) ("b" . 2))
      (request-parameters))
  (("a" . 1) ("b" . 2)))

(deftest request-parameters-2
    (with-request :post '(("a" . 1) ("b" . 2))
      (request-parameters))
  (("a" . 1) ("b" . 2)))

;;; test string-whitespace-p
(deftest string-whitespace-p-1
    (string-whitespace-p "")
  t)

(deftest string-whitespace-p-2
    (string-whitespace-p "      ")
  t)

(deftest string-whitespace-p-3
    (string-whitespace-p " a    ")
  nil)

;;; test render-extra-tags
(deftest-html render-extra-tags-1
    (render-extra-tags "test-" 2)
  (htm (:div :class "test-1" "<!-- empty -->")
       (:div :class "test-2" "<!-- empty -->")))

;;; test with-extra-tags
(deftest-html with-extra-tags-1
    (with-extra-tags
      (with-html (:div "hi")))
  (htm (:div :class "extra-top-1" "<!-- empty -->")
       (:div :class "extra-top-2" "<!-- empty -->")
       (:div :class "extra-top-3" "<!-- empty -->")
       (:div "hi")
       (:div :class "extra-bottom-1" "<!-- empty -->")
       (:div :class "extra-bottom-2" "<!-- empty -->")
       (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; test alist->plist
(deftest alist->plist-1
    (alist->plist '((hello . world) (blah . test)))
  (:hello world :blah test))

(deftest alist->plist-2
    (alist->plist '(("hello" . world) ("blah" . test)))
  (:hello world :blah test))

(deftest alist->plist-3
    (alist->plist nil)
  nil)

;;; test intersperse
(deftest intersperse-1
    (intersperse '(1 2 3 4 5) 0)
  (1 0 2 0 3 0 4 0 5))

(deftest intersperse-2
    (intersperse '(1) 0)
  (1))

(deftest intersperse-3
    (intersperse nil 0)
  nil)

(deftest intersperse-4
    (intersperse '((1 2) (3 4) (5 6)) 0)
  ((1 2) 0 (3 4) 0 (5 6)))

(deftest intersperse-5
    (intersperse '(1 2) 0)
  (1 0 2))

(deftest intersperse-6
    (intersperse '(1 2) 0 :last -1)
  (1 -1 2))

(deftest intersperse-7
    (intersperse '(1 2 3 4 5 6) 0 :last -1)
  (1 0 2 0 3 0 4 0 5 -1 6))

;;; test insert-after
(deftest insert-after-1
    (let ((a (list 1 3 4 5)))
      (insert-after 2 a 0)
      a)
  (1 2 3 4 5))

;;; test insert-at
(deftest insert-at-1
    (let ((a (list 1 2 3 4)))
      (insert-at 0 a 0)
      a)
  (0 1 2 3 4))

(deftest insert-at-2
    (let ((a (list 1 2 3 4)))
      (insert-at 1.5 a 1)
      a)
  (1 1.5 2 3 4))

(deftest insert-at-3
    (let ((a (list 1 2 3 4)))
      (insert-at 5 a 4)
      a)
  (1 2 3 4 5))

;;; test remove-keyword-parameter
(deftest remove-keyword-parameter-1
    (remove-keyword-parameter '(0 :a 1 :b 2 :c 3) :b)
  (0 :a 1 :c 3))

;;; test remove-keyword-parameters
(deftest remove-keyword-parameters-1
    (remove-keyword-parameters '(0 :a 1 :b 2 :c 3) :a :c)
  (0 :b 2))

;;; test tokenize-uri
(addtest tokenize-uri
  (with-test-webapp ()
    (ensure-same 
      (weblocks::tokenize-uri "/hello/world/blah/test/world?hello=5 ;blah=7")
      '("hello" "world" "blah" "test" "world"))
    (ensure-same (weblocks::tokenize-uri "") nil)
    (ensure-same (weblocks::tokenize-uri "/") nil)
    (ensure-same (weblocks::tokenize-uri "//") '(""))))

;;; test public-file-relative-path
(deftest public-file-relative-path-1
    (format nil "~A" (public-file-relative-path :stylesheet "foo"))
  "stylesheets/foo.css")

(deftest public-file-relative-path-2
    (format nil "~A" (public-file-relative-path :script "bar"))
  "scripts/bar.js")

;;; test public-files-relative-paths
(deftest public-files-relative-paths-1
    (format nil "~A" (public-files-relative-paths
                      '(:stylesheet . "foo")
                      '(:script . "bar")))
  "(stylesheets/foo.css scripts/bar.js)")

;;; test request-uri-path
(deftest request-uri-path-1
    (with-request :get nil
      (request-uri-path))
  "/foo/bar")

(deftest request-uri-path-2
    (with-request :get nil :uri "/"
      (request-uri-path))
  "/")

;;; test string-remove-left
(deftest string-remove-left-1
    (string-remove-left "Hello World" "he")
  nil)

(deftest string-remove-left-2
    (string-remove-left "Hello World" "He")
  "llo World")

(deftest string-remove-left-3
    (string-remove-left "Hello World" "he" :ignore-case-p t)
  "llo World")

;;; test string-remove-right
(deftest string-remove-right-1
    (string-remove-right "Hello World" "Ld")
  nil)

(deftest string-remove-right-2
    (string-remove-right "Hello World" "ld")
  "Hello Wor")

(deftest string-remove-right-3
    (string-remove-right "Hello World" "Ld" :ignore-case-p t)
  "Hello Wor")

;;; test find-all
(deftest find-all-1
    (weblocks-util::find-all '(1 2 3 4 5 6) #'oddp)
  (1 3 5))

(deftest find-all-2
    (weblocks-util::find-all '(1 2 3 4 5 6) #'oddp :key #'1+)
  (2 4 6))

;;; test stable-set-difference
(deftest stable-set-difference-1
    (stable-set-difference '(1 2 3 4 5 6 7 8) '(2 5 6))
  (1 3 4 7 8))

;;; test symbol-status
(deftest symbol-status-1
    (symbol-status 'integer)
  :external)

(deftest symbol-status-2
    (symbol-status 'symbol-status-1)
  :internal)

;;; test string-invert-case
(deftest string-invert-case-1
    (string-invert-case "tEst")
  "TeST")

(deftest string-invert-case-2
    (string-invert-case "")
  "")

(deftest string-invert-case-3
    (string-invert-case 'nil)
  "nil")

;;; test ninsert
(deftest ninsert-1
    (let ((l (list 1 2 3 4)))
      (ninsert l 0 0))
  (0 1 2 3 4))

(deftest ninsert-2
    (let ((l (list 1 2 3 4)))
      (ninsert l 0 1))
  (1 0 2 3 4))

(deftest ninsert-3
    (let ((l (list 1 2 3 4)))
      (ninsert l 0 4))
  (1 2 3 4 0))

;;; test puri:uri from pathname
(deftest puri-uri-pathname-1
    (puri:render-uri (puri:uri (make-pathname :directory '(:relative "foo" "bar"))) nil)
  "foo/bar/")

(deftest puri-uri-pathname-2
    (puri:render-uri (puri:uri (make-pathname :directory '(:absolute "foo" "bar"))) nil)
  "/foo/bar/")

(deftest puri-uri-pathname-3
    (puri:render-uri (puri:uri (make-pathname :directory '(:absolute "foo" "bar")
                                              :name "baz" :type "txt")) nil)
  "/foo/bar/baz.txt")

;;; test add-get-param-to-url
(deftest add-get-param-to-url-1
    (weblocks:add-get-param-to-url "/foo/bar/baz?hi=bye" "a" "b")
  "/foo/bar/baz?hi=bye&a=b")

;;; test remove-parameter-from-uri
(deftest remove-parameter-from-uri-1
    (with-request :get '(("action" . "test1") ("session" . "test2"))
      (weblocks:remove-parameter-from-uri "/pub/baz" "session"))
  "/pub/baz?action=test1")

(deftest remove-parameter-from-uri-2
    (with-request :get '(("action" . "test1") ("session" . "test2"))
      (weblocks:remove-parameter-from-uri "/pub/baz" "action"))
  "/pub/baz?session=test2")

(deftest remove-parameter-from-uri-3
    (with-request :get '(("action" . "test1") ("session" . "test2") ("foo" . "bar"))
      (weblocks:remove-parameter-from-uri "/pub/baz" "action"))
  "/pub/baz?session=test2&foo=bar")

;;; Test object-class-name
(deftest object-class-name-1
    (object-class-name *joe*)
  employee)

;;; test append-custom-slots
(deftest append-custom-fields-1
    (weblocks::append-custom-fields '(a b c) '(:a 1 :b 2 :custom-fields (d e f) :c 3))
  (d e f a b c))

;;; test hash-keys
(deftest hash-keys-1
    (let ((ht (make-hash-table )))
      (setf (gethash 'foo ht) 1)
      (setf (gethash 'bar ht) 1)
      (sort (hash-keys ht) #'string-lessp :key #'symbol-name))
  (bar foo))

;;; Test find-slot-dsd
(deftest find-slot-dsd-1
    (slot-definition-name (find-slot-dsd 'employee 'name))
  name)

(defclass find-slot-dsd-test-a ()
  ((find-slot-dsd-test-a :accessor find-slot-dsd-test-a)))

(defclass find-slot-dsd-test-b ()
  ((find-slot-dsd-test-b :accessor find-slot-dsd-test-b)))

(defclass find-slot-dsd-test-c (find-slot-dsd-test-a find-slot-dsd-test-b)
  ())

(deftest find-slot-dsd-2
    (slot-definition-name (find-slot-dsd 'find-slot-dsd-test-c 'find-slot-dsd-test-b))
  find-slot-dsd-test-b)

;;; Test find-slot-esd
(deftest find-slot-esd-1
    (slot-definition-name (find-slot-esd 'employee 'name))
  name)

;;; Test drop-last
(deftest drop-last-1
    (drop-last (list 1 2 3))
  (1 2))

(deftest drop-last-2
    (drop-last (list 1))
  nil)

(deftest drop-last-3
    (drop-last nil)
  nil)

;;; Test function-designator-p
(deftest function-designator-p-1
    (function-designator-p 'not-a-function)
  nil)

(deftest function-designator-p-2
    (function-designator-p 'append)
  t)

(deftest function-designator-p-3
    (function-designator-p (lambda ()))
  t)

(deftest function-designator-p-4
    (function-designator-p (list))
  nil)

;;; maybe-add-trailing-slash
(addtest maybe-add-trailing-slash-accept-pathnames
  (let* ((p (make-pathname :directory '(:absolute "usr" "bin")))
         (p2 (make-pathname :defaults p :name "env" :type nil
                            :version :newest)))
    (ensure-same (weblocks::maybe-add-trailing-slash p) p)
    (ensure-same (weblocks::maybe-add-trailing-slash p2)
                 (make-pathname :directory '(:absolute "usr" "bin" "env")))))
