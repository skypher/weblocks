
(in-package :weblocks-test)

;; Slot rendering helper
(defun render-slot-simple (obj slot-name slot-value &rest keys)
  (if (typep slot-value 'standard-object)
      (apply #'weblocks::visit-object-slots slot-value #'render-slot-simple keys)
      (with-html
	(:p (str slot-name))
	(:p (str slot-value)))))

;;; Test humanize-name function
(deftest humanize-name-1
    (humanize-name 'hello-world)
  "Hello World")

(deftest humanize-name-2
    (humanize-name "HELLO-WORLD")
  "Hello World")

(deftest humanize-name-3
    (humanize-name 'hello-ref)
  "Hello")

;;; Test attributize-name function
(deftest attributize-name-1
    (attributize-name 'hello-world)
  "hello-world")

(deftest attributize-name-2
    (attributize-name "hello World-REF")
  "hello-world-ref")

(deftest attributize-name-3
    (attributize-name nil)
  "")

(deftest attributize-name-4
    (attributize-name 1)
  "1")

;;; Test list->assoc function
(deftest list->assoc-1
    (weblocks::list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

;;; Introspection helper
(defun class-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
	  (apply #'weblocks::class-visible-slots (class-of obj) args)))

;;; Test class-visible-slots function
(deftest class-visible-slots-1
    (class-visible-slot-names *joe*)
  (name manager))

(deftest class-visible-slots-2
    (class-visible-slot-names *joe* :visible-slots '(age))
  (name age manager))

(deftest class-visible-slots-3
    (class-visible-slot-names *joe* :visible-slots '(age blah))
  (name age manager))

;;; Test get-slot-value
(deftest get-slot-value-1
    (get-slot-value *joe* (car (car (object-visible-slots *joe* :slots '(age) :mode :strict))))
  30)

(deftest get-slot-value-2
    (get-slot-value *joe* (car (car (object-visible-slots *joe*))))
  "Joe")

;;; Test slot-value-by-path
(deftest slot-value-by-path-1
    (slot-value-by-path *joe* '(address-ref street))
  "100 Broadway")

(deftest slot-value-by-path-2
    (slot-value-by-path *joe* 'name)
  "Joe")

(deftest slot-value-by-path-3
    (slot-value-by-path *joe* '(nil name))
  "Joe")

(deftest slot-value-by-path-4
    (address-city (slot-value-by-path *joe* '(address-ref)))
  "New York")

(deftest slot-value-by-path-5
    (slot-value-by-path *joe* '(address-ref) :observe-inline-p t)
  "Address")

;;; Introspection helper
(defun object-visible-slot-names (obj &rest args)
  (mapcar (lambda (x)
	    (if (typep (car x) 'standard-direct-slot-definition)
		(cons (slot-definition-name (car x)) (cdr x))
		x))
	  (apply #'object-visible-slots obj args)))

;;; Test render-slot-inline-p
(deftest render-slot-inline-p-1
    (render-slot-inline-p *joe* 'name)
  t)

(deftest render-slot-inline-p-2
    (render-slot-inline-p *joe* 'address-ref)
  nil)

;;; Test object-visible-slots function
(deftest object-visible-slots-1
    (object-visible-slot-names *joe*)
  ((name . name) (manager . manager)))

(deftest object-visible-slots-2
    (object-visible-slot-names *joe* :slots '((name . "first-name")))
  ((name . "first-name") (manager . manager)))

(deftest object-visible-slots-3
    (object-visible-slot-names *joe* :slots '((name . first-name) age))
  ((name . first-name) (age . age) (manager . manager)))

(deftest object-visible-slots-4
    (object-visible-slot-names *joe* :slots '((name . first-name) (age . how-old)))
  ((name . first-name) (age . how-old) (manager . manager)))

(deftest object-visible-slots-5
    (object-visible-slot-names *joe* :slots '((manager . boss) doesnt-exist))
  ((name . name) (manager . boss)))

(deftest object-visible-slots-6
    (object-visible-slot-names *joe* :slots '(manager) :mode :hide)
  ((name . name)))

(deftest object-visible-slots-7
    (object-visible-slot-names *joe* :slots '(manager name) :mode :strict)
  ((manager . manager) (name . name)))

(deftest object-visible-slots-8
    (object-visible-slot-names *joe* :slots '(manager (name . first-name) (age . how-old))
				     :mode :strict)
  ((manager . manager) (name . first-name) (age . how-old)))

(deftest object-visible-slots-9
    (object-visible-slot-names *joe* :slots '(manager) :mode :strict)
  ((manager . manager)))

(deftest object-visible-slots-10
    (mapcar
     (lambda (x)
       (if (functionp x)
	   (funcall x)
	   x))
     (flatten (object-visible-slot-names *joe* :slots `((manager . ,(lambda () 1))))))
  (name name manager 1))

(deftest object-visible-slots-11
    (mapcar
     (lambda (x)
       (if (functionp x)
	   (funcall x)
	   x))
     (flatten (object-visible-slot-names *joe* :slots `((manager . ,(lambda () 1))) :mode :strict)))
  (manager 1))

(deftest object-visible-slots-12
    ;; Make sure we can't add new fields in strict mode - since the
    ;; arguments propagate recursively, new slots appear more than
    ;; once for complex objects. Additionally, slots specified for
    ;; inner objects appear as new slots in the outer objects and vica
    ;; versa.
    (values
     (object-visible-slot-names *joe* :slots '(name test) :mode :strict)
     (object-visible-slot-names *joe* :slots '(name (test . blah)) :mode :strict))
  ((name . name))
  ((name . name)))

(deftest object-visible-slots-13
    (object-visible-slot-names *joe* :custom-slots '((1 . (foo . hello)) (bar . world)))
  ((name . name) (foo . hello) (manager . manager) (bar . world)))

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
    (string-whitespace-p "   	")
  t)

(deftest string-whitespace-p-3
    (string-whitespace-p " a  	")
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

;;; test strictly-less-p
(deftest strictly-less-p-1
    (strictly-less-p 1 2)
  t)

(deftest strictly-less-p-2
    (not (null (strictly-less-p "a" "b")))
  t)

(deftest strictly-less-p-3
    (not (null (strictly-less-p nil nil)))
  nil)

(deftest strictly-less-p-4
    (strictly-less-p nil 'a)
  nil)

(deftest strictly-less-p-5
    (strictly-less-p 'a nil)
  t)

;;; test equivalentp
(deftest equivalentp-1
    (equivalentp "a" "a")
  t)

(deftest equivalentp-2
    (equivalentp nil nil)
  t)

;;; test object-id
(deftest object-id-1
    (object-id *joe*)
  1)

;;; test visit-object-slots
(deftest-html visit-object-slots-1
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     :slots '(name) :mode :strict)
  (htm
   (:p "NAME")
   (:p "Joe")))

(deftest-html visit-object-slots-2
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     :slots `((name . ,(lambda (obj slot-name slot-value &rest args)
			       (with-html (:p "TEST"))))))
  (htm
   (:p "TEST")
   (:p "MANAGER")
   (:p "Jim")))

(deftest-html visit-object-slots-3
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     :slots `(name)
     :mode :strict
     :custom-slots `((blah . ,(lambda (obj slot-name slot-value &rest args)
				      (with-html (:p "TEST"))))
		     hello))
  (htm
   (:p "NAME")
   (:p "Joe")
   (:p "TEST")
   (:p "HELLO")
   (:p "NIL")))

(deftest-html visit-object-slots-4
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     :slots `((name . ,(lambda (obj slot-name slot-value &rest args)
			       (with-html (:p "TEST")))))
     :call-around-fn-p nil)
  (htm
   (:p "NAME")
   (:p "Joe")
   (:p "MANAGER")
   (:p "Jim")))

(deftest visit-object-slots-5
    (weblocks::visit-object-slots
     *joe*
     (lambda (obj slot-name slot-value &rest keys)
       1))
  (1 1))

(deftest-html visit-object-slots-6
    (weblocks::visit-object-slots
     *joe*
     #'render-slot-simple
     :slots '(name education) :custom-slots '(test))
  (htm
   (:p "NAME") (:p "Joe")
   (:p "UNIVERSITY") (:p "Bene Gesserit University")
   (:p "GRADUATION-YEAR") (:p "2000")
   (:p "MANAGER") (:p "Jim")
   (:p "TEST") (:p "NIL")))

(deftest-html visit-object-slots-7
    (weblocks::visit-object-slots
     (make-instance 'employee)
     #'render-slot-simple
     :slots '(name) :mode :strict
     :ignore-unbound-slots-p t)
  (htm
   (:p "NAME")
   (:p "NIL")))

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

;;; test tokenize-uri
(deftest tokenize-uri-1
    (weblocks::tokenize-uri "///hello/world/blah\\test\\world?hello=5 ;blah=7")
  ("hello" "world" "blah" "test" "world"))

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
    (let (*uri-tokens*)
      (declare (special *uri-tokens*))
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
    (find-all '(1 2 3 4 5 6) #'oddp)
  (1 3 5))

(deftest find-all-2
    (find-all '(1 2 3 4 5 6) #'oddp :key #'1+)
  (2 4 6))

;;; test stable-set-difference
(deftest stable-set-difference-1
    (stable-set-difference '(1 2 3 4 5 6 7 8) '(2 5 6))
  (1 3 4 7 8))
