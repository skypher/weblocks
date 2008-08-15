(in-package :weblocks-test)

(deftest dependencies-by-symbol-1
    (with-webapp ()
      (remove nil (weblocks::dependencies-by-symbol 'non-existent-widget-name)))
  nil)

(deftest dependencies-by-symbol-2
    (format nil "~A" (mapcar #'dependency-url
			     (remove nil (weblocks::dependencies-by-symbol 'navigation))))
  "(/pub/stylesheets/navigation.css)")


(deftest make-local-dependency-1
    (with-webapp ()
      (make-local-dependency :stylesheet "non-existing-file-name"))
  nil)

(deftest make-local-dependency-2
    (format nil "~A" (dependency-url (make-local-dependency :stylesheet "non-existing-file-name" :do-not-probe t)))
  "/pub/stylesheets/non-existing-file-name.css")

(deftest make-local-dependency-3
    (format nil "~A" (dependency-url (make-local-dependency :stylesheet "main")))
  "/pub/stylesheets/main.css")

(deftest make-local-dependency-4
    (format nil "~A" (dependency-url (make-local-dependency :script "weblocks")))
  "/pub/scripts/weblocks.js")

(deftest make-local-dependency-5
    (format nil "~A" (dependency-url (make-local-dependency :script "weblocks" :do-not-probe t)))
  "/pub/scripts/weblocks.js")



(deftest dependencies-equalp-1
    (dependencies-equalp (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
			 (make-instance 'stylesheet-dependency :url "http://boing.com/bcd"))
  nil)

(deftest dependencies-equalp-2
    (dependencies-equalp (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
			 (make-instance 'stylesheet-dependency :url "http://boing.com/abc"))
  t)

(deftest dependencies-equalp-3
    (dependencies-equalp (make-instance 'script-dependency :url "http://boing.com/abc")
			 (make-instance 'script-dependency :url "http://boing.com/bcd"))
  nil)

(deftest dependencies-equalp-4
    (dependencies-equalp (make-instance 'script-dependency :url "http://boing.com/abc")
			 (make-instance 'script-dependency :url "http://boing.com/abc"))
  t)



(deftest dependencies-lessp-1
    (dependencies-lessp (make-instance 'stylesheet-dependency :url "http://boing.com/abc")
			(make-instance 'stylesheet-dependency :url "http://boing.com/bcd"))
  nil)

(deftest dependencies-lessp-2
    (dependencies-lessp (make-instance 'script-dependency :url "http://boing.com/abc")
			(make-instance 'stylesheet-dependency :url "http://boing.com/bcd"))
  nil)

(deftest dependencies-lessp-3
    (dependencies-lessp (make-instance 'stylesheet-dependency :url "http://boing.com/bcd")
			(make-instance 'script-dependency :url "http://boing.com/abc"))
  t)

(deftest dependencies-lessp-4
    (dependencies-lessp (make-instance 'script-dependency :url "http://boing.com/bcd")
			(make-instance 'script-dependency :url "http://boing.com/abc"))
  nil)

(deftest dependencies-lessp-5
    (dependencies-lessp nil nil)
  nil)


(deftest per-class-dependencies-1
    (per-class-dependencies "a string")
  nil)

(deftest per-class-dependencies-2
    (per-class-dependencies (lambda ()))
  nil)

(deftest per-class-dependencies-3
    (per-class-dependencies 'some-symbol)
  nil)

(deftest per-class-dependencies-4
    (apply #'dependencies-equalp
	    (append
	     (remove nil (per-class-dependencies (make-instance 'navigation)))
	     (list (make-local-dependency :stylesheet "navigation"))))
  t)



(deftest dependencies-1
    (with-webapp ()
      (dependencies 'some-symbol))
  nil)

(deftest dependencies-2
    (with-webapp ()
      (dependencies "some-string"))
  nil)

(deftest dependencies-3
    (dependencies (lambda ()))
  nil)

(deftest dependencies-4
    (dependencies (make-instance 'widget :name "abc123"))
  nil)

(deftest dependencies-5
    (with-webapp ()
      (apply #'dependencies-equalp
	     (append
	      (dependencies "main")
	      (list (make-local-dependency :stylesheet "main")))))
  t)

(deftest dependencies-6
    (format nil "~A" (mapcar #'dependency-url (dependencies (make-instance 'navigation))))
  "(/pub/stylesheets/navigation.css)")


(deftest prune-dependencies-1
    (format nil "~A"
	    (mapcar #'dependency-url
		    (weblocks::prune-dependencies
		     (list (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")))))
  "(http://boing.com/abc.css http://boing.com/bcd.css)")

(deftest prune-dependencies-2
    (format nil "~A"
	    (mapcar #'dependency-url
		    (weblocks::prune-dependencies
		     (list (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")))))
  "(http://boing.com/abc.css http://boing.com/bcd.css)")


(deftest sort-dependencies-by-type-1
    (format nil "~A"
	    (mapcar #'dependency-url
		    (weblocks::sort-dependencies-by-type
		     (list (make-instance 'script-dependency :url "http://boing.com/abc.js")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'script-dependency :url "http://boing.com/bcd.js")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/aaa.css")
			   ))))
  "(http://boing.com/bcd.css http://boing.com/bcd.css http://boing.com/aaa.css
 http://boing.com/abc.js http://boing.com/bcd.js)")


(deftest compact-dependencies-1
    (format nil "~A"
	    (mapcar #'dependency-url
		    (compact-dependencies
		     (list (make-instance 'script-dependency :url "http://boing.com/abc.js")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'script-dependency :url "http://boing.com/bcd.js")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/bcd.css")
			   (make-instance 'stylesheet-dependency :url "http://boing.com/aaa.css")
			   ))))
  "(http://boing.com/bcd.css http://boing.com/aaa.css http://boing.com/abc.js
 http://boing.com/bcd.js)")


(deftest-html render-dependency-in-page-head-1
    (render-dependency-in-page-head (make-instance 'script-dependency :url "http://boing.com/abc.js"))
  (:script :src "http://boing.com/abc.js" :type "text/javascript" ""))

(deftest-html render-dependency-in-page-head-2
    (render-dependency-in-page-head (make-instance 'stylesheet-dependency :url "http://boing.com/abc.css"))
  (:link :rel "stylesheet" :type "text/css" :href "http://boing.com/abc.css"))

