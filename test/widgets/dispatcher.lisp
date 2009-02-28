
(in-package :weblocks-test)

;; whether find-widget-by-path* leaves the cache alone
(deftest dispatcher-cache-1
    (with-request :get nil :uri "/test1"
      (setf (root-composite) (create-site-layout))
      (catch 'hunchentoot::handler-done
	(handle-client-request (weblocks::current-webapp)))
      (flet ((the-cache-path ()
	       (car (dispatcher-cache
		     (find-widget-by-path '(root-inner test-nav-1)))))) 
	(values (the-cache-path)
		(progn
		  (find-widget-by-path '(root-inner test-nav-1 test2))
		  (the-cache-path)))))
  ("test1")
  ("test1"))
