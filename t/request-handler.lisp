(defpackage #:weblocks/t/request-handler
  (:use #:cl
        #:rove
        #:hamcrest/rove
;;        #:weblocks
        #:weblocks/t/utils)
  (:import-from #:weblocks.app
                #:defapp
                #:*current-app*)
  (:import-from #:weblocks.widgets.string-widget
                #:make-string-widget)
  (:import-from #:weblocks.request-handler
                #:handle-client-request)
  (:import-from #:weblocks.response
                #:catch-possible-abort
                #:*code*
                #:*headers*)
  (:import-from #:weblocks.session))
(in-package weblocks/t/request-handler)


(defapp app-with-init
  :prefix "/"
  :autostart nil)


(defmethod weblocks.session:init ((app app-with-init))
  (make-string-widget
   "Hello world"))


(deftest process-first-request
  (with-session
    (with-request ("/foo/bar" :app app-with-init)
      (let ((result (handle-client-request *current-app*)))
        (ok (search "Hello world" result)
            "Result should have a greeting.")))))


(deftest process-request-with-missing-action
  (with-session
    (with-request ("/foo/bar?action=store-data" :app app-with-init)
      (let ((result (catch-possible-abort
                      (handle-client-request *current-app*))))
        (ok (equal result
                   "")
            "Result should have an error message.")
        (ok (equal *code* 302)
            "And response code should be 302")

        (testing "And user should be redirected to the app's prefix uri."
          (assert-that *headers*
                       (has-plist-entries :location "/")))))))

