(defpackage #:weblocks.t.request-handler
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.request-handler)


(plan 2)


(weblocks.app:defapp app-with-init
  :prefix "/"
  :autostart nil)


(defmethod weblocks.session:init ((app app-with-init))
  (weblocks.widgets.string-widget:make-string-widget
   "Hello world"))


(subtest "process-first-request"
  (with-session
    (with-request ("/foo/bar" :app app-with-init)
      (let ((result (weblocks.request-handler:handle-client-request
                     weblocks.app::*current-app*)))
        (ok (search "Hello world" result)
            "Result should have a greeting.")))))


(subtest "process-request-with-missing-action"
  (with-session
    (with-request ("/foo/bar?action=store-data" :app app-with-init)
      (let ((result (weblocks.response:catch-possible-abort
                      (weblocks.request-handler:handle-client-request
                       weblocks.app::*current-app*))))
        (is result
            ""
            "Result should have an error message.")
        (is weblocks.response:*code*
            302
            "And response code should be 302")

        (subtest "And user should be redirected to the app's prefix uri."
          (assert-that
           weblocks.response:*headers*
           (has-plist-entries :location "/")))))))

(finalize)
