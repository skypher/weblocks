(defpackage #:weblocks.t.request-handler
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.request-handler)


(plan 1)


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


(subtest "process-request-with-action"
  (with-session
    (with-request ("/foo/bar?action=store-data" :app app-with-init)
      (let ((result (weblocks.request-handler:handle-client-request
                     weblocks.app::*current-app*)))
        (ok (search "Hello world" result)
            "Result should have a greeting.")))))

(finalize)
