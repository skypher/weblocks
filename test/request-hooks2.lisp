(defpackage #:weblocks.t.request-hooks
  (:use #:cl
        #:prove
        #:weblocks.t.utils))
(in-package weblocks.t.request-hooks)


(subtest "session-request-hooks"
  (with-request ("/")
    (with-session
      (let ((hooks (weblocks::session-request-hooks)))
        (isnt hooks
              nil
              "For fresh request and session, request-hooks should be a fresh struct.")
        (is (weblocks::dynamic-action-hook hooks)
            nil
            "And there is no dynamic action hooks.")
        (is (weblocks::pre-action-hook hooks)
            nil
            "And there is no \"pre\" action hooks.")
        (is (weblocks::post-action-hook hooks)
            nil
            "And there is no \"post\" action hooks.")

        (is (weblocks::dynamic-render-hook hooks)
            nil
            "And there is no dynamic render hooks.")
        (is (weblocks::pre-render-hook hooks)
            nil
            "And there is no \"pre\" render hooks.")
        (is (weblocks::post-render-hook hooks)
            nil
            "And there is no \"post\" render hooks.")))))


(subtest "add-request-hook"
  (with-request ("/")
    (with-session
      (weblocks::add-request-hook :session :post-action 'foo)

      (is  (member 'foo (weblocks:request-hook :session :post-action))
           t))))
