(defpackage #:weblocks.t.actions
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.actions)


(plan 4)


(subtest "get-action-name-from-request"
  (with-session
    (with-request ("/?action=blah" :method :get)
      (is (weblocks.request::get-action-name-from-request)
          "blah"
          "It should work with GET parameters"))
    
    (with-request ("/" :method :post
                       :data '(("action" . "blah")))
      (is (weblocks.request::get-action-name-from-request)
          "blah"
          "And with POSTs"))))


(subtest "make-action/get-request-action-1"
  (with-session
    (let ((action-name (make-action (lambda (&rest keys)
                                      (declare (ignore keys))
                                      123))))
      (is (weblocks::eval-action action-name nil)
          123
          "This action just returns 123 when evaluated."))))


(subtest "function-or-action->action-error"
  (with-session
    (with-request ("/")
      (is-error (weblocks::function-or-action->action "abc123")
                'error
                "Action with name \"abc123\" wasn't defined and function should raise an exception."))))


(subtest "function-or-action->action-success"
  (with-session
    (make-action #'identity "abc123")
    
    (is (weblocks::function-or-action->action "abc123")
        "abc123"
        "When action is defined function-or-action->action should return it's name")

    (is (weblocks::function-or-action->action #'identity)
        "abc123"
        "This also should work if a function was given as an argument")))


(finalize)
