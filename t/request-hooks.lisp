(defpackage #:weblocks.t.request-hooks
  (:use #:cl
        #:prove
        #:weblocks.t.utils))
(in-package weblocks.t.request-hooks)


(plan 3)


(subtest "Callbacks list for unknown name is empty"
  (with-session
    (with-request ("/")
      (is (weblocks.hooks::get-callbacks
           weblocks.hooks::*session-hooks*
           :some-unknown-name)
          nil
          "If no callbacks were added for the name, then get-callbacks should return an empty list."))))


(subtest "Add session hook"
  (with-session
    (with-request ("/")
      (weblocks.hooks:add-session-hook :post-action 'foo)

      (ok (member 'foo (weblocks.hooks::get-callbacks
                        weblocks.hooks::*session-hooks*
                        :post-action))
          "Function add-session-hook should put given value into the list of callbacks bound to the session."))))


(subtest "Hooks evaluation"
  (subtest "Without params"
    (with-session
      (with-request ("/")
        (let (call-result)
          (flet ((callback ()
                   (setf call-result
                         'callback-was-called)))
            (weblocks.hooks:add-session-hook :post-action #'callback)
            (weblocks.hooks:eval-hooks :post-action)

            (is call-result
                'callback-was-called))))))

  (subtest "With params"
    (with-session
      (with-request ("/")
        (let (call-result)
          (flet ((callback (param)
                   (push param call-result)))
            (weblocks.hooks:add-session-hook :some-hook #'callback)
            (weblocks.hooks:eval-hooks :some-hook 'blah)

            (is call-result
                '(blah))))))))


(finalize)
