(defpackage #:weblocks-test/hooks
  (:use #:cl
        #:rove
        #:weblocks-test/utils)
  (:import-from #:weblocks/hooks
                #:get-callbacks
                #:*session-hooks*
                #:get-callbacks-names
                #:with-hook
                #:call-next-hook))
(in-package weblocks-test/hooks)


(deftest empty-call-back-list
  (testing "Callbacks list for unknown name is empty"
    (with-session
      (with-request ("/")
        (ng (get-callbacks
             *session-hooks*
             :some-unknown-name)
            "If no callbacks were added for the name, then get-callbacks should return an empty list.")))))


(weblocks/hooks:defhook test-action
  "Just a test hook.")


(deftest on-session-hook-test
  (with-session
      (with-request ("/")
        (weblocks/hooks:on-session-hook-test-action
         foo ())

        (let ((callbacks (get-callbacks-names
                          *session-hooks*
                          'test-action)))
          (ok (equal (first callbacks)
                     'foo))
          "Macro on-session-hook-test-action should put a callable into the list of callbacks bound to the session."))))


(weblocks/hooks:defhook some-hook)


(deftest hooks-evaluation
  (testing "Without params"
    (with-session
      (with-request ("/")
        (let (call-result)
          
          (weblocks/hooks:on-session-hook-test-action
            set-result ()
            (setf call-result
                  'callback-was-called))
          
          (weblocks/hooks:with-test-action-hook ()
            ;; do nothing
            )

          (ok (equal call-result
                     'callback-was-called))))))

  (testing "With params"
    (with-session
      (with-request ("/")
        (let (call-result)
          (weblocks/hooks:on-session-hook-some-hook
           add-value (param)
           (push param call-result))
          
          (weblocks/hooks:with-some-hook-hook ('blah))
          (weblocks/hooks:with-some-hook-hook ('minor))
          
          (ok (equal call-result
                     '(minor blah)))))))


  (testing "Hook should return last form's value"
    (with-session
      (with-request ("/")
        (let ((result (weblocks/hooks:with-test-action-hook ()
                        'foo
                        'bar)))

          (ok (equal result
                     'bar)))))))


(deftest nested-evaluation
  ;; Here we check if hooks are propertly chained
  (with-session
    (with-request ("/")
      (let (result)
        
        (weblocks/hooks:on-session-hook-some-hook 
         inner-value ()
         (push :inner-before result)
         (call-next-hook)
         (push :inner-after result))
        
        (weblocks/hooks:on-session-hook-some-hook
         outer-value ()
         (push :outer-before result)
         (call-next-hook)
         (push :outer-after result))
          
        (weblocks/hooks:with-some-hook-hook ()
          ;; Now we surrounded this code with hooks
          ;; and will insert another value to the list
          (push :real-value result))

        (ok (equal result
                   '(:outer-after :inner-after :real-value :inner-before :outer-before)))))))


(deftest rewritting-callback
  (testing "If a callback with same name already exists, it is rewritten"
    (with-session
      (with-request ("/")
        (let (result)
          (weblocks/hooks:on-session-hook-test-action foo ()
            (push :foo result)
            (call-next-hook))

          ;; Add a hook with same name, but now it will push :bar
          ;; into the list.
          (weblocks/hooks:on-session-hook-test-action foo ()
            (push :bar result)
            (call-next-hook))

          (weblocks/hooks:with-test-action-hook ()
            t)

          (ok (equal result
                     '(:bar))
              "If callback was overwritten, then we have only :bar in the list."))))))


(deftest automatic-call-next-hook
  (testing "Call-next-hook will be called automatically if it wasn't used in a callback body."
    (with-session
      (with-request ("/")
        (let (result)
          ;; First callback will push :foo
          (weblocks/hooks:on-session-hook-test-action
            foo ()
            (push :foo result))

          ;; Second will push :bar
          (weblocks/hooks:on-session-hook-test-action
            bar ()
            (push :bar result))

          ;; But these callbacks does not call call-next-hook

          (weblocks/hooks:with-test-action-hook ()
            t)

          ;; Neither of two callbacks use call-next-hook,
          ;; but results should contain both :foo and :bar,
          ;; because call-next-hook was called at the end of each callback
          ;; implicitly
          (ok (equal result
                     '(:foo :bar))
              "Call-next-hook should be called implicitly and execute all available hooks."))))))

