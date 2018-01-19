(defpackage #:weblocks/t/hooks
  (:use #:cl
        #:rove
        #:weblocks/t/utils))
(in-package weblocks/t/hooks)


(deftest empty-call-back-list
  (testing "Callbacks list for unknown name is empty"
    (with-session
      (with-request ("/")
        (ng (weblocks.hooks::get-callbacks
             weblocks.hooks::*session-hooks*
             :some-unknown-name)
            "If no callbacks were added for the name, then get-callbacks should return an empty list.")))))


(deftest add-session-hook
  (with-session
    (with-request ("/")
      (weblocks.hooks:add-session-hook :action foo ())

      (let ((callbacks (weblocks.hooks::get-callbacks-names
                        weblocks.hooks::*session-hooks*
                        :action)))
        (ok (equal (first callbacks)
                   'foo))
        "Function add-session-hook2 should put given value into the list of callbacks bound to the session."))))


(deftest hooks-evaluation
  (testing "Without params"
    (with-session
      (with-request ("/")
        (let (call-result)
          
          (weblocks.hooks:add-session-hook :action
              set-result ()
            (setf call-result
                  'callback-was-called))
          
          (weblocks.hooks:with-hook (:action)
            ;; do nothing
            )

          (ok (equal call-result
                     'callback-was-called))))))

  (testing "With params"
    (with-session
      (with-request ("/")
        (let (call-result)
          (weblocks.hooks:add-session-hook
              :some-hook
              add-value (param)
            (push param call-result))
          
          (weblocks.hooks:with-hook (:some-hook 'blah))
          (weblocks.hooks:with-hook (:some-hook 'minor))

          (ok (equal call-result
                     '(minor blah)))))))


  (testing "Hook should return last form's value"
    (with-session
      (with-request ("/")
        (let ((result (weblocks.hooks:with-hook (:action)
                        'foo
                        'bar)))

          (ok (equal result
                     'bar)))))))


(deftest nested-evaluation
  ;; Here we check if hooks are propertly chained
  (with-session
    (with-request ("/")
      (let (result)
        
        (weblocks.hooks:add-session-hook
            :some-hook
            inner-value ()
          (push :inner-before result)
          (weblocks.hooks:call-next-hook)
          (push :inner-after result))
        
        (weblocks.hooks:add-session-hook
            :some-hook
            outer-value ()
          (push :outer-before result)
          (weblocks.hooks:call-next-hook)
          (push :outer-after result))
          
        (weblocks.hooks:with-hook (:some-hook)
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
          (weblocks.hooks:add-session-hook :my-hook foo ()
            (push :foo result)
            (weblocks.hooks:call-next-hook))

          ;; Add a hook with same name, but now it will push :bar
          ;; into the list.
          (weblocks.hooks:add-session-hook :my-hook foo ()
            (push :bar result)
            (weblocks.hooks:call-next-hook))

          (weblocks.hooks:with-hook (:my-hook)
            t)

          (ok (equal result
                     '(:bar))
              "If callback was overwritten, then we have only :bar in the list."))))))


(deftest automatic-call-next-hook
  (testing "Call-next-hook will be called automatically if it wasn't used in a callback body."
    (with-session
      (with-request ("/")
        (let (result)
          (weblocks.hooks:add-session-hook :my-hook foo ()
            (push :foo result))

          (weblocks.hooks:add-session-hook :my-hook bar ()
            (push :bar result))

          (weblocks.hooks:with-hook (:my-hook)
            t)

          ;; Neither of two callbacks use call-next-hook,
          ;; but results should contain both :foo and :bar,
          ;; because call-next-hook was called at the end of each callback
          ;; implicitly
          (ok (equal result
                     '(:foo :bar))
              "Call-next-hook should be called implicitly and execute all available hooks."))))))

