
(in-package :weblocks-test)

;;; test refresh-request-p
(deftest refresh-request-p-1
    (with-request :get nil
      (setf (webapp-session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  t)

(deftest refresh-request-p-2
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) '("foo"))
      (refresh-request-p))
  nil)

(deftest refresh-request-p-3
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (setf (session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  nil)

;;; test initial-request-p
(deftest initial-request-p-1
    (with-request :get nil
      (initial-request-p))
  nil)

(deftest initial-request-p-2
    (with-request :get nil
      (setf (session-value 'last-request-uri) :none)
      (initial-request-p))
  nil)

;;; test ajax-request-p
(deftest ajax-request-p-1
    (with-request :get nil
      (ajax-request-p))
  nil)

(deftest ajax-request-p-2
    (with-request :get nil
      (make-request-ajax)
      (ajax-request-p))
    t)

;;; test pure-request-p
(deftest pure-request-p-1
    (with-request :get nil
      (not (null (pure-request-p))))
  nil)

(deftest pure-request-p-2
    (with-request :get '(("pure" . blah))
      (not (null (pure-request-p))))
  nil)

(deftest pure-request-p-3
    (with-request :get '(("pure" . true))
      (not (null (pure-request-p))))
  t)

;;; test redirect
(deftest redirect-1
    (with-request :get nil
      (catch 'hunchentoot::handler-done
        (redirect "/foo")
        1))
  nil)

(deftest redirect-2
    (with-request :get nil
      (make-request-ajax)
      (catch 'hunchentoot::handler-done
        (redirect "/foo")
        1))
  "{\"redirect\":\"/foo\"}")

(addtest redirect-post-action
  (let ((weblocks:*request-hook* (make-instance 'weblocks::request-hooks)))
    (redirect "/foo" :defer :post-action)
    (ensure (weblocks::post-action-hook *request-hook*))))

(addtest redirect-post-render
  (let ((weblocks:*request-hook* (make-instance 'weblocks::request-hooks)))
    (redirect "/foo" :defer :post-render)
    (ensure (weblocks::post-render-hook *request-hook*))))

(addtest redirect-post-action-legacy-wrapper
  (let ((weblocks:*request-hook* (make-instance 'weblocks::request-hooks)))
    (post-action-redirect "/foo")
    (ensure (weblocks::post-action-hook *request-hook*))))

(addtest redirect-post-render-legacy-wrapper
  (let ((weblocks:*request-hook* (make-instance 'weblocks::request-hooks)))
    (post-render-redirect "/foo")
    (ensure (weblocks::post-render-hook *request-hook*))))

(addtest redirect-open-new-window
  (with-request :get nil
    (redirect "/foo" :new-window-p t))
  (ps:ps*
    `((slot-value window 'open) "/foo" "/foo")))

(addtest redirect-open-new-window-custom-title
  (with-request :get nil
    (redirect "/foo" :new-window-p t :window-title "bar"))
  (ps:ps*
    `((slot-value window 'open) "/foo" "bar")))

