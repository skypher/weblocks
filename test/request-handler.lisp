
(in-package :weblocks-test)

(deftestsuite request-handler-suite (weblocks-suite)
  ())

;;; testing handle-client-request
(deftest handle-client-request-0
    (with-request :get nil
      (setf (slot-value *request* 'hunchentoot::script-name) "/hello/world/bar.txt")
      (multiple-value-bind (res err)
          (ignore-errors
            (handle-client-request))
        res))
  nil)

(defwebapp hcr-hello-webapp
  :init-user-session 'hcr-init-user-session)
(defmethod initialize-instance :after ((self hcr-hello-webapp) &key &allow-other-keys)
  (setf (weblocks::weblocks-webapp-name self) "HCR Hello"))

#+(or) ;; TODO refactor for cl-who
(deftest handle-client-request-1
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil
        (let (result1 result2 result3
              (app (weblocks::current-webapp)))
          ;; set up our mini-application with one dataform widget
          (defun hcr-init-user-session (comp)
            (push (make-instance 'dataform :data *joe*)
                  (composite-widgets comp)))
          (setf (slot-value *request* 'hunchentoot::uri) "/hcr-hello-webapp/foo/bar")
          ;; handle the first request (make sure data is drawn)
          (setf result1 (handle-client-request app))
          ;; unbind init-user-session to make sure root-composite persists
          (fmakunbound 'hcr1-init-user-session)
          ;; fake user clicking on "modify"
          (setf (slot-value *request* 'get-parameters)
                `((,weblocks::*action-string* . "abc123")))
          ;; handle another request, this time AJAX (make sure form is drawn)
          (setf (slot-value *request* 'hunchentoot::headers-in)
                (cons '("X-Requested-With" . "blah")
                      (slot-value *request* 'hunchentoot::headers-in)))
          (setf result2 (handle-client-request app))
          (values (null (webapp-session-value "debug-reset-sessions"))
                  result1 result2))))
  t
  #.(with-request-template "~
<div class='widget dataform' id='id-123'>~
<div class='view data employee'>~
<div class='extra-top-1'><!-- empty --></div>~
<div class='extra-top-2'><!-- empty --></div>~
<div class='extra-top-3'><!-- empty --></div>~
<h1><span class='action'>Viewing:&nbsp;</span><span class='object'>Employee</span></h1>~
<ul>~
<li class='name'><span class='label text'>Name:&nbsp;</span><span class='value'>Joe</span></li>~
<li class='manager'><span class='label text'>Manager:&nbsp;</span><span class='value'>Jim</span></li>~
</ul>~
<div class='submit'><a class='modify' ~
                       href='/hcr-hello-webapp/foo/bar?action=abc123' ~
                       onclick='initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); ~
                       return false;'>Modify</a></div>~
<div class='extra-bottom-1'><!-- empty --></div>~
<div class='extra-bottom-2'><!-- empty --></div>~
<div class='extra-bottom-3'><!-- empty --></div>~
</div>~
</div>"
      :widget-stylesheets '("dataform")
      :title "HCR Hello - Bar")
  #.(format nil "{\"widgets\":~
{\"id-123\":~
\"<form class='view form employee' action='/hcr-hello-webapp/foo/bar' method='post' ~
      onsubmit='initiateFormAction(\\\"abc124\\\", $(this), \\\"weblocks-session=1%3ATEST\\\"); ~
                return false;'>~
<div class='extra-top-1'><!-- empty --></div>~
<div class='extra-top-2'><!-- empty --></div>~
<div class='extra-top-3'><!-- empty --></div>~
<fieldset><h1><span class='action'>Modifying:&nbsp;</span>~
<span class='object'>Employee</span>~
</h1>~
<h2 class='form-fields-title'>Form fields:</h2>~
<ul><li class='name'><label class='input'>~
<span class='slot-name'><span class='extra'>Name:&nbsp;~
<em class='required-slot'>(required)&nbsp;</em></span></span>~
<input type='text' name='name' value='Joe' maxlength='40' />~
</label>~
</li>~
<li class='manager'><label class='input'>~
<span class='slot-name'><span class='extra'>Manager:&nbsp;</span></span>~
<input type='text' name='manager' value='Jim' maxlength='40' />~
</label>~
</li>~
</ul>~
<div class='submit'>~
<input name='submit' type='submit' class='submit' value='Submit' onclick='disableIrrelevantButtons(this);' />~
<input name='cancel' type='submit' class='submit cancel' value='Cancel' ~
onclick='disableIrrelevantButtons(this);' />~
</div>~
<input name='action' type='hidden' value='abc124' />~
</fieldset>~
<div class='extra-bottom-1'><!-- empty --></div>~
<div class='extra-bottom-2'><!-- empty --></div>~
<div class='extra-bottom-3'><!-- empty --></div>~
</form>\"},~
\"on-load\":null}"))

(defwebapp hcr2-hello-webapp
  :init-user-session 'hcr2-init-user-session
  :dependencies '((:script "weblocks-debug")))

;;; make sure debug toolbar is rendered when appropriate
#+(or) ;; TODO
(deftest handle-client-request-2
    (with-test-webapp (:class-name 'hcr2-hello-webapp)
      (with-request :get nil
        (let (result1 result2
              (app (weblocks::current-webapp)))
          ;; set up our mini-application with one dataform widget
          ;; note: we need to add debug dependencies manually
          ;; here. Normally they would get added by start-weblocks, but
          ;; we're not calling start-weblocks here
;;;       (setf (slot-value app 'weblocks::application-dependencies)
;;;             (append (weblocks::webapp-application-dependencies)
;;;                     (dependencies "debug-toolbar")))
          (setf (weblocks::weblocks-webapp-debug app) t)
          (setf (slot-value *request* 'hunchentoot::uri) "/hcr2-hello-webapp/foo/bar")
          (defun hcr2-init-user-session (comp)
            (setf (composite-widgets comp) (list (lambda () nil))))
          ;; handle the first request (make sure data is drawn)
          (setf result1 (handle-client-request app))
          (fmakunbound 'hcr2-init-user-session)
          (values result1 (not (null (webapp-session-value "debug-reset-sessions")))))))
  #.(with-request-template
            "~
<div class='widget function'>~
</div>" :render-debug-toolbar-p t
:title "hcr2-hello-webapp - Bar")
  t)

;;; make sure navigation controls are modified by request uri
#+(or) ;; TODO refactor for cl-who
(deftest handle-client-request-3
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil
        (let (result (app (weblocks::current-webapp)))
          ;; set up our mini-application with one navigation widget
          (defun hcr-init-user-session (comp)
            (setf (composite-widgets comp)
                  (list (make-navigation "test-nav"
                                         "test1" (lambda (&rest args)
                                                   (with-html (:div "hi1")))
                                         "test2" (lambda (&rest args)
                                                   (with-html (:div "hi2")))))))
          ;; set the URI
          (setf (slot-value *request* 'hunchentoot::uri) "/hcr-hello-webapp/test2?action=blah")
          ;; handle the request
          (setf result (handle-client-request app))
          (fmakunbound 'hcr-init-user-session)
          result)))
  #.(with-request-template
            "~
<div class='selector-mixin widget dispatcher selector navigation' id='test-nav'>~
<div class='navigation-body'>~
<div class='widget function'>~
<div>hi2</div>~
</div>~
<div class='view menu'>~
<div class='extra-top-1'><!-- empty --></div>~
<div class='extra-top-2'><!-- empty --></div>~
<div class='extra-top-3'><!-- empty --></div>~
<h1>Test Nav</h1>~
<ul>~
<li><a href='/hcr-hello-webapp/'>Test1</a></li>~
<li class='selected-item'><span>Test2</span></li>~
</ul>~
<div class='extra-bottom-1'><!-- empty --></div>~
<div class='extra-bottom-2'><!-- empty --></div>~
<div class='extra-bottom-3'><!-- empty --></div>~
</div>~
</div>~
</div>"
      :widget-stylesheets '("navigation")
      :title "HCR Hello - Test2"))

(deftest handle-client-request-4
    (with-request :get nil
      (let (result1 result2 result3)
        ;; set the uri
        (setf (slot-value *request* 'hunchentoot::uri) "/")
        ;; make sure we handle cookies
        (setf *session* nil)
        (catch 'hunchentoot::handler-done
          (handle-client-request (weblocks::current-webapp)))
        (string-downcase (header-out :location))))
  "http://nil/?weblocks-session=1%3atest")

(deftest handle-client-request-5
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil :uri "/hcr-hello-webapp/"
        (let* ((res 0) (inc-res (lambda () (incf res)))
               pushed-pre pushed-post)
          ;; set up our mini-application
          (defun hcr-init-user-session (comp) nil)
          ;; start the session
          (start-session)
          ;; do the test
          (unwind-protect
               (progn
                 (push inc-res (request-hook :application :pre-action))
                 (setf pushed-pre t)
                 (push inc-res (request-hook :application :post-action))
                 (setf pushed-post t)
                 (handle-client-request (weblocks::current-webapp)))
            (when pushed-post
              (pop (request-hook :application :post-action)))
            (when pushed-pre
              (pop (request-hook :application :pre-action))))
          ;; tear down the application
          (fmakunbound 'hcr-init-user-session)
          res)))
  2)

(deftest handle-client-request-6
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil :uri "/hcr-hello-webapp/"
        (let ((res 0))
          ;; set up our mini-application
          (declare (special *request-hook*))
          (defun hcr-init-user-session (comp) nil)
          ;; start the session
          (start-session)
          ;; do the test
          (push (lambda ()
                  (incf res))
                (request-hook :session :pre-render))
          (push (lambda ()
                  (incf res))
                (request-hook :session :post-render))
          (handle-client-request (weblocks::current-webapp))
          ;; tear down the application
          (fmakunbound 'hcr-init-user-session)
          res)))
  2)

(deftest handle-client-request-7
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil :uri "/hcr-hello-webapp/"
        (let ((res 0))
          ;; start the session
          (start-session)
          ;; set up our mini-application
          (defun hcr-init-user-session (comp)
            (push (lambda ()
                    (incf res))
                  (request-hook :request :pre-action))
            (push (lambda ()
                    (incf res))
                  (request-hook :request :post-render)))
          ;; do the test
          (handle-client-request (weblocks::current-webapp))
          ;; tear down the application
          (fmakunbound 'hcr-init-user-session)
          res)))
  2)

(deftest handle-client-request-8
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get `(("pure" . true) (,weblocks::*action-string* . "abc123"))
          :uri "/hcr-hello-webapp/"
        (let ((res 0))
          ;; start the session
          (start-session)
          ;; action
          (make-action (lambda (&rest args)
                         (incf res)))
          ;; set up our mini-application
          (defun hcr-init-user-session (comp)
            (push (lambda ()
                    (incf res))
                  (request-hook :request :pre-action))
            (push (lambda ()
                    (incf res))
                  (request-hook :request :post-render)))
          ;; do the test
          (catch 'hunchentoot::handler-done
            (handle-client-request (weblocks::current-webapp)))
          ;; tear down the application
          (fmakunbound 'hcr-init-user-session)
          res)))
  1)

#+(or)
(deftest handle-client-request-9
    (with-test-webapp (:class-name 'hcr-hello-webapp)
      (with-request :get nil :uri "/hcr-hello-webapp/"
        (let (result1)
          ;; set up our mini-application with one dataform widget
          (defun hcr-init-user-session (comp)
            (setf (composite-widgets comp)
                  (list (lambda ()
                          (declare (special *current-page-description*))
                          (setf *current-page-description* "Some Page")))))
          ;; handle the first request (make sure data is drawn)
          (setf result1 (handle-client-request (weblocks::current-webapp)))
          (fmakunbound 'hcr-init-user-session)
          result1)))
  #.(with-request-template
        "<div class='widget function'></div>"
      :title "HCR Hello - Some Page"))

(deftest handle-client-request-10
    (with-request :get '(("action" . "abc123"))
      (let ((res 0))
        ;; set up mini application
        (declare (special *request-hook*))
        (start-session)
        ;; set the uri
        (setf (slot-value *request* 'hunchentoot::uri) "/")
        ;; prepare action
        (make-action (lambda (&rest args)
                       nil)
                     "abc123")
        ;; prepare hooks
        (push (lambda ()
                (incf res))
              (request-hook :session :pre-action))
        (push (lambda ()
                (incf res))
              (request-hook :session :post-action))
        ;; prepare dummy app
        (setf (root-composite) (make-instance 'composite))
        ;; make sure we redirect to hide ugly URLs
        (catch 'hunchentoot::handler-done
          (handle-client-request (weblocks::current-webapp)))
        ;; result
        (values (string-downcase (header-out :location)) res)))
  "http://nil/?weblocks-session=1%3atest" 2)

(deftest handle-client-request-11
    (with-request :get '(("action" . "abc123"))
      (make-request-ajax)
      (start-session)
      ;; set the uri
      (setf (slot-value *request* 'hunchentoot::uri) "/")
      ;; prepare action
      (make-action (lambda (&rest args)
                     nil)
                   "abc123")
      ;; prepare dummy app
      (setf (root-composite) (make-instance 'composite))
      ;; make sure we redirect to hide ugly URLs
      (catch 'hunchentoot::handler-done
        (handle-client-request (weblocks::current-webapp)))
      ;; result
      (header-out :location))
  nil)

(deftest handle-client-request-12
    (with-request :get nil
      ;; set up mini application
      (start-session)
      ;; set the uri
      (setf (slot-value *request* 'hunchentoot::uri) "/")
      ;; prepare dummy app
      (setf (root-composite) (make-instance 'composite))
      ;; make sure we redirect to hide ugly URLs
      (catch 'hunchentoot::handler-done
        (handle-client-request (weblocks::current-webapp)))
      ;; clean up app
      (fmakunbound 'init-user-session)
      ;; result
      (header-out :location))
  nil)

(defmethod begin-transaction :after (store)
  (declare (special *res*))
  (when (and (eql store *default-store*)
             (boundp '*res*))
    (incf *res*)))

(defmethod commit-transaction :after (store)
  (declare (special *res*))
  (when (and (eql store *default-store*)
             (boundp '*res*))
    (incf *res*)))

(defmethod rollback-transaction :after (store)
  (declare (special *res*))
  (when (and (eql store *default-store*)
             (boundp '*res*))
    (decf *res*)))

(deftest handle-client-request-13
    (with-request :post '(("action" . "abc123"))
      (let ((*res* 0))
        ;; set up mini application
        (declare (special *request-hook* *res*))
        (make-request-ajax)
        (start-session)
        ;; set the uri
        (setf (slot-value *request* 'hunchentoot::uri) "/")
        ;; prepare action
        (make-action (lambda (&rest args)
                       nil)
                     "abc123")
        ;; prepare dummy app
        (setf (root-composite) (make-instance 'composite))
        ;; handle the request
        (catch 'hunchentoot::handler-done
          (handle-client-request (weblocks::current-webapp)))
        ;; result
        *res*))
  2)

(deftest handle-client-request-14
    (with-request :get '(("action" . "abc123"))
      (let ((weblocks::*render-debug-toolbar* nil) (*res* 0))
        ;; set up mini application
        (declare (special *request-hook* *res*))
        (make-request-ajax)
        (start-session)
        ;; set the uri
        (setf (slot-value *request* 'hunchentoot::uri) "/")
        ;; prepare action
        (make-action (lambda (&rest args)
                       (error "foo"))
                     "abc123")
        ;; prepare dummy app
        (setf (root-composite) (make-instance 'composite))
        ;; handle the request
        (catch 'hunchentoot::handler-done
          (ignore-errors
            (handle-client-request (weblocks::current-webapp))))
        ;; clean up app
        (fmakunbound 'init-user-session)
        ;; result
        *res*))
  0)

(defwebapp broken-init
  :init-user-session 'broken-init-user-session)

(defun broken-init-user-session (rootcomp)
  (cerror "keep going" "oopsie I messed up")
  (setf (composite-widgets rootcomp) '("hello there")))

#+(or) ; disabled until Andrea's error handling patch is applied.
(addtest allow-restart-in-sessinit
  (with-test-webapp (:class-name 'broken-init)
    (with-request :get nil :uri "/broken-init/"
      (let ((finished-handler 0)
            (*catch-errors-p* nil))
        (handler-bind
            ((simple-error
              (lambda (sig)
                (ensure-same (format nil "~A" sig) "oopsie I messed up")
                (let ((restart (find-restart 'continue)))
                  (ensure restart)
                  (incf finished-handler)
                  (invoke-restart restart)))))
          (catch 'hunchentoot::handler-done
            (handle-client-request (weblocks::current-webapp))))
        (ensure-same finished-handler 1)
        (ensure-same (composite-widgets (root-composite))
                     '("hello there"))))))

;;; test remove-session-from-uri
(deftest remove-session-from-uri-1
    (with-request :get nil
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah")

(deftest remove-session-from-uri-2
    (with-request :get '(("action" . "test"))
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah?action=test")

(deftest remove-session-from-uri-3
    (with-request :get '(("action" . "test") ("weblocks-session" "123"))
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah?action=test")

;;; test remove-action-from-uri
(deftest remove-action-from-uri-1
    (with-request :get '(("action" . "test") ("weblocks-session" . "123"))
      (weblocks::remove-action-from-uri "/pub/test/blah"))
  "/pub/test/blah?weblocks-session=123")

;;; test render-dirty-widgets
(deftest render-dirty-widgets-1
    (with-request :get nil
      (let ((weblocks::*dirty-widgets* (list (make-instance 'composite)
                                             (make-instance 'composite :name 'foo-bar)))
            (*weblocks-output-stream* (make-string-output-stream))
            (*on-ajax-complete-scripts* (list "testjs")))
        (declare (special weblocks::*dirty-widgets*
                          *weblocks-output-stream* *on-ajax-complete-scripts*))
        (weblocks::render-dirty-widgets)
        (get-output-stream-string *weblocks-output-stream*)))
  #.(format nil "~
{~
\"widgets\":~
{~
\"id-123\":\"<div class='widget composite' id='id-123'><\\/div>\",~
\"foo-bar\":\"<div class='widget composite' id='foo-bar'><\\/div>\"~
},~
\"before-load\":null,~
\"on-load\":[\"testjs\"]~
}"))

(deftest render-dirty-widgets-2
    (with-request :get nil
      (let* ((dirty-child (make-instance 'widget))
             (parent (make-instance 'composite 
                                    :widgets (list dirty-child)))
             (weblocks::*dirty-widgets* (list dirty-child parent))
             (*weblocks-output-stream* (make-string-output-stream))
             (*on-ajax-complete-scripts* (list "testjs")))
        (declare (special weblocks::*dirty-widgets*
                          *weblocks-output-stream* *on-ajax-complete-scripts*))
        (weblocks::render-dirty-widgets)
        (get-output-stream-string *weblocks-output-stream*)))
  #.(format nil "~
{~
\"widgets\":~
{\"id-123\":\"<div class='widget composite' id='id-123'><div class='widget' id='id-123'><\\/div><\\/div>\"},~
\"before-load\":null,~
\"on-load\":[\"testjs\"]~
}"))

(defwidget dirtier ()
  ((other :initarg :other)))

(defmethod render-widget-body ((wij dirtier) &key &allow-other-keys)
  (mark-dirty (slot-value wij 'other))
  (princ 42 *weblocks-output-stream*))

(addtest detect-dirty-circularity
  (ensure-same (symbol-status 'dirtier) :internal)
  (symbol-macrolet ((d weblocks::*dirty-widgets*))
    (let* ((a (make-instance 'dirtier :dom-id "a"))
           (b (make-instance 'dirtier :other a :dom-id "b"))
           (weblocks::*dirty-widgets* '()))
      (declare (special weblocks::*dirty-widgets*))
      (setf (slot-value a 'other) b)
      (setf d (list a b))
      (weblocks::render-dirty-widgets)
      (ensure-null d)
      (ensure-same (get-output-stream-string *weblocks-output-stream*)
                   #.(format nil "~
{~
\"widgets\":~
{~
\"a\":\"<div class='widget dirtier' id='a'>42<\\/div>\",~
\"b\":\"<div class='widget dirtier' id='b'>42<\\/div>\"~
},~
\"before-load\":null,~
\"on-load\":null~
}")))))
