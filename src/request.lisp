
(in-package :weblocks)

(export '(*json-content-type* initial-request-p
          ajax-request-p pure-request-p redirect post-action-redirect
          post-render-redirect))

(defparameter *json-content-type* "application/json; charset=utf-8"
  "A content type sent to the client to identify json data.")


(defun initial-request-p ()
  "Returns true if the request is the first request for the session."
  (equalp (weblocks.session:get-value 'last-request-uri) :none))

(defun ajax-request-p ()
  "Detects if the current request was initiated via AJAX by looking
for 'X-Requested-With' http header. This function expects to be called
in a dynamic hunchentoot environment."
  (and (header-in* "X-Requested-With")
       (equal "XMLHttpRequest" (header-in* "X-Requested-With"))))

(defun pure-request-p ()
  "Detects if the current request is declared as 'pure', i.e. affects
no widgets or internal application state, but merely is a request for
information. Such requests simply return the result of the function
that represents the action and are used by some AJAX operations to
retreive information (suggest block, etc). When such requests are
satisfied, the actions have access to the session, the widgets, and
all other parameters. However, none of the callbacks (see
*on-pre-request*) are executed, no widgets are sent to the client,
etc."
  (string-equal (weblocks.request:request-parameter "pure") "true"))

(defvar *redirect-request-p* nil)

(defun redirect-request-p ()
  (declare (special *redirect-request-p*))
  (or *redirect-request-p* 
      (weblocks.session:get-value 'redirect-p)))

(defun clear-session-redirect-p ()
  ;; First, set a flag if delayed redirect was requested.
  (setf *redirect-request-p*
        (weblocks.session:get-value 'redirect-p))
  ;; Next, reset this flag in the session
  (weblocks.session:set-value 'redirect-p nil))

(defun clear-redirect-var ()
  (declare (special *redirect-request-p*))
  (setf *redirect-request-p* nil))

(eval-when (:load-toplevel)
  (weblocks.hooks:add-application-hook :post-action 'clear-session-redirect-p )
  (weblocks.hooks:add-application-hook :pre-action 'clear-redirect-var ))


(defun set-redirect-true ()
  (weblocks.session:set-value 'redirect-p t))


(defun redirect (uri &key (defer (and (boundp '*session*) (boundp '*request-hook*)
                                      :post-render))
                          new-window-p (window-title uri))
  "Redirects the client to a new URI.

There are several modes of redirecting:

Immediate redirect (:DEFER NIL): interrupt request processing at once
and send either a `redirect' HTTP response (for normal requests) or
an appropriate JSON command (for AJAX requests).

Deferred redirect (:DEFER (:POST-ACTION|:POST-RENDER); the default
being :POST-RENDER): like immediate redirecting but the execution will be
deferred until action processing (POST-ACTION) or rendering (POST-RENDER)
is finished.

Redirect to new window (NEW-WINDOW=T): opens URI in a new window. The current
request continues to be processed in a normal fashion.
WINDOW-TITLE is the title of the new window, defaulting to the target URI.
DEFER is disregarded in this case.

NEW-WINDOW functionality will only work when Javascript is enabled."
  (assert (member defer '(nil :post-action :post-render)))
  (flet ((do-redirect ()
           (if (weblocks.request:ajax-request-p)
               (weblocks.response:abort-processing
                (format nil "{\"redirect\":\"~A\"}" uri)
                :content-type *json-content-type*)
               (weblocks.response:abort-processing
                ""
                :headers (list :location uri)
                :code 302))))

    (set-redirect-true)

    (cond
      (new-window-p
       (send-script
        (ps:ps*
         `((slot-value window 'open) ,uri ,window-title))))
      ((eq defer :post-action)
       (weblocks.hooks:add-request-hook :post-action #'do-redirect))
      ((eq defer :post-render)
       (weblocks.hooks:add-request-hook :post-render #'do-redirect))
      (t (do-redirect)))))

;;; legacy wrappers for redirect
(defun post-action-redirect (uri)
  "Legacy wrapper; use REDIRECT with :DEFER set to :POST-ACTION instead."
  (redirect uri :defer :post-action))

(defun post-render-redirect (uri)
  "Legacy wrapper; use REDIRECT with :DEFER set to :POST-RENDER instead."
  (redirect uri :defer :post-render))


(defun parse-location-hash ()
  (let ((raw-hash (weblocks.request:request-parameter "weblocks-internal-location-hash")))
    (when raw-hash
      (query-string->alist (cl-ppcre:regex-replace "^#" raw-hash "")))))
