
(in-package :weblocks)

(export '(*json-content-type* refresh-request-p initial-request-p
	  ajax-request-p pure-request-p redirect post-action-redirect
	  post-render-redirect))

(defparameter *json-content-type* "application/json; charset=utf-8"
  "A content type sent to the client to identify json data.")

(defun refresh-request-p ()
  "Determines if a request is a result of the user invoking a browser
refresh function. Note that a request will not be considered a refresh
if there is an action involved (even if the user hits refresh)."
  (declare (special *uri-tokens*))
  (and
   (null (get-request-action))
   (equalp (all-tokens *uri-tokens*) (webapp-session-value 'last-request-uri))))

(defun initial-request-p ()
  "Returns true if the request is the first request for the session."
  (equalp (webapp-session-value 'last-request-uri) :none))

(defun ajax-request-p ()
  "Detects if the current request was initiated via AJAX by looking
for 'X-Requested-With' http header. This function expects to be called
in a dynamic hunchentoot environment."
  (header-in* "X-Requested-With"))

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
  (string-equal (get-parameter "pure") "true"))

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
           (if (ajax-request-p)
             (progn
               (setf (content-type*) *json-content-type*)
               (abort-request-handler
                 (format nil "{\"redirect\":\"~A\"}" uri)))
             (hunchentoot:redirect uri))))
    (cond
      (new-window-p
        (send-script
          (ps:ps*
            `((slot-value window 'open) ,uri ,window-title))))
      ((eq defer :post-action)
       (push #'do-redirect (request-hook :request :post-action)))
      ((eq defer :post-render)
       (push #'do-redirect (request-hook :request :post-render)))
      (t (do-redirect)))))

;;; legacy wrappers for redirect
(defun post-action-redirect (uri)
  "Legacy wrapper; use REDIRECT with :DEFER set to :POST-ACTION instead."
  (redirect uri :defer :post-action))

(defun post-render-redirect (uri)
  "Legacy wrapper; use REDIRECT with :DEFER set to :POST-RENDER instead."
  (redirect uri :defer :post-render))

