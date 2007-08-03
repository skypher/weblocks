
(in-package :weblocks)

(export '(refresh-request-p initial-request-p ajax-request-p
	  pure-request-p))

(defun refresh-request-p ()
  "Determines if a request is a result of the user invoking a browser
refresh function. Note that a request will not be considered a refresh
if there is an action involved (even if the user hits refresh)."
  (declare (special *uri-tokens*))
  (and
   (null (get-request-action))
   (equalp *uri-tokens* (session-value 'last-request-uri))))

(defun initial-request-p ()
  "Returns true if the request is the first request for the session."
  (equalp (session-value 'last-request-uri) :none))

(defun ajax-request-p ()
  "Detects if the current request was initiated via AJAX by looking
for 'X-Requested-With' http header. This function expects to be called
in a dynamic hunchentoot environment."
  (header-in "X-Requested-With"))

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

