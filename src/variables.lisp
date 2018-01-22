(defpackage #:weblocks/variables
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export #:*current-page-title*
           #:*current-page-keywords*
           #:*current-page-headers*
           #:*rewrite-for-session-urls*
           #:*default-content-type*
           #:*ignore-missing-actions*
           #:*catch-errors-p*
           #:*backtrace-on-session-init-error*
           #:*before-ajax-complete-scripts*
           #:*on-ajax-complete-scripts*
           #:*action-string*
           #:*approved-return-codes*
           #:*style-warn-on-circular-dirtying*
           #:*style-warn-on-late-propagation*))

(in-package weblocks/variables)


(defvar *current-page-title* nil
  "Hold a title of the currently rendered page.")

(defvar *current-page-keywords* nil
  "Hold keywords of the currently rendered page.")

(defvar *current-page-headers* nil
  "A headers list. TODO: make more detailed description.")

(defvar *rewrite-for-session-urls* nil
  "")

;;; Set outgoing encoding to utf-8
(defvar *default-content-type* "text/html; charset=utf-8")

(defvar *ignore-missing-actions* t)


(defvar *catch-errors-p* t
  "If this variable is nil, then weblocks will start lisp's debugger on unhandled conditions.")

(defvar *backtrace-on-session-init-error* t)


(defvar-unbound *before-ajax-complete-scripts*
  "A list of client-side scripts to be sent over to the browser at
   the end of ajax request execution.  TODO when executed?")

(defvar-unbound *on-ajax-complete-scripts*
  "A list of client-side scripts to be sent over to the browser at
   the end of ajax request execution.")


(defvar *action-string* "action"
  "A string used to pass actions from a client to the server. See
  'weblocks/request:get-request-action'.")


(defvar *approved-return-codes* (list 200))


(defparameter *style-warn-on-circular-dirtying* nil
  "Whether to emit a style-warning when widgets are
marked dirty after the rendering phase.")


(defparameter *style-warn-on-late-propagation* nil
  "Whether to emit a style-warning when widgets are
marked dirty in the rendering phase.")
