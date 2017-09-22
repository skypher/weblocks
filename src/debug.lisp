(defpackage #:weblocks.debug
  (:use #:cl))
(in-package weblocks.debug)


;; TODO: move useful staff from debug-mode.lisp, to this package


(defvar *on* nil
  "When true, then Weblocks will be saving addional information useful
for debugging.")


(defvar *latest-session* nil
  "Stores last session, to be able to clear it during development.

To clear, use function \(reset-last-session\).")


(defun reset-latest-session ()
  (when *latest-session*
    (weblocks.hooks:with-hook (:reset-session *latest-session*)
                              (clrhash *latest-session*))))



;; This piece will store
(weblocks.hooks:add-application-hook :handle-request
    track-latest-session ()

  (when *on*
   (setf weblocks.session::*latest-session*
         weblocks.session::*session*)))
