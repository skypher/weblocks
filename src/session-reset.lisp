(defpackage #:weblocks/session-reset
  (:use #:cl)
  (:import-from #:weblocks/session
                #:*session*)
  (:import-from #:weblocks/hooks
                #:with-hook))
(in-package weblocks/session-reset)


(defun reset-session (session)
  "Internal function for session reset."
  (when session
    (with-hook (:reset-session session)
      (clrhash session))))


;; This function whould be external for weblocks/session package
;; to make it easier for end user
(defun weblocks/session:reset ()
  "Reset current-session"
  (reset-session *session*))
