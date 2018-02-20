(defpackage #:weblocks/session-reset
  (:use #:cl)
  (:import-from #:weblocks/session
                #:*session*)
  (:import-from #:weblocks/hooks))
(in-package weblocks/session-reset)


(defun reset-session (session)
  "Internal function for session reset."
  (when session
    (weblocks/hooks:with-reset-session-hook (session)
      (clrhash session))))


;; This function whould be external for weblocks/session package
;; to make it easier for end user
(defun weblocks/session:reset ()
  "Reset current-session"
  (reset-session *session*))
