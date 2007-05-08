
(in-package :weblocks)

(defun render-debug-toolbar ()
  "When weblocks is started in debug mode, called on every request to
present the user with a toolbar that aids development."
  (with-html
    (:div :class "debug-toolbar"
	  (:a :href (make-action-url "debug-reset-sessions")
	      :title "Reset Sessions"
	      (:img :src "/pub/images/reset.png"
		    :alt "Reset Sessions")))))

(defun initialize-debug-actions ()
  "When weblocks is started in debug mode, called on session
initalization so that appropriate actions that aid development can be
created."
  (make-action (lambda ()
		 (reset-sessions)
		 (redirect "/"))
	       "debug-reset-sessions"))
