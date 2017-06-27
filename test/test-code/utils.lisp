
(in-package :weblocks-test)

;; unfortunate that `ensure-same' doesn't accept lambda expr
(defun set-equal-equal (a b)
  (set-equal a b :test #'equal))

(defun set-equal-uri= (a b)
  (set-equal a b :test #'puri:uri=))

(defun remove-all-methods (gf)
  (mapc (f_ (remove-method gf _)) (copy-list (generic-function-methods gf))))


;;; utilities for easier testing
(defun data-header-template (action body &key (data-class-name "employee") preslots
                             (postslots `((:div :class "submit"
                                                ,(link-action-template action "Modify"
                                                                       :class "modify")))))
  `(:div :class ,(format nil "view data ~(~A~)" data-class-name)
        (:div :class "extra-top-1" "<!-- empty -->")
        (:div :class "extra-top-2" "<!-- empty -->")
        (:div :class "extra-top-3" "<!-- empty -->")
        (:h1 (:span :class "action" "Viewing:&nbsp;")
             (:span :class "object" ,(weblocks::humanize-name data-class-name)))
        ,@preslots
        (:ul ,@body)
        ,@postslots
        (:div :class "extra-bottom-1" "<!-- empty -->")
        (:div :class "extra-bottom-2" "<!-- empty -->")
        (:div :class "extra-bottom-3" "<!-- empty -->")))

