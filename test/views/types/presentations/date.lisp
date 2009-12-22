(in-package #:weblocks-test)

(deftestsuite views/types/presentations/date-suite (weblocks-suite)
  ())

(defclass some-dates ()
  ((x :initform (encode-universal-time 0 0 12 10 1 1984))
   (y :initform (encode-universal-time 0 0 12 3 4 1983))
   (z :initform (encode-universal-time 0 0 12 1 12 1966))))

(defview some-dates (:inherit-from '(:scaffold some-dates))
  (x :present-as date)
  (y :present-as (date :format "%m/%d/%Y"))
  (z :present-as (date :format "%A, %B %d, %Y")))

(addtest print-dates
  (ensure-html-output
   (render-object-view (make-instance 'some-dates) (find-view 'some-dates))
   #.(data-header-template
      nil '((:li :class "x"
	     (:span :class "label date" "X:&nbsp;")
	     (:span :class "value" "1984-01-10"))
	    (:li :class "y"
	     (:span :class "label date" "Y:&nbsp;")
	     (:span :class "value" "04/03/1983"))
	    (:li :class "z"
	     (:span :class "label date" "Z:&nbsp;")
	     (:span :class "value" "Thursday, December 01, 1966")))
      :data-class-name 'some-dates :postslots '())))
