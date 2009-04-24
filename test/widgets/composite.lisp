
(in-package :weblocks-test)

;;; test suite for composite widgets
(deftestsuite composite-suite (weblocks-suite)
  ())

;;; test setting composite widget to single widget on init
(addtest init-composite-single
  (ensure-same (composite-widgets (make-instance 'composite :widgets "foo"))
	       (list "foo")))

;;; test proper setting of widget parent on adding to composite
(addtest composite-add-widget-parent
  (let* ((w (make-instance 'composite))
	 (c (make-instance 'composite :widgets w)))
    (ensure-same (widget-parent w) c)))

;;; widget can be in multiple composites; it's up to the user to ensure
;;; proper parentship
(addtest composite-add-widget-multiple
  (let ((w (make-instance 'composite)))
    (make-instance 'composite :widgets w)
    (make-instance 'composite :widgets w)))

;;; Make sure parents are switched properly
#+(or) ; disabled for now -- see comment to (SETF WIDGET-CHILDREN)
(addtest composite-add-widget-parent-switching
  (let* ((w1 (make-instance 'composite))
	 (w2 (make-instance 'composite))
	 (c (make-instance 'composite :widgets w1)))
    (ensure (widget-parent w1))
    (ensure-null (widget-parent w2))
    (setf (composite-widgets c) w2)
    (ensure-null (widget-parent w1))
    (ensure (widget-parent w2))))

;;; testing render for composite widget
(addtest render-composite
  (let (a-rendered
	b-rendered
	(comp (make-instance 'composite)))
    (push-end (lambda ()
                (setf a-rendered t))
              (composite-widgets comp))
    (push-end (lambda ()
                (setf b-rendered t))
              (composite-widgets comp))
    (render-widget comp)
    (ensure a-rendered)
    (ensure b-rendered)))

