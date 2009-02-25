;;;; A quick demo hack of a twitter user and/or friends timeline feed  
;;;; widget.  Depends on cl-twitter (cl.net)
;;;; 
;;;; You have to authenticate a user before the widget will work correctly. 

(defwidget twitter-widget ()
  ((history :accessor twitter-history :initarg :history :initform nil)
   (timeout :accessor twitter-timeout :initarg :timeout :initform 300)
   (last-poll :accessor twitter-last-poll :initarg :last-poll :initform 0)
   (maxcount :accessor twitter-max-count :initarg :max :initform 100)
   (page :accessor twitter-page :initarg :page :initform 0)
   (count :accessor twitter-count :initarg :count :initform 5)
   (friends-p :accessor include-friends-p :initarg :friends-p :initform nil)))

(defmethod initialize-instance :after ((widget twitter-widget) &rest initargs)
  (declare (ignore initargs))
  (update-history widget))

(defun update-history (widget)
  "Called on init or page change to cache tweets"
  (with-slots (page count maxcount history) widget
    (when (> page (max-page widget))
      (setf page (max-page widget)))
    (when (or (null history) (twitter-timed-out-p widget))
      (setf history
	    (append (widget-latest widget)
		    history)))))

(defun widget-latest (widget)
  (with-slots (history maxcount) widget
    (sort 
     (if history
	 (append (twitter-op :user-timeline 
			     :since-id (tweet-id (first history)))
		 (when (include-friends-p widget)
		   (twitter-op :friends-timeline
			       :since-id (tweet-id (first history)))))
	 (append (twitter-op :user-timeline
			     :count maxcount)
		 (when (include-friends-p widget)
		   (twitter-op :friends-timeline
			       :count maxcount))))
     #'> :key #'tweet-id)))

(defun max-page (widget)
  (with-slots (history maxcount count) widget
    (/ (min (length history) maxcount) count)))

(defun twitter-timed-out-p (widget)
  (with-slots (timeout last-poll) widget
    (< timeout (- (get-universal-time) last-poll))))

(defun current-tweets (widget)
  (with-slots (page count history) widget
    (let ((page-entries (nthcdr (* page count) history)))
      (subseq page-entries 0 (min count (length page-entries))))))

;;
;; Rendering
;;

(defmethod render-widget-body ((widget twitter-widget) &rest args)
  (declare (ignore args))
  (with-html
    (render-link (f_% (update-history widget)) "Refresh")
    "&nbsp;|&nbsp;"
    (render-link (f_% (setf (include-friends-p widget)
			    (not (include-friends-p widget)))
		      (setf (twitter-history widget) nil)
		      (update-history widget))
		 (if (include-friends-p widget)
		     "User Only"
		     "Show Followed"))
    (:table
     (:tr (:th "Name") (:th "Time") (:th "Message"))
     (dolist (tweet (current-tweets widget))
       (with-html (:tr (:td (:img :src (twitter-user-profile-image-url (tweet-user tweet)))
			    (str (twitter-user-screen-name (tweet-user tweet))))
		       (:td (str (tweet-created-at tweet)))
		       (:td (str (tweet-text tweet)))))))
    (twitter-page-nav widget)))

(defun twitter-page-nav (widget)
  (with-slots (page count maxcount) widget
    (with-html 
      (if (> page 0)
	  (render-link (f_% (decf page) 
			    (update-history widget))
		       "Previous Page")
	  (str "Previous Page"))
      (str "&nbsp; | &nbsp;")
      (if (< page (max-page widget))
	  (render-link (f_% (incf page)
			    (update-history widget))
		       "Next Page")
	  (str "Next Page")))))

