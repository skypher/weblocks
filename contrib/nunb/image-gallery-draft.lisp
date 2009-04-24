(in-package :app)

(defwidget image-gallery ()
  ((thumbnails-only :accessor get-thumbnails-only :initarg :thumbnails-only :initform t)
   (images :accessor gallery-images :initarg :images :initform nil)))

(defmethod render-widget-body ((obj image-gallery) &rest args)
  (declare (ignore args))
  (with-html
    (:script :type "text/javascript"
              (fmt "~%// <![CDATA[~%")
              (fmt "function switchPhoto (photourl) { ~% document.images.imgPhoto.src= photourl; }" )
              #+ORIG(fmt "function switchPhoto (photourl) { ~% document.images.imgPhoto.src=\"/pub/photos/\" + photourl; }" )
              (fmt "~%// ]]>~%"))
    (let* ((images (gallery-images obj)))
      (when images
;	(break "got here")
	(htm (:div (:span (:p (format nil "~{~A~}" images)))))
	(htm (:div :class "photo-gallery"
		   (htm (:div :style (if (get-thumbnails-only obj)
					 "display: none;"
					 "display: block")
			      :class "photo-main" 
			      ;(:p "Photograph Full Size") 
			      (:img :src (first images) :width "600"  :id "imgPhoto")))
		   (:div :class "photo-thumbs" ; :style "text-align:center;"
			 ;(:p "Photographs thumbnails")
			 (dolist (i images)
			   (render-gallery-link i)))))))))

; renders great, needs the lbon js css to be loaded, else degrades to full page load.
(defun render-gallery-link (img &key (ajaxp t))
  (let* ((action-code (weblocks::function-or-action->action (closure
							      (with-html-output-to-string (st)
								(htm (:div ;"Inside Lightbox Div is here"
									   (:img :src img)
									   (:a :href "#" :class "lbAction" :rel "deactivate" "Return")))))))
	 (url (make-action-url action-code))) 
    (with-html 
      (:img :src img :height "90px")
      (:a :href (string+ url "&pure=true") 
	  :class "lbOn" "Click to view"))
    (send-script "mylightboxinitialize();")))

;weird old shit, action is ok, but the bottom with0html makesn o sense
(defun old-shitrender-gallery-link (url &key (ajaxp t))
  (let* (	 
	 (action-code (weblocks::function-or-action->action (closure
							      (with-html-output-to-string (st)
								(htm (:div "Div Starts"
									   (:img :src url)
									   (:a :href "#" :class "lbAction" :rel "deactivate" "Return"))))
							      ;(get-output-stream-string st)
							      )))
	 (url (make-action-url action-code))

	 #+OLD(show-this-photo (make-action (closure "weep")))) 
    (with-html 
      (:img :src url :height "90px")
      (:a :href (str (format nil "~A" (string+ url "&pure=true") ))
	  :class "lb0n" "Click to view"))))

;;Renders main photo area in full, others as small Js links. Or if js is off, goes to url.
(defun js-small-big-render-gallery-link (url &key (ajaxp t))
    (with-html
      (:a :href (str url)
	  :onclick (when ajaxp
		     (format nil "switchPhoto(\"~A\"); return false;"
			     url))
	  (htm (:img :src url :height "60px")))))


;; works fine, looks ugly.
(defun x-render-gallery-link (url &key (ajaxp t))
  (let ((po (make-instance 'photo
			   :url url 
			   :description "Enter Description")))
;    (break (format nil "URL is ~A" url))
    (with-html 
      (:img :src url :height "90px")
      (render-link (lambda (&rest args)
		     (do-dialog "View Photo"    
		       #+OLD(make-quickform 'photo-base-view :data po
				       :on-success (closure (answer w))
				       :on-cancel (closure (answer w))) 
		       ))
	       "Click to View"))))