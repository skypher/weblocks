(in-package :app)

(defwidget poncy (composite)
  ((width :accessor poncy-width :initarg :width)
   (title :accessor poncy-title :initarg :title)))

(defmethod render-widget-body ((obj poncy) &rest args)
  (with-html (:div :style "margin: 0 auto; text-align:center;"
		   (render-image (my-tab-image "tabs" (poncy-title obj) (poncy-width obj))))
	     (:div :style 
		 (dolist (b (composite-widgets obj)) 
			   (render-widget-body b)))))


#+OLD(defun make-text-tab (width file text)
 (let* ((y 13))
  (with-canvas (:width width :height (* 2 y))
    (let ((font (get-font "/tmp/font.ttf"))
          (step (/ pi 7)))
      (set-font font 12))
    (let ((x width) (2y (* 2 y)))
      (set-rgba-fill  0.3 0.3 0.3 0.3)
      (rounded-rectangle 0 0 x 2y 10 10 )
      (set-gradient-fill  2 2y
			  0.4 0.4 0.4 0.8
			  2 y
			  0 0 0 0.2)
      (set-gradient-fill  2 y
			  0.4 0.4 0.4 0.9
			  2 0
			  0 0 0 0.2)

      ;(clip-path)      
      ;(rectangle 0 0 x y)
      (clip-path)
      (centered-circle-path 2 2 10)
      (centered-circle-path x 2 10)
      (fill-path)
      (rgba-fill 255 255 255  1.0)
      (vecto::translate (/ x 2) 8)
      (draw-centered-string 0 0 text)
      (fill-path)
      (save-png file)))))