(in-package :app)
 
(defclass calendar-presentation (input-presentation)
  ())

(defmethod dependencies append ((cp calendar-presentation))
  ; (break "Yay appending dependencies!") 
  ; well we get here but there is no sign of them in the browser..
  ; Perhaps these dependencies are being loaded *after* the jscode in the field render?
  ; Or else, the dom-load event does not wait for these dependencies to be loaded?
  ; Answer (partial) : on a normal refresh everything works, so the problem must be ajax-loading.
  (list 
   (make-yui-dependency :stylesheet  "fonts/fonts-min")
   (make-yui-dependency :stylesheet "button/assets/skins/sam/button")
   (make-yui-dependency :stylesheet "container/assets/skins/sam/container")
   (make-yui-dependency :stylesheet "calendar/assets/skins/sam/calendar")
   (make-yui-dependency :script "yahoo-dom-event/yahoo-dom-event") 
   (make-yui-dependency :script "dragdrop/dragdrop-min")
   (make-yui-dependency :script "element/element-beta-min")
   (make-yui-dependency :script  "button/button-min")      
   (make-yui-dependency :script  "container/container-min")   
   (make-yui-dependency :script "calendar/calendar-min")
   (make-yui-dependency :stylesheet "nandan/nunb-calendar")
))

;; Little snippet from Prototype to replace YUI code for on Event
;;  $ ('show').observe ('click', function (event){                                                                                                                                   
;;                 dialog.show ();
;;          }); 

;; Best way to use observe and on load in Prototype
;;
;; document.observe ("dom:loaded", function () {
;;      // do what needs to be done, eg. below
;;   $$ ('div.tabcontent').invoke ('hide');
;; });


;; Show YUI calendar, different types
(defmethod render-view-field-value  (value (presentation calendar-presentation)
                                    (field form-view-field) (view form-view) widget obj
                                    &rest args &key intermediate-values &allow-other-keys)
  (let ((cal-button-id         (gensym "calbtn"))
        (yui-container-div     (gensym "yuicontainer"))
	(cal-div               (gensym "cal"))
	(a-slot-name           (attributize-name (view-field-slot-name field))))
    (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
      (with-html
       	(:div :class "yui-skin-sam"
	      (:div 
		    (:div :class "datefield"
			  (:input :type "text" :name a-slot-name :id a-slot-name :readonly "yes"
							    :value (if intermediate-value-p
								       intermediate-value
								       (apply #'print-view-field-value value presentation field view widget obj args))
							    :maxlength (input-presentation-max-length presentation))
			  #+OLD (:button :type "button" :id cal-button-id :title "Show Calendar" (:img :src "/pub/images/cal.png" :height "18" :width "18")))
		    (:div :id yui-container-div :class "yuicalcontainer" ; :style "display:none;"
			   (:div :id cal-div :class "innercalendar")))))

      ;; (yui) calendar inside frame, but clickable and should update field itself. ;metatilities:date-string
      (send-script (format nil "
			var dialog, calendar;
			calendar = new YAHOO.widget.Calendar('~A', {
			    iframe:true,          // Turn iframe off, since container has iframe support.
			    hide_blank_weeks:true  // Enable, to demonstrate how we handle changing height, using changeContent
			});
                        function handleSelect(type,args,obj) { 
                          var dates = args[0];
                          var date = dates[0]; ; ;
                          var year = date[0].toString().substr(2);
                          var month = date[1], day = date[2]; ; ;
                          var txtDate1 = document.getElementById('~A'); ; ;
                          txtDate1.value = day + '/' + month + '/' + year; ; ;
                          //txtDate1.value = day + '-' + month + '-' + year; ; ;
                          //txtDate1.value = year + '-' + month + '-' + day;  
                          dialog.hide();
                        }  
                        calendar.selectEvent.subscribe(handleSelect, calendar, true);  
			dialog = new YAHOO.widget.Dialog('~A', {
			    context:['~A', 'tl', 'bl'],
                            //fixedcenter:true,
			    width:'16em',  // Sam Skin dialog needs to have a width defined (7*2em + 2*1em = 16em).
			    draggable:true,
			    close:true
			});
			calendar.render();
			dialog.render();
			// Using dialog.hide() instead of visible:false is a workaround for an IE6/7 container known issue with border-collapse:collapse.
			dialog.hide();
			calendar.renderEvent.subscribe(function() {
			    // Tell Dialog it's contents have changed, Currently used by container for IE6/Safari2 to sync underlay size
			  //  dialog.fireEvent('changeContent');
			});
	                YAHOO.util.Event.on('~A', 'click', function() {
			    dialog.show();
			   if (YAHOO.env.ua.opera && document.documentElement) {
				// Opera needs to force a repaint
				document.documentElement.style += '';
			    } 
			});
                        YAHOO.util.Event.on('~A', 'click', function() {
			    dialog.show();
			   if (YAHOO.env.ua.opera && document.documentElement) {
				// Opera needs to force a repaint
				document.documentElement.style += '';
			    } 
			});"
			   cal-div
			   a-slot-name
			   yui-container-div
			   a-slot-name
			   cal-button-id
			   a-slot-name)))))

 


(defmethod render-view-field-value (value (presentation calendar-presentation)
                                    field view widget obj &rest args)
  (declare (ignore args))
  (with-html
    (:span value)))

 
(defclass calendar-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser calendar-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  ;(break "My parser was called")
  ;(break (format nil "Called value ~A tipval ~A" value (text-input-present-p value))) 
  (if (text-parser-matches parser)
      (when (ppcre:all-matches (text-parser-matches parser) value)
        (values t (text-input-present-p value) (alex-date-string-yui value)))
      (values t (text-input-present-p value) (alex-date-string-yui value) )))


;; ######################################################################################
;; ############## The two render-view-field methods are moved down here #################
;; ##################### suspect they're never called by weblocks #######################
;; ######################################################################################

(defmethod render-view-field ((field form-view-field) (view form-view)
                              widget (presentation calendar-presentation) value obj
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc attribute-slot-name validation-errors
                                  :test #'string-equal
                                  :key #'view-field-slot-name))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
           (:span :class "label"
                  (:span :class "slot-name"
                         (:span :class "extra"
				 ;(str (view-field-label field)) ":&nbsp;")
                                 (when (form-view-field-required-p field)
                                   (htm (:em :class "required-slot" "(required)&nbsp;")))
                                )))
           (apply #'render-view-field-value
                  value presentation
                  field view widget obj
                  args)
           (when validation-error
             (htm (:p :class "validation-error"
                      (:em
                       (:span :class "validation-error-heading" "Error:&nbsp;")
                       (str (format nil "~A" (cdr validation-error)))))))))
))

;; For template form, just remove the label that we usually show.
(defmethod render-view-field ((field form-view-field) (view templform-view)
                              widget (presentation calendar-presentation) value obj
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc attribute-slot-name validation-errors
                                  :test #'string-equal
                                  :key #'view-field-slot-name))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
           (:span :class "label"
                  (:span :class "slot-name"
                         (:span :class "extra"
				(str (view-field-label field)) ":&nbsp;"
                                 (when (form-view-field-required-p field)
                                   (htm (:em :class "required-slot" "(required)&nbsp;")))
                                )))
           (apply #'render-view-field-value
                  value presentation
                  field view widget obj
                  args)
           (when validation-error
             (htm (:p :class "validation-error"
                      (:em
                       (:span :class "validation-error-heading" "Error:&nbsp;")
                       (str (format nil "~A" (cdr validation-error)))))))))
))
