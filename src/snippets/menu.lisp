
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entires."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu (options &key selected-pane header (container-id (gen-id))
                    ordered-list-p (empty-message *menu-empty-message*)
                    disabled-pane-names)
  "Renders a menu snippet based on given options and selected
option. An option may be a dotted pair of a label and URL to link to,
or a name (which will be converted to a label and a URL via
humanize-name and attributize-name, respectively). The selected-uri
will be compared to an option's URL tokens via equalp. If the selected
option isn't specified, first option is rendered as selected.  If
CONTAINER-ID is provided, it is used as the basis of DOM IDs for the
menu and each menu item generated with `unattributized-name'. If a
given pane name is found in `disabled-pane-names', it's rendered in
the navigation as disabled."
  (declare (special *current-navigation-url*))
  (flet ((render-menu-items (&optional orderedp)
           (loop
              for option in options
              for item-number from 1
              do (progn
                   (unless (consp option)
                     (setf option
                           (cons (humanize-name option)
                                 (attributize-name option))))
                   (unless selected-pane
                     (setf selected-pane (car option)))
                   (let* ((label (car option))
                          (target (cdr option))
                          (pane-selected-p (string-equal (attributize-name (car option))
                                                         (attributize-name selected-pane)))
                          (pane-disabled-p (member (attributize-name (car option))
                                                   disabled-pane-names
                                                   :key #'attributize-name
                                                   :test #'string-equal))
                          (pane-class (cond
                                        (pane-selected-p "selected-item")
                                        (pane-disabled-p "disabled-item"))))
                     (with-html
                       (:li :id (unattributized-name (format nil "~A-~A" container-id label)
                                                     'menu-item)
                            :class pane-class
                            (:span :class (concatenate 'string
                                                       "item-wrapper"
                                                       (when orderedp
                                                         (format nil " item-number-~A" item-number)))
                                   (etypecase target
                                     (string
                                      (if (or pane-selected-p pane-disabled-p)
                                        (htm (:span :class "label" (str label)))
                                        (htm (:a :href (make-webapp-uri
                                                         (string-left-trim
                                                           "/" (concatenate 'string
                                                                            (string-right-trim "/" *current-navigation-url*)
                                                                            "/"
                                                                            (string-left-trim "/" target))))
                                                   (str label)))))
                                     (function
                                      (render-link target label)))))))))))
    (with-html
      (:div :class "view menu"
            :id (unattributized-name container-id 'menu)
            (with-extra-tags
              (when header
                (htm (:h1 (str header))))
              (if (null options)
                  (htm
                   (:div :class "empty-menu" (str empty-message)))
                  (if ordered-list-p
                      (htm (:ol (render-menu-items t)))
                      (htm (:ul (render-menu-items))))))))))

