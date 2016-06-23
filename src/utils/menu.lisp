
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entries."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu-item-wt (&key item-id pane-class span-class content target-is-function pane-selected-or-disabled item-href)
  (with-html-to-string
    (:li :id item-id
     :class pane-class
     (:span :class span-class
      (if target-is-function 
        (cl-who:htm (str content))
        (if pane-selected-or-disabled
          (htm (:span :class "label" (str content)))
          (htm (:a :href item-href (str content)))))))))

(defun render-menu-wt (&key menu-id menu-header menu-content menu-empty-p menu-empty-message menu-ordered-list-p)
  (with-html-to-string
    (:div :class "view menu" ; should probably be 'rendered-menu' but I'm not going to be
     ; the one adapting the CSS to this.
     :id menu-id
     (with-extra-tags
       (when menu-header
         (htm (:h1 (str menu-header))))
       (if menu-empty-p
         (str menu-empty-message)
         (if menu-ordered-list-p 
           (htm (:ol (str menu-content)))
           (htm (:ul (str menu-content)))))))))

(defun render-menu (options &key selected-pane header (container-id (gen-id)) (base "")
                    ordered-list-p (empty-message *menu-empty-message*)
                    disabled-pane-names 
                    (menu-template #'render-menu-wt)
                    (item-template #'render-menu-item-wt))
  "Renders a menu snippet based on given options and selected
option. An option may be a dotted pair of a label and \(URL to link to
or function to call on the item's label and selection state \(a boolean)),
or a name (which will be converted to a label and a URL via
humanize-name and attributize-name, respectively). The selected-pane
will be compared to an option's URL via equalp. If the selected
option isn't specified, the first option is rendered as selected.  If
CONTAINER-ID is provided, it is used as the basis of DOM IDs for the
menu and each menu item generated with `unattributized-name'. If a
given pane name is found in `disabled-pane-names', it's rendered in
the navigation as disabled."
  (let ((items-content 
          (format nil "~{~A~}"
                  (loop
                    for option in options
                    for item-number from 1
                    collect (progn
                              (unless (consp option)
                                (setf option
                                      (cons (humanize-name option)
                                            (attributize-name option))))
                              (let* ((label (car option))
                                     (target (cdr option))
                                     (pane-selected-p (equalp target (or selected-pane "")))
                                     (pane-disabled-p (unless (functionp label)
                                                        (member (attributize-name label)
                                                                disabled-pane-names
                                                                :key #'attributize-name
                                                                :test #'string-equal)))
                                     (pane-class (cond
                                                   (pane-selected-p "selected-item")
                                                   (pane-disabled-p "disabled-item"))))
                                (if (functionp label)
                                  (capture-weblocks-output (funcall label))
                                  (funcall 
                                    item-template
                                    :item-id (unattributized-name (format nil "~A-~A" container-id label)
                                                                  'menu-item)
                                    :pane-class pane-class
                                    :span-class (concatenate 'string
                                                             "item-wrapper"
                                                             (when ordered-list-p
                                                               (format nil " item-number-~A" item-number)))
                                    :target-is-function (functionp target)
                                    :pane-selected-or-disabled (or pane-selected-p pane-disabled-p)
                                    :item-href (when (and (stringp target) (not (or pane-selected-p pane-disabled-p)))
                                                 (concatenate 'string
                                                              (string-right-trim "/" base)
                                                              "/"
                                                              (string-left-trim "/" target)))
                                    :content (etypecase target
                                               (string label)
                                               (function
                                                 (capture-weblocks-output 
                                                   (funcall target label pane-selected-p))))))))))))
    (write-string 
      (funcall menu-template 
               :menu-id (unattributized-name container-id 'menu)
               :menu-header header 
               :menu-empty-p (null options)
               :menu-empty-message empty-message
               :menu-ordered-list-p ordered-list-p
               :menu-content items-content)
      *weblocks-output-stream* )))

