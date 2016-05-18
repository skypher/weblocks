
(in-package :weblocks-test)

;;; test datagrid-render-mining-bar
(deftest-html dataseq-render-mining-bar-datagrid-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (dataseq-render-mining-bar
       (make-instance 'datagrid
                      :data-class 'employee
                      :show-total-items-count-p nil
                      :allow-searching-p nil
                      :allow-select-p nil)))
  (:div :class "data-mining-bar" ""))

(deftest-html datagrid-render-mining-bar-datagrid-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (dataseq-render-mining-bar
       (make-instance 'datagrid
                      :data-class 'employee
                      :show-total-items-count-p t
                      :allow-searching-p nil
                      :allow-select-p nil)))
  (:div :class "data-mining-bar"
        (:span :class "total-items" "(Total of 2 Employees)")))

;;; test render-widget-body for datagrid
(deftest-html render-widget-body-datagrid-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :data-class 'employee
                                 :show-total-items-count-p nil
                                 :allow-pagination-p nil))
            (*on-ajax-complete-scripts* nil))
        (declare (special *on-ajax-complete-scripts*))
        ;; render datagrid
        (render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
          :action "/foo/bar"
          :method "get"
          :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
          (:div :class "extra-top-1" "<!-- empty -->")
          (:div :class "extra-top-2" "<!-- empty -->")
          (:div :class "extra-top-3" "<!-- empty -->")
          (:fieldset
           (:div :class "datagrid-body"
                 #.(table-header-template
                    '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc124" "Name")))
                      (:th :class "manager" (:span :class "label" #.(link-action-template "abc125" "Manager"))))
                    '((:tr
                       (:td :class "name" (:span :class "value" "Bob"))
                       (:td :class "manager" (:span :class "value" "Jim")))
                      (:tr :class "altern"
                       (:td :class "name" (:span :class "value" "Joe"))
                       (:td :class "manager" (:span :class "value" "Jim"))))
                    :summary "Ordered by name, ascending."))
           (:input :name "action" :type "hidden" :value "abc123"))
          (:div :class "extra-bottom-1" "<!-- empty -->")
          (:div :class "extra-bottom-2" "<!-- empty -->")
          (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-datagrid-2
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'datagrid
                                 :data-class 'employee
                                 :allow-select-p nil
                                 :allow-searching-p nil
                                 :allow-pagination-p nil
                                 :show-total-items-count-p nil))
            (*on-ajax-complete-scripts* nil))
        (declare (special *on-ajax-complete-scripts*))
        ;; render datagrid
        (render-widget-body grid)))
  (htm
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
          :action "/foo/bar"
          :method "get"
          :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
          (:div :class "extra-top-1" "<!-- empty -->")
          (:div :class "extra-top-2" "<!-- empty -->")
          (:div :class "extra-top-3" "<!-- empty -->")
          (:fieldset
           (:div :class "datagrid-body"
                 #.(table-header-template
                    '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc124" "Name")))
                      (:th :class "manager" (:span :class "label" #.(link-action-template "abc125" "Manager"))))
                    '((:tr
                       (:td :class "name" (:span :class "value" "Joe"))
                       (:td :class "manager" (:span :class "value" "Jim"))))
                    :summary "Ordered by name, ascending."))
           (:input :name "action" :type "hidden" :value "abc123"))
          (:div :class "extra-bottom-1" "<!-- empty -->")
          (:div :class "extra-bottom-2" "<!-- empty -->")
          (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-datagrid-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :sort (cons 'name :asc)
                                 :data-class 'employee
                                 :show-total-items-count-p nil))
            (*on-ajax-complete-scripts* nil))
        (declare (special *on-ajax-complete-scripts*))
        ;; set items per page...
        (setf (pagination-items-per-page (dataseq-pagination-widget grid)) 1)
        ;; render datagrid
        (render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
        ;; add another item
        (persist-object *default-store* *employee4*)
        ;; go to next page
        (do-request `((,weblocks::*action-string* . "abc126")))
        (render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ; Page 1
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
          :action "/foo/bar"
          :method "get"
          :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
          (:div :class "extra-top-1" "<!-- empty -->")
          (:div :class "extra-top-2" "<!-- empty -->")
          (:div :class "extra-top-3" "<!-- empty -->")
          (:fieldset
           (:div :class "datagrid-body"
                 #.(table-header-template
                    '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc124" "Name")))
                      (:th :class "manager" (:span :class "label" #.(link-action-template "abc125" "Manager"))))
                    '((:tr
                       (:td :class "name" (:span :class "value" "Bob"))
                       (:td :class "manager" (:span :class "value" "Jim"))))
                    :summary "Ordered by name, ascending."))
           (:input :name "action" :type "hidden" :value "abc123"))
          (:div :class "extra-bottom-1" "<!-- empty -->")
          (:div :class "extra-bottom-2" "<!-- empty -->")
          (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "id-123"
         #.(pagination-page-info-template 1 2) "&nbsp;"
         #.(link-action-template "abc126" "Next >" :class "next-page")
         #.(pagination-goto-form-template "abc127" :page-one-p t))
   ; Page 2
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
          :action "/foo/bar"
          :method "get"
          :onsubmit "initiateFormAction(\"abc128\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
          (:div :class "extra-top-1" "<!-- empty -->")
          (:div :class "extra-top-2" "<!-- empty -->")
          (:div :class "extra-top-3" "<!-- empty -->")
          (:fieldset
           (:div :class "datagrid-body"
                 #.(table-header-template
                    '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc129" "Name")))
                      (:th :class "manager" (:span :class "label" #.(link-action-template "abc130" "Manager"))))
                    '((:tr
                       (:td :class "name" (:span :class "value" "Guy"))
                       (:td :class "manager" (:span :class "value" "Jim"))))
                    :summary "Ordered by name, ascending."))
           (:input :name "action" :type "hidden" :value "abc128"))
          (:div :class "extra-bottom-1" "<!-- empty -->")
          (:div :class "extra-bottom-2" "<!-- empty -->")
          (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "id-123"
         #.(link-action-template "abc131" "< Previous" :class "previous-page")
         "&nbsp;" #.(pagination-page-info-template 2 3) "&nbsp;"
         #.(link-action-template "abc132" "Next >" :class "next-page")
         #.(pagination-goto-form-template "abc133"))))

;;; test render-datagrid-table-body
(deftest-html render-datagrid-table-body-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :data-class 'employee))
            (*on-ajax-complete-scripts* nil))
        (declare (special *on-ajax-complete-scripts*))
        ;; render datagrid
        (dataseq-update-sort-column grid)
        (render-dataseq-body grid)
        ;; sort by name (should be descending)
        (do-request `((,weblocks::*action-string* . "abc123")))
        (dataseq-update-sort-column grid)
        (render-dataseq-body grid)
        ;; refresh the action (should still be descending)
        (do-request `((,weblocks::*action-string* . "abc123")))
        (dataseq-update-sort-column grid)
        (render-dataseq-body grid)))
  (htm
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc123" "Name")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc124" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, ascending."))
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-desc" (:span :class "label" #.(link-action-template "abc125" "Name")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc126" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, descending."))
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-desc" (:span :class "label" #.(link-action-template "abc127" "Name")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc128" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, descending."))))

(deftest-html render-datagrid-table-body-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :view (defview () (:type table :inherit-from '(:scaffold employee))
                                         (name :allow-sorting-p nil)
                                         (manager :allow-sorting-p nil))
                                 :data-class 'employee)))
        (render-dataseq-body grid)))
     (:div :class "datagrid-body"
           #.(table-header-template
              '((:th :class "name" (:span :class "label" "Name"))
                (:th :class "manager" (:span :class "label" "Manager")))
              '((:tr
                 (:td :class "name" (:span :class "value" "Joe"))
                 (:td :class "manager" (:span :class "value" "Jim")))
                (:tr :class "altern"
                 (:td :class "name" (:span :class "value" "Bob"))
                 (:td :class "manager" (:span :class "value" "Jim")))))))

(deftest-html render-datagrid-table-body-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :data-class 'employee)))
        (dataseq-update-sort-column grid)
        (render-dataseq-body grid)
        (setf (first-name *bob*) "Zed")
        (dataseq-update-sort-column grid)
        (render-dataseq-body grid)
        (setf (first-name *bob*) "Bob")))
  (htm
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc123" "Name")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc124" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, ascending."))
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc125" "Name")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc126" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Zed"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, ascending."))))

(deftest-html render-datagrid-table-body-4
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
                                 :data-class 'employee
                                 :sort '(name . :asc)
                                 :view (defview () (:type table)
                                         name address manager))))
        ;; render datagrid
        (render-dataseq-body grid)
        ;; sort by name (should be descending)
        (do-request `((,weblocks::*action-string* . "abc123")))
        (render-dataseq-body grid)))
  (htm
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-asc" (:span :class "label" #.(link-action-template "abc123" "Name")))
              (:th :class "address" (:span :class "label" #.(link-action-template "abc124" "Address")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc125" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "address" (:span :class "value" "Address"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "address" (:span :class "value" "Address"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, ascending."))
   (:div :class "datagrid-body"
         #.(table-header-template
            '((:th :class "name sort-desc" (:span :class "label" #.(link-action-template "abc126" "Name")))
              (:th :class "address" (:span :class "label" #.(link-action-template "abc127" "Address")))
              (:th :class "manager" (:span :class "label" #.(link-action-template "abc128" "Manager"))))
            '((:tr
               (:td :class "name" (:span :class "value" "Joe"))
               (:td :class "address" (:span :class "value" "Address"))
               (:td :class "manager" (:span :class "value" "Jim")))
              (:tr :class "altern"
               (:td :class "name" (:span :class "value" "Bob"))
               (:td :class "address" (:span :class "value" "Address"))
               (:td :class "manager" (:span :class "value" "Jim"))))
            :summary "Ordered by name, descending."))))

;;; test specialization of dependencies
(deftest datagrid-dependencies-1
    (with-request :get nil
      (not (null
            (member (make-versioned-regex "pagination" "css")
                    (dependencies (make-instance 'datagrid :data-class 'employee))
                    :key (lambda (e)
                           (format nil "~A" (dependency-url e)))
                    :test #'cl-ppcre:scan))))
  t)

(addtest datagrid-i18n-1
  (ensure-alist-has-keys 
    (widget-translation-table 
      (make-instance 'datagrid :data-class 'employee))
    (list 
      :select-label       
      :select-all-label  
      :select-none-label)))
