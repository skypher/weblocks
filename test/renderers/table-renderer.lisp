
(in-package :weblocks-test)

;; with-table-header
(deftest-html with-table-header-1
    (with-table-header *joe* (lambda ()
			       (with-html
				 (:p "hi"))))
  (:div :class "renderer table employee"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:p "hi")
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html with-table-header-2
    (with-table-header nil (lambda ()
			     (with-html
			       (:p "hi"))))
  (:div :class "renderer table empty-table"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:p "hi")
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

;; with-table-row
(deftest-html with-table-row-1
    (weblocks::with-table-row *joe* (lambda ()
				      (with-html
					(:td "hi"))))
  (:tr (:td "hi")))

(deftest-html with-table-row-2
    (weblocks::with-table-row *joe* (lambda ()
				      (with-html
					(:td "hi"))) :alternp t)
  (:tr :class "altern"
       (:td "hi")))

;; render-table-header-row
(deftest-html render-table-header-row-1
    (render-table-header-row *joe*)
  (:tr
   (:th :class "name" "Name")
   (:th :class "manager" "Manager")))

(deftest-html render-table-header-row-2
    (render-table-header-row *joe* :inlinep t)
  (htm
   (:th :class "name" "Name")
   (:th :class "manager" "Manager")))

(deftest-html render-table-header-row-3
    (render-table-header-row *joe* :inlinep t :slots '((name . blah)) :mode :strict)
  (:th :class "name" "Blah"))

(deftest-html render-table-header-row-4
    (render-table-header-row *joe* :slots '(education))
  (:tr
   (:th :class "name" "Name")
   (:th :class "university" "University")
   (:th :class "graduation-year" "Graduation Year")
   (:th :class "manager" "Manager")))

(deftest-html render-table-header-row-5
    (render-table-header-row *joe* :slots `((name . ,(lambda () nil))))
  (:tr
   (:th :class "name" "Name")
   (:th :class "manager" "Manager")))

;; render-table-header-cell
(deftest-html render-table-header-cell-1
    (render-table-header-cell *joe* 'name "Joe")
  (:th :class "name" "Name"))

(deftest-html render-table-header-cell-2
    (render-table-header-cell *joe* 'address-ref *home-address*)
  (:th :class "address-ref" "Address"))

(deftest-html render-table-header-cell-3
    (render-table-header-cell *joe* 'address *home-address*)
  (htm
   (:th :class "street" "Street")
   (:th :class "city" "City")))

(deftest-html render-table-header-cell-4
    (render-table-header-cell *joe* 'address *home-address* :slots '(city) :mode :strict)
  (:th :class "city" "City"))

;; render-table-body-row
(deftest-html render-table-body-row-1
    (render-table-body-row *joe*)
  (:tr
   (:td :class "name" (:span :class "value" "Joe"))
   (:td :class "manager" (:span :class "value" "Jim"))))

(deftest-html render-table-body-row-2
    (render-table-body-row *joe* :inlinep t)
  (htm
   (:td :class "name" (:span :class "value" "Joe"))
   (:td :class "manager" (:span :class "value" "Jim"))))

(deftest-html render-table-body-row-3
    (render-table-body-row *joe* :inlinep t :slots '((name . blah)) :mode :strict)
  (:td :class "name" (:span :class "value" "Joe")))

(deftest-html render-table-body-row-4
    (render-table-body-row *joe* :slots '(education))
  (:tr
   (:td :class "name" (:span :class "value" "Joe"))
   (:td :class "university" (:span :class "value" "Bene Gesserit University"))
   (:td :class "graduation-year" (:span :class "value" "2000"))
   (:td :class "manager" (:span :class "value" "Jim"))))

;; render-table-body-cell
(deftest-html render-table-body-cell-1
    (render-table-body-cell *joe* 'name "Joe")
  (:td :class "name" (:span :class "value" "Joe")))

(deftest-html render-table-body-cell-2
    (render-table-body-cell *joe* 'address-ref *home-address*)
  (:td :class "address-ref" (:span :class "value" "Address")))

(deftest-html render-table-body-cell-3
    (render-table-body-cell *joe* 'address *home-address*)
  (htm
   (:td :class "street" (:span :class "value" "100 Broadway"))
   (:td :class "city" (:span :class "value" "New York"))))

(deftest-html render-table-body-cell-4
    (render-table-body-cell *joe* 'address *home-address* :slots '(city) :mode :strict)
  (:td :class "city" (:span :class "value" "New York")))

;; render-table
(deftest-html render-table-1
    (render-table (list *joe* *joe*)
		  :pretable-fn (lambda (&rest args)
				 (with-html (:p "hello")))
		  :posttable-fn (lambda (&rest args)
				  (with-html (:p "world"))))
  #.(table-header-template
     '((:th :class "name" "Name")
       (:th :class "manager" "Manager"))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim"))))
     :pretable '((:p "hello"))
     :posttable '((:p "world"))))

;; render-empty-table
(deftest-html render-empty-table-1
    (render-empty-table)
  (:div :class "renderer table empty-table"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:p (:span :class "message" "NIL"))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-empty-table-2
    (render-empty-table :on-empty-string "no data" :caption "caption")
  (:div :class "renderer table empty-table"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:p (:span :class "caption" "caption:&nbsp;")
	    (:span :class "message" "no data"))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))
