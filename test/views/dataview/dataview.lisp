
(in-package :weblocks-test)

;;; Test view-caption
(deftest dataview-view-caption-1
    (view-caption (make-instance 'data-view
				 :caption "foo"))
  "foo")

;;; Test data view with-view-header
(deftest-html data-view-with-view-header-1
    (with-view-header (make-instance 'data-view)
      *joe* nil
      (lambda (&rest args)
	(declare (ignore args))))
  #.(data-header-template nil '(nil)
			  :postslots nil))

;;; Test data view render-view-field
(deftest-html data-view-render-view-field-1
    (render-view-field (make-instance 'data-view-field
				      :slot-name 'first-name)
		       (make-instance 'data-view)
		       nil (make-instance 'text-presentation)
		       "Joe" *joe*)
  (:li :class "first-name"
       (:span :class "label text"
	      "First Name:&nbsp;")
       (:span :class "value"
	      "Joe")))

;;; Test data view render-view-field-value
(deftest-html data-view-render-view-field-value-1
    (render-view-field-value "Joe" (make-instance 'text-presentation)
			     (make-instance 'data-view-field)
			     (make-instance 'data-view)
			     nil *joe*)
  (:span :class "value" "Joe"))

(deftest-html data-view-render-view-field-value-2
    (render-view-field-value nil (make-instance 'text-presentation)
			     (make-instance 'data-view-field)
			     (make-instance 'data-view)
			     nil *joe*)
  (:span :class "value missing" "Not Specified"))

(deftest-html data-view-render-view-field-value-3
    (render-view-field-value "test" (make-instance 'text-presentation)
			     (make-instance 'data-view-field)
			     (make-instance 'data-view)
			     nil *joe* :highlight ".s")
  (:span :class "value" "t<strong>es</strong>t"))

(deftest-html data-view-render-view-field-value-4
    (render-view-field-value "te<st" (make-instance 'text-presentation)
			     (make-instance 'data-view-field)
			     (make-instance 'data-view)
			     nil *joe* :highlight "e<s")
  (:span :class "value" "t<strong>e&lt;s</strong>t"))

(deftest-html data-view-render-view-field-value-5
    (render-view-field-value "<script>alert('XSS Attack')</script>"
			     (make-instance 'text-presentation)
			     (make-instance 'data-view-field)
			     (make-instance 'data-view)
			     nil *joe* :highlight "e<s")
  (:span :class "value"
	 "&lt;script&gt;alert(&#039;XSS Attack&#039;)&lt;/script&gt;"))

;;; Test highlight-regex-matches
(deftest highlight-regex-matches-1
    (weblocks::highlight-regex-matches "Hello World!" "el")
  "H<strong>el</strong>lo World!")

(deftest highlight-regex-matches-2
    (weblocks::highlight-regex-matches "" "el")
  "")

(deftest highlight-regex-matches-3
    (weblocks::highlight-regex-matches "te<st" "el")
  "te&lt;st")

(deftest highlight-regex-matches-4
    (weblocks::highlight-regex-matches "te<st" "")
  "te&lt;st")

(deftest highlight-regex-matches-5
    (weblocks::highlight-regex-matches "ello" "el")
  "<strong>el</strong>lo")

(deftest highlight-regex-matches-6
    (weblocks::highlight-regex-matches "ello" "lo")
  "el<strong>lo</strong>")

(deftest highlight-regex-matches-7
    (weblocks::highlight-regex-matches "te<st" "e<s")
  "t<strong>e&lt;s</strong>t")

;;; Test data view print-view-field-value
(deftest data-view-print-view-field-value-1
    (print-view-field-value "test" nil nil nil nil nil)
  "test")

(deftest data-view-print-view-field-value-2
    (print-view-field-value 'test nil nil nil nil nil)
  "Test")

(deftest data-view-print-view-field-value-3
    (print-view-field-value *joe* nil nil nil nil nil)
  "Employee")

;;; Test data view render-object-view
(deftest-html data-view-render-object-view-1
    (with-request :get nil (render-object-view *joe* '(data employee)))
  #.(data-header-template
     nil
     '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label text" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :postslots nil))

;;; Test render-view (we'll test on data view)
(deftest-html render-view-1
    (let (weblocks::*page-dependencies*)
      (declare (special weblocks::*page-dependencies*))
      (render-view (defview nil ()
		     hello world)
		   :class-name 'render-view-test-class))
  #.(data-header-template
     nil
     '((:li :class "hello" (:span :class "label text" "Hello:&nbsp;") (:span :class "value missing"
								       "Not Specified"))
       (:li :class "world" (:span :class "label text" "World:&nbsp;") (:span :class "value missing"
								       "Not Specified")))
     :postslots nil
     :data-class-name "render-view-test-class"))

