
(in-package :weblocks-test)

(deftestsuite snippets/suggest-suite (weblocks-suite print-upcase-suite)
  ())

;;; test render-suggest
(deftest-html render-suggest-1
    (with-request :get nil
      (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1))
  (htm
   (:select :id "I1" :name "some-name"
	    (:option "a")
	    (:option "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest(false, 'I1', 'some-name', 'C1');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-2
    (with-request :get nil
      (render-suggest 'some-name (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1))
  (htm
   (:input :type "text" :id "I1" :name "some-name" :class "suggest" :maxlength "40")
   (:div :id "C1" :class "suggest" :style "display: none" "")
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "declareSuggest('I1', 'C1', 'abc123', 'weblocks-session=1%3ATEST');")
	      (fmt "~%// ]]>~%"))))

(deftest render-suggest-3
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(render-suggest 'some-name (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1))
      (do-request `(("pure" . "true") (,weblocks::*action-string* . "abc123"))))
  "<ul><li>a</li><li>b</li><li>c</li></ul>")

(deftest-html render-suggest-4
    (with-request :get nil
      (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1 :default-value "b"))
  (htm
   (:select :id "I1" :name "some-name"
	    (:option "a")
	    (:option :selected "selected" "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest(false, 'I1', 'some-name', 'C1', 'b');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-5
    (with-request :get nil
      (render-suggest 'some-name (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1
		      :default-value "test"))
  (htm
   (:input :type "text" :id "I1" :name "some-name" :class "suggest" :value "test" :maxlength "40")
   (:div :id "C1" :class "suggest" :style "display: none" "")
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "declareSuggest('I1', 'C1', 'abc123', 'weblocks-session=1%3ATEST');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-6
    (with-request :get nil
      (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1
		      :default-value '("test" "b")))
  (htm
   (:select :id "I1" :name "some-name"
	    (:option "a")
	    (:option :selected "selected" "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest(false, 'I1', 'some-name', 'C1', 'test');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-7
    (with-request :get nil
      (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1
		      :welcome-name (cons "foo" "bar")))
  (htm
   (:select :id "I1" :name "some-name"
	    (:option :value "bar" "[Select foo]")
	    (:option "a")
	    (:option "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest(true, 'I1', 'some-name', 'C1');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-8
    (with-request :get nil
      (make-request-ajax)
      (render-suggest 'some-name '("a" "b" "c")  :input-id 'i1 :choices-id 'c1))
  (htm
   (:input :type "text" :id "I1" :name "some-name" :class "suggest" :maxlength "40")
   (:div :id "C1" :class "suggest" :style "display: none" "")
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "declareSuggest('I1', 'C1', [\"a\",\"b\",\"c\"], 'weblocks-session=1%3ATEST');")
	      (fmt "~%// ]]>~%"))))

;;; test format-suggest-list
(deftest format-suggest-list-1
    (weblocks::format-suggest-list '("a" "b" "c"))
  "<ul><li>a</li><li>b</li><li>c</li></ul>")

