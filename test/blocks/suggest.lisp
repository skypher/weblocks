
(in-package :weblocks-test)

;;; test render-suggest
(deftest-html render-suggest-1
    (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1)
  (htm
   (:select :id "I1" :name "some-name"
	    (:option "a")
	    (:option "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest('I1', 'some-name', 'C1');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-2
    (with-request :get nil
      (render-suggest 'some-name nil :fetch-fn (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1))
  (htm
   (:input :type "text" :id "I1" :name "some-name" :class "suggest")
   (:div :id "C1" :class "suggest" "")
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "declareSuggest('I1', 'C1', 'abc123', 'weblocks-session=1%3Atest');")
	      (fmt "~%// ]]>~%"))))

(deftest render-suggest-3
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(render-suggest 'some-name nil :fetch-fn (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1))
      (do-request `(("pure" . "true") (,weblocks::*action-string* . "abc123"))))
  "<ul><li>a</li><li>b</li><li>c</li></ul>")

(deftest-html render-suggest-4
    (render-suggest 'some-name '("a" "b" "c") :input-id 'i1 :choices-id 'c1 :value "b")
  (htm
   (:select :id "I1" :name "some-name"
	    (:option "a")
	    (:option :selected "true" "b")
	    (:option "c"))
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "replaceDropdownWithSuggest('I1', 'some-name', 'C1', 'b');")
	      (fmt "~%// ]]>~%"))))

(deftest-html render-suggest-5
    (with-request :get nil
      (render-suggest 'some-name nil :fetch-fn (lambda (a) '("a" "b" "c")) :input-id 'i1 :choices-id 'c1
		      :value "test"))
  (htm
   (:input :type "text" :id "I1" :name "some-name" :class "suggest" :value "test")
   (:div :id "C1" :class "suggest" "")
   (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt "declareSuggest('I1', 'C1', 'abc123', 'weblocks-session=1%3Atest');")
	      (fmt "~%// ]]>~%"))))

;;; test format-suggest-list
(deftest format-suggest-list-1
    (weblocks::format-suggest-list '("a" "b" "c"))
  "<ul><li>a</li><li>b</li><li>c</li></ul>")
