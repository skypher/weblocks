
(in-package :weblocks)

(export '(begin-section
          end-section
          begin-collapsible-section
          end-collapsible-section))

(defun begin-section (title &key (container-class "") (container-id (gen-id)) (heading t)
                            heading-onclick heading-class)
  "Begin a section (a DIV container) on the client.
The section is preceded by a heading 3 unless HEADING is NIL."
  (when heading
    (write
      (with-html-to-string
        (:h3 :class heading-class :onclick heading-onclick (esc title)))
      :stream *weblocks-output-stream*
      :escape nil))
  (format *weblocks-output-stream* "<div id='~D' class='~A'>" container-id container-class))

(defun end-section ()
  "End a client section."
  (format *weblocks-output-stream* "</div>"))

(defun begin-collapsible-section (title)
  "Begin a section that is expandable/collapsible via clicks on
its heading."
  (let ((id (gen-id)))
    (begin-section title
                   :container-id id
                   :heading-onclick (format nil "toggleExpandCollapse(this,document.getElementById(\"~D\"));" id)
                   :heading-class "collapsible-heading collapsed"
                   :container-class "collapsible-container collapsed")))

(defun end-collapsible-section ()
  (end-section))

