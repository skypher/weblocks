
(in-package :mw-diff-sexp)

(defmethod render-difference :around ((record insertion-record))
  (list (cons :added (change record))))

(defmethod render-difference :around ((record deletion-record))
  (list (cons :removed (change record))))

(defmethod render-difference :around ((record update-record))
  (list (cons :removed (update-record-old record))
        (cons :added (update-record-new record))))


(in-package :your-package)

(defun diff (list1 list2 &key &allow-other-keys)
  (let ((result (mw-diff-sexp:diff-sexp list1 list2)))
    (if (and (consp result) (eql (length result) 1))
      (car result)
      result)))

(defun remove-lhtml-header (lhtml)
  "Removes the prologue from a LHTML list:

  (:HTML NIL (:HEAD NIL) (:BODY NIL (:P \"foo\")))

  becomes

  (:DIV NIL (:P \"foo\"))"
  (cddr (cadddr lhtml)))

(defun html->lhtml (html &key (remove-prologue t))
  (let ((lhtml (chtml:parse html (chtml:make-lhtml-builder))))
    (if remove-prologue
      (remove-lhtml-header lhtml)
      lhtml)))

(defun lhtml->html (lhtml)
  (chtml:serialize-lhtml lhtml (chtml:make-string-sink))) ; FIXME lowercase

(defstub lhtml->xhtml (lhtml))

(defun map-subtrees (fn tree)
  "MAPCAR on a tree structure; FN gets called with each subtree."
  (loop for node in tree
        if (atom node)
          collect node
        else
          collect (map-subtrees fn (funcall fn node))))

;(defvar *html1* (chtml:parse "<p>Absatz1</p><ol><li>Liste1</li></ol><p>Absatz2</p>" (chtml:make-lhtml-builder)))
;(defvar *html2* (chtml:parse "<p>Absatz1a</p><p>Absatz2</p><ul><li>Liste1</li></ul>" (chtml:make-lhtml-builder)))
(defun diff-html (html1 html2 &key (keep-unchanged t))
  (flet ((diff-marks->lhtml (lhtml-with-marks)
           (map-subtrees (lambda (x)
                       ;(format t "x: ~S~%" x)
                       (cond
                         ((and (consp x) (eq (car x) :added))
                          (append '(:div ((:class "insertion"))) (list (cdr x))))
                         ((and (consp x) (eq (car x) :removed))
                          (append '(:div ((:class "deletion"))) (list (cdr x))))
                         (t x)))
                     lhtml-with-marks)))
    (let* ((lhtml1 (html->lhtml html1)) ; we prepend a div container to get diffs
           (lhtml2 (html->lhtml html2)) ; of the top-level elements. XXX probably no longer necessary...
           (diff-lhtml (diff lhtml1 lhtml2))
           (diff-lhtml-formatted (diff-marks->lhtml diff-lhtml))
           (diff-html (lhtml->html (append (list :div nil) diff-lhtml-formatted))))
      #+(or)
      (format t "~S~%~S~%~S~%~S~%~S~%" lhtml1 lhtml2 diff-lhtml diff-lhtml-formatted diff-html)
      diff-html)))

(defun diff-changes-html (html1 html2)
  (format nil "~A~A" "<p>Änderungen:</p>" (diff-html html1 html2 :keep-unchanged nil)))

;; quick test
(format t "~S~%" (diff-html "<p>Absatz1</p>" "<p>Absatz2</p>"))

