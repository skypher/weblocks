
(in-package :weblocks-test)

(deftestsuite widgets/navigation-suite (weblocks-suite) nil)

(defun make-test-nav ()
  (let ((widget (make-instance 'widget)))
    (make-navigation "Test navigation" (list "Default Item" widget "")
                                       (list "Item One" widget "one")
                                       (list "Item Two" widget "two"))))

(addtest navigation-pane-name-for-token
  (let ((nav (make-test-nav)))
      (ensure-same (weblocks::navigation-pane-name-for-token nav "one") "Item One")
      (ensure-same (weblocks::navigation-pane-name-for-token nav "ONE") "Item One")
      (ensure-same (weblocks::navigation-pane-name-for-token nav nil) "Default Item")))

(addtest navigation-menu-items
  (let ((nav (make-test-nav))
        (*lift-equality-test* (curry-after #'tree-equal :test #'equal)))
    (ensure-same (navigation-menu-items nav)
                 '(("Default Item" . "")
                   ("Item One" . "one")
                   ("Item Two" . "two")))))

(addtest navigation-hidden-panes-1
  (let ((nav (make-test-nav))
        (*lift-equality-test* (curry-after #'tree-equal :test #'equal)))
    (setf (navigation-hidden-panes nav) (list "one"))
    (ensure-same (navigation-menu-items nav)
                 '(("Default Item" . "")
                   ("Item Two" . "two")))))

(addtest navigation-hidden-panes-2
  (let ((nav (make-test-nav))
        (*lift-equality-test* (curry-after #'tree-equal :test #'equal)))
    (setf (navigation-hidden-panes nav) (list nil))
    (ensure-same (navigation-menu-items nav)
                 '(("Item One" . "one")
                   ("Item Two" . "two")))))
