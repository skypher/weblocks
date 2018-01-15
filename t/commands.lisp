(defpackage #:weblocks.t.commands
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.commands)


(plan 1)

(subtest "Function create-command should return a plist with a JSON-rpc method call"
  (let ((result (weblocks.commands::create-command :update-widget
                                                   :widget "some-string"
                                                   :dom-id 42)))
    (assert-that result
                 (has-plist-entries
                  :|jsonrpc| "2.0"
                  :|method| :|updateWidget|
                  :|params| (has-plist-entries
                             :|widget| "some-string"
                             :|domId| 42)))))

(finalize)
