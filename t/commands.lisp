(defpackage #:weblocks-test/commands
  (:use #:cl
        #:hamcrest/rove
        #:rove)
  (:import-from #:weblocks/commands
                #:create-command))
(in-package weblocks-test/commands)


(deftest test-create-command
  (testing "Function create-command should return a plist with a JSON-rpc method call"
    (let ((result (create-command :update-widget
                                  :widget "some-string"
                                  :dom-id 42)))
      (testing "Now we'll check the structure of the response"
        (assert-that result
                     (has-plist-entries
                      :|jsonrpc| "2.0"
                      :|method| :|updateWidget|
                      :|params| (has-plist-entries
                                 :|widget| "some-string"
                                 :|domId| 42)))))))
