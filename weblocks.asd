;; Временно сохранил зависимости
;; (ql:quickload '(
;;                 :log4cl
;;    :clack
;;    :lack-request
;;    :routes
;;    :function-cache
;;    :local-time
;;    :local-time-duration
;;    :dexador ;; to retrive remote dependencies and put them into the cache
;;    :closer-mop
;;    :puri
;;    :cl-json
;;    :alexandria
;;    :serapeum ;; utilities, like alexandria
;;    :spinneret
;;    :parenscript
;;    :cl-fad
;;    :optima
;;    :cl-cont
;;    :metatilities
;;    :cl-ppcre
;;    :anaphora
;;    :f-underscore
;;    :bordeaux-threads
;;    :salza2
;;    :html-template
;;    :trivial-timeout
;;    :trivial-backtrace 
;;    :parse-number 
;;    :pretty-function 
;;    :babel
;;    :metacopy
;;    :split-sequence
;;    :cl-strings
;;                 ))

(defsystem weblocks
  :name "weblocks"
  :class :package-inferred-system
  :version (:read-file-form "version.lisp-expr")
  :maintainer "Alexander Artemenko, Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :pathname "src"
  :depends-on ("weblocks/app"
               ;; "weblocks/app-dependencies"
               "weblocks/js/jquery"
               ;; "weblocks/session"
               ;; "weblocks/hooks"
               ;; "weblocks/request"
               
               ;; :log4cl
               ;; :clack
               ;; :lack-request
               ;; :routes
               ;; :function-cache
               ;; :local-time
               ;; :local-time-duration
               ;; :dexador ;; to retrive remote dependencies and put them into the cache
               ;; :closer-mop
               ;; :puri
               ;; :cl-json
               ;; :alexandria
               ;; :serapeum ;; utilities, like alexandria
               ;; :spinneret
               ;; :parenscript
               ;; :cl-fad
               ;; :optima
               ;; :cl-cont
               ;; :metatilities
               ;; :cl-ppcre
               ;; :anaphora
               ;; :f-underscore
               ;; :bordeaux-threads
               ;; :salza2
               ;; :html-template
               ;; :trivial-timeout
               ;; :trivial-backtrace 
               ;; :parse-number 
               ;; :pretty-function 
               ;; :babel
               ;; :metacopy
               ;; :split-sequence
               ;; :cl-strings
               ;;   "application"
   )
;;  :serial t
;;  :components ;; ((:module src
              ;;   :components (
              ;;                (:file "package")
              ;;                (:module utils
              ;;                 :components ((:file "clos")
              ;;                              (:file "misc")
              ;;                              (:file "runtime-class")
              ;;                              (:file "string")
              ;;                              (:file "list")
              ;;                              (:file "uri")
              ;;                              (:file "timing")
              ;;                              )
              ;;                         )
              ;;                (:file "variables")
              ;;                (:file "html")
              ;;                (:file "weblocks")
              ;;                (:file "versioning")
              ;;                (:file "bundling")
              ;;                (:file "response"
              ;;                 :depends-on ("request2"
              ;;                              "commands"))
              ;;                (:file "dependencies")
              ;;                (:file "dependencies2"
              ;;                 :depends-on ("response"
              ;;                              "routes"))
              ;;                (:module "js-backend"
              ;;                 :components ((:file "base")
              ;;                              (:file "jquery")))
              ;;                (:file "application-mop")
              ;;                (:file "application")
              ;;                (:file "commands"
              ;;                 :depends-on ("hooks"))
              ;;                (:file "actions")
              ;;                (:file "actions2"
              ;;                 :depends-on ("response"
              ;;                              "commands"))
              ;;                ;; TODO: remove
              ;;                (:file "routes")
              ;;                (:file "dom-object")
              ;;                (:module widgets
              ;;                 :depends-on ("dependencies2"
              ;;                              "commands")
              ;;                 :components ((:module widget
              ;;                               :components ((:file "widget-mop")
              ;;                                            (:file "uri-parameters-mixin")
              ;;                                            ;; (:file "widget")
              ;;                                            (:file "widget2")
              ;;                                            (:file "string-widget")
              ;;                                            (:file "funcall-widget")))))
              ;;                (:file "session")
              ;;                (:file "root-widget"
              ;;                 :depends-on ("session"))
              ;;                (:file "default-init"
              ;;                 :depends-on ("session"
              ;;                              "widgets"
              ;;                              "html"))
              ;;                (:file "request2"
              ;;                 :depends-on ("session"))
              ;;                (:file "hooks")
              ;;                (:file "debug")
              ;;                (:file "page")
              ;;                (:file "log-actions")
              ;;                (:file "debug-mode")
              ;;                (:file "uri-tokens")
              ;;                (:file "session-lock")
              ;;                (:file "error-handler2")
              ;;                (:file "request-handler2")
              ;;                (:module linguistic
              ;;                 :components ((:file "grammar")))
              ;;                (:module control-flow
              ;;                 :components ((:file "call-answer")
              ;;                              (:file "dialog")
              ;;                              (:file "workflow")))
              ;;                (:file "server")
              ;;                (:file "server2")
              ;;                (:file "current-app"
              ;;                 :depends-on ("application"))
              ;;                ;; (:file "default-application")
              ;;                )))
  :in-order-to ((test-op (test-op "weblocks-test"))))


(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "log4cl" '(#:log))

(register-system-packages "weblocks/widgets/base" '(#:weblocks/widget))
(register-system-packages "weblocks/js/base" '(#:weblocks/js))
