
(in-package :weblocks-test)

;;; test defwebapp
(deftest defwebapp-1
    (let (weblocks::*webapp-name*)
      (declare (special weblocks::*webapp-name*))
      (defwebapp 'hello)
      weblocks::*webapp-name*)
  hello)
