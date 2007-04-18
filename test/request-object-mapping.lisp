
(in-package :weblocks-test)

;;; test request-object-mapping
(deftest request-object-mapping-1
    (with-request :get '(("name" . "Pink") ("age" . 25))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)

(deftest request-object-mapping-2
    (with-request :post '(("name" . "Pink") ("age" . 25))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)
