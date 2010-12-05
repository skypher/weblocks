(defpackage :weblocks-postmodern-test
  (:use :cl :weblocks :postmodern)
  (:shadowing-import-from :postmodern #:text)
  (:shadowing-import-from :weblocks #:commit-transaction))

(in-package :weblocks-postmodern-test)

(defparameter *db* nil)
(defparameter *user* nil)
(defparameter *pass* nil)
(defparameter *host* nil)

(defparameter *postmodern-store* nil)
(defparameter *test-status* (list nil nil nil nil nil))

;; Let's go with an "Office Space" theme...
(defclass employee ()
  ((id :col-type serial
       :initarg :id :reader id)
   (company :col-type string
	    :initarg :company :accessor company)
   (first-name :col-type string
	       :initarg :first-name :accessor first-name)
   (last-name :col-type string
	      :initarg :last-name :accessor last-name)
   (quote :col-type string
	  :initarg :quote :accessor movie-quote)
   (tps-reports :col-type boolean
		:initarg :tps-reports :accessor tps-reports)
   (stapler :col-type boolean
	    :initarg :stapler :accessor stapler))
  (:metaclass dao-class)
  (:keys id))

(deftable employee (!dao-def))

(defparameter *bill*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Bill"
		 :last-name "Lumbergh"
		 :quote "Yeeeaaaaah."
		 :tps-reports nil
		 :stapler nil))

(defparameter *peter*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Peter"
		 :last-name "Gibbons"
		 :quote "The thing is, Bob, it's not that I'm lazy, it's that I just don't care."
		 :tps-reports nil
		 :stapler nil))

(defparameter *milton*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Milton"
		 :last-name "Waddams"
		 :quote "I could set the building on fire."
		 :tps-reports t
		 :stapler t))

(defun test-creation ()
  (format t "***Testing Creation...~%")
  (create-table 'employee)
  (persist-object *postmodern-store* *bill*)
  (persist-object *postmodern-store* *peter*)
  (persist-object *postmodern-store* *milton*)
  (format t "Our story begins at a company named Initech which employs, among others, Peter, Bill and Milton.~%~%")
  (when (= (count-persistent-objects *postmodern-store* 'employee) 3)
    (setf (first *test-status*) t)))

(defun display-employee-count ()
  (format t "Employees count: ~A~%"
	  (count-persistent-objects *postmodern-store* 'employee)))

(defun test-retrieval ()
  (format t "***Testing Retrieval...~%")
  (display-employee-count)
  (let ((emp (find-persistent-object-by-id *postmodern-store* 'employee 1)))
    (format t "Employee 1: ~S whose name is ~A~%" emp
	    (concatenate 'string (first-name emp) " " (last-name emp))))
  (format t "Employees: ~S~%~%"
	  (find-persistent-objects *postmodern-store* 'employee))
  (when (find-persistent-object-by-id *postmodern-store* 'employee 1)
    (setf (second *test-status*) t)))


(defun test-updating ()
  (format t "***Testing Updating...~%")
  (format t "As time goes on, Peter finds himself increasingly dissatisfied with his job...~%")
  (let ((employee (find-persistent-object-by-id *postmodern-store* 'employee 2)))
    (format t "Peter: ~A~%" (movie-quote employee))
    (format t "But this is perceived as management potential by consultants leading Peter to begin disregarding his boss, Bill.~%")
    (setf (movie-quote employee) "Not right now, Lumbergh, I'm kinda busy. In fact, look, I'm gonna have to ask you to just go ahead and come back another time.")
    (persist-object *postmodern-store* employee)
    (format t "Peter: ~A~%" (movie-quote employee)))
  (format t "Peter begins stealing from Initech...~%~%")
  (when (search "Lumbergh" (movie-quote (find-persistent-object-by-id *postmodern-store* 'employee 2)))
    (setf (third *test-status*) t)))

(defun test-deletion ()
  (format t "***Testing Deletion...~%")
  (format t "Throughout this time, Milton has been walked on and treated unfairly.~%")
  (format t "They even fire him by simply not sending paychecks rather than telling him to leave!~%")
  (delete-persistent-object-by-id *postmodern-store* 'employee 3)
  (format t "Bill: Yeah...you're gonna have to talk to payroll...about that.~%")
  (format t "Meanwhile Peter's attempts to steal haven't been going so well...~%")
  (let ((employee (car (find-persistent-objects *postmodern-store* 'employee
						:where '(:= 'first-name "Peter")))))
    (delete-persistent-object *postmodern-store* employee))
  (format t "Peter: I think I might be going away for a little while...to jail.~%")
  (display-employee-count)
  (format t "But then, Milton snaps...~%")
  (clean-store *postmodern-store*)
  (format t "Peter: Yeah I think the fire pretty much took care of everything.~%")
  (display-employee-count)
  (format t "~%~%")
  (when (= (count-persistent-objects *postmodern-store* 'employee) 0)
    (setf (fourth *test-status*) t)))

(defun test-transactions ()
  (format t "***Testing Nested Transactions...~%")
  (format t "But even in retirement Milton can't seem to find any respect...~%")
  (begin-transaction *postmodern-store*)
  (let ((employee *milton*))
    (setf (movie-quote employee) "Milton: Excuse me, Senor, may I speak to you please?")
    (begin-transaction *postmodern-store*)
    (persist-object *postmodern-store* employee)
    (format t "~A~%~%" (movie-quote employee))
    (when (= (cdr (gethash (bordeaux-threads:current-thread)
			   weblocks-postmodern::*transactions*)) 1)
      (setf (fifth *test-status*) t))
    (commit-transaction *postmodern-store*))
  (unless (= (cdr (gethash (bordeaux-threads:current-thread)
			   weblocks-postmodern::*transactions*)) 0)
    (setf (fifth *test-status*) nil))
  (commit-transaction *postmodern-store*))

(defun test-all ()
  (test-creation)
  (test-retrieval)
  (test-updating)
  (test-deletion)
  (test-transactions))

(defun run-all-tests (&key (db *db*) (user *user*) (pass *pass*) (host *host*))
  (let ((*db* db)
	(*user* user)
	(*pass* pass)
	(*host* host))
    (setf *postmodern-store* (open-store :postmodern :db *db* :user *user*
					 :pass *pass* :host *host*))
    (unwind-protect (test-all)
      (if (notevery #'null *test-status*)
	  (format t "All tests passed!~%")
	  (format t "Uh-oh! There were test failures..
~%Creation: ~A~%Retreival: ~A~%Updating: ~A~%Deletion: ~A~%Nested Transactions: ~A~%~%"
		  (first *test-status*) (second *test-status*) (third *test-status*)
		  (fourth *test-status*) (fifth *test-status*)))
      (setf *test-status* (list nil nil nil nil nil))
      (query (:drop-table 'employee))
      (close-store *postmodern-store*))))
