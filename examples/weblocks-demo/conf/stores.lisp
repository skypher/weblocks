
(in-package :weblocks-demo)

;;; Multiple stores may be defined. The last defined store will be the
;;; default. In the case of weblocks demo static store configuration
;;; isn't used - we dynamically create stores for each session because
;;; we need to sandbox users. We only create static configurations to
;;; load appropriate store code during application startup.

;;; Memory store
(defstore *scratch-store* :memory)

;;; Prevalence store...
(defstore *prevalence-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :weblocks-demo)))

;;; CLSQL store
;; (defstore *sql-store* :clsql '("localhost" "test" "username" "password")
;; 	  :database-type :mysql)

;;; Cascade delete should be turned off for prevalence store
(setf *default-cascade-delete-mixins-p* nil)

