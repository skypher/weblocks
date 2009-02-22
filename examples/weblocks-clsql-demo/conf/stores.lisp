
(in-package :weblocks-clsql-demo)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *sql-store* :clsql '("localhost" "test" "username" "password")
	  :database-type :mysql)

