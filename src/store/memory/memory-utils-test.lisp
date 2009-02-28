
;;; As of now these tests are not ran. if we ever develop a system to
;;; easily unit test specifics of stores, we should use these tests

(in-package :weblocks-test)

;;; test scratch stores
(deftest scratch-store-1
    (mapcar #'object-id (objects-from-scratch-store (make-scratch-store *joe* *bob* *some-college*)))
  1 2 1)

;;; test object-satisfies-search-p
(deftest object-satisfies-search-p-1
    (object-satisfies-search-p "hi" nil nil t *joe*)
  nil)

(deftest object-satisfies-search-p-2
    (object-satisfies-search-p "Joe" nil nil t *joe*)
  t)

(deftest object-satisfies-search-p-3
    (object-satisfies-search-p "30" nil nil t *joe*)
  nil)

(deftest object-satisfies-search-p-4
    (object-satisfies-search-p "30" nil nil t *joe* :slots '(age))
  t)

(deftest object-satisfies-search-p-5
    (object-satisfies-search-p "Broadway" nil nil t *joe*)
  nil)

(deftest object-satisfies-search-p-6
    (object-satisfies-search-p "address" nil nil t *joe*)
  nil)

(deftest object-satisfies-search-p-7
    (not (null (object-satisfies-search-p "Joe" *joe* 'name 'string "Joe")))
  t)

(deftest object-satisfies-search-p-8
    (object-satisfies-search-p "Bene" nil nil t *joe* :slots '(education))
  t)

(deftest object-satisfies-search-p-9
    (object-satisfies-search-p "Broadway" nil nil t *joe* :slots '(address-ref))
  nil)

;;; test object-satisfies-search-p for booleans
(deftest object-satisfies-search-p-booleans-1
    (object-satisfies-search-p "Yes" *joe* 'veteran 'boolean t)
  t)

(deftest object-satisfies-search-p-booleans-2
    (object-satisfies-search-p "Yes" *joe* 'veteran 'boolean nil)
  nil)

(deftest object-satisfies-search-p-booleans-3
    (object-satisfies-search-p "No" *joe* 'veteran 'boolean nil)
  t)

(deftest object-satisfies-search-p-booleans-4
    (object-satisfies-search-p "No" nil nil t *joe* :slots '(veteran))
  t)

;;; test filter-objects-in-memory
(deftest filter-objects-in-memory-1
    (with-request :get nil
      (length (weblocks:filter-objects-in-memory (list *joe* *bob*) "Joe")))
  1)

(deftest filter-objects-in-memory-2
    (with-request :get nil
      (length (weblocks:filter-objects-in-memory (list *joe* *bob*) "o")))
  2)

(deftest filter-objects-in-memory-3
    (with-request :get nil
      (length
       (weblocks:filter-objects-in-memory
	(list *joe* *bob*) "o"
	'(:slots (age) :mode :strict))))
  0)

(deftest filter-objects-in-memory-4
    (with-request :get nil
      (length (weblocks:filter-objects-in-memory (list *joe* *bob*) nil)))
  2)

;;; test strictly-less-p
(deftest strictly-less-p-1
    (strictly-less-p 1 2)
  t)

(deftest strictly-less-p-2
    (not (null (strictly-less-p "a" "b")))
  t)

(deftest strictly-less-p-3
    (not (null (strictly-less-p nil nil)))
  nil)

(deftest strictly-less-p-4
    (strictly-less-p nil 'a)
  nil)

(deftest strictly-less-p-5
    (strictly-less-p 'a nil)
  t)

(deftest strictly-less-p-6
    (strictly-less-p 'a "b")
  t)

(deftest strictly-less-p-7
    (strictly-less-p 'b "a")
  nil)

(deftest strictly-less-p-8
    (strictly-less-p t nil)
  t)

(deftest strictly-less-p-9
    (strictly-less-p nil t)
  nil)

;;; test equivalentp
(deftest equivalentp-1
    (equivalentp "a" "a")
  t)

(deftest equivalentp-2
    (equivalentp nil nil)
  t)

;;; test order-objects-in-memory
(deftest order-objects-in-memory-1
    (mapcar #'first-name (order-objects-in-memory (list *joe* *bob*) (cons '(name) :asc)))
  ("Bob" "Joe"))

(deftest order-objects-in-memory-2
    (mapcar #'first-name (order-objects-in-memory (list *joe* *bob*) (cons '(name) :desc)))
  ("Joe" "Bob"))

(deftest order-objects-in-memory-3
    (mapcar #'first-name (order-objects-in-memory (list *joe* *bob*) (cons '(age) :desc)))
  ("Bob" "Joe"))

(deftest order-objects-in-memory-4
    (mapcar #'first-name (order-objects-in-memory (list *joe* *bob*) nil))
  ("Joe" "Bob"))

;;; test range-objects-in-memory
(deftest range-objects-in-memory-1
    (range-objects-in-memory (list 1 2 3 4 5)
			     nil)
  (1 2 3 4 5))

(deftest range-objects-in-memory-2
    (range-objects-in-memory (list 1 2 3 4 5)
			     (cons 1 3))
  (2 3))
