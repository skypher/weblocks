(in-package :blog)

(defview user-table-view (:type table :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))

(defview post-table-view (:type table :inherit-from '(:scaffold post)))
(defview post-data-view (:type data :inherit-from '(:scaffold post)))
(defview post-form-view (:type form :inherit-from '(:scaffold post)))
