(in-package :blog)

(defview user-grid-view (:type grid :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))

(defview post-grid-view (:type grid :inherit-from '(:scaffold post)))
(defview post-data-view (:type data :inherit-from '(:scaffold post)))

(defview post-form-view (:type form :inherit-from '(:scaffold post))
  (time :hidep t)
  ;; POST-AUTHOR-ID and ALL-USERS will be defined below
  (author :reader #'post-author-id
	  :present-as (dropdown :choices #'all-users
				:label-key #'user-name)
	  :parse-as (object-id :class-name 'user)
	  :requiredp t)
  (short-text :present-as textarea
	      :requiredp t)
  (text :present-as (textarea :cols 30)
	:requiredp t))
