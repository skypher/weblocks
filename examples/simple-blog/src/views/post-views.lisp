(in-package :simple-blog)

(defview user-table-view (:type table :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))

(defview post-table-view (:type table :inherit-from '(:scaffold post)))

(defview post-data-view (:type data :inherit-from '(:scaffold post))
  (author :reader #'post-author-name)
  (time :reader #'post-formatted-time))

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

(defview post-short-view (:type data :inherit-from 'post-data-view)
  (title :present-as (action-link
		      :action-fn (lambda (w)
				   (setf (mode w) :full)
				   (let ((blog (blog-widget w)))
				     (setf (mode blog) :post))
				   (safe-funcall (on-select w) w))))
  (text :hidep t))

(defview post-full-view (:type data :inherit-from 'post-data-view)
  (short-text :hidep t))
