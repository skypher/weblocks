
(in-package :weblocks)

(export '(file-upload file-upload-presentation file-upload-parser
          file-upload-parser-upload-directory file-upload-parser-file-name))

;;; file upload presentation
(defclass file-upload-presentation (form-presentation)
  ()
  (:documentation "Present a field for uploading a file."))

(defmethod render-view-field-value (value (presentation file-upload-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values &allow-other-keys)
  (with-html
    (:input :type "file" :name (attributize-name (view-field-slot-name field)))))

;;; file upload parser
(defclass file-upload-parser (parser)
  ((upload-directory :accessor file-upload-parser-upload-directory
                     :initarg :upload-directory
                     :documentation "The directory where files will be copied on upload.")
   (file-name :accessor file-upload-parser-file-name
              :initform :browser
              :initarg :file-name
              :documentation "Can be :browser, in which case name
              provided by the browser will be used, :unique, in which
              case a random hash will be used, or a string that will
              be used directly."))
  (:documentation "A parser designed to handle file uploads."))

(defmethod parse-view-field-value ((parser file-upload-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (when (null value)
    (return-from parse-view-field-value (values t nil)))
  (when (stringp value)
    (error "The value of the upload field is incorrect. Please turn on
    multipart requests and turn off ajax."))
  (flet ((octet-string->utf-8 (s)
           "Kludge to fix librfc2388 bug."
           (hunchentoot::octets-to-string
	     (map 'vector
		  (lambda (c)
		    (let ((x (char-int c)))
		      (assert (and (not (minusp x))
				   (< x 256)))
		      x))
		  s))))
    (let* ((temp-path (first value))
           (browser-name (octet-string->utf-8 (second value)))
           (file-name (etypecase (file-upload-parser-file-name parser)
                        (symbol (ecase (file-upload-parser-file-name parser)
                                  (:browser browser-name)
                                  (:unique (hunchentoot::create-random-string))))
                        (string (file-upload-parser-file-name parser)))))
      (copy-file temp-path
                 (merge-pathnames file-name
                                  (file-upload-parser-upload-directory parser))
                 :if-exists :supersede)
      (values t value file-name))))
