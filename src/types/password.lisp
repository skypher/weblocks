(in-package weblocks)

(export '(password *max-password-length*))

(deftype password () 'string)

(defparameter *max-password-length* 10
  "Default maximum for the length of the password field")

(defslotmethod render-form-value (obj slot-name (slot-type (eql 'password)) slot-value &rest
				    keys &key slot-path &allow-other-keys)
  "Password slotmethod that specializes on rendering password sting, simigliar code  render-form-value spcailized on standard object "
  (let ((attributized-slot-name (attributize-name (if slot-name slot-name (last-item slot-path)))))
    (render-password  attributized-slot-name
		      (apply #'form-print-object obj slot-name
				slot-type slot-value keys)
		      :maxlength (max-raw-slot-input-length obj slot-name slot-type))))


(defslotmethod max-raw-slot-input-length (obj slot-name (slot-type (eql 'password)))
  *max-password-length*)

(defslotmethod data-print-object (obj slot-name (slot-type (eql 'password)) slot-value
				      &rest args)
  "Renders asterisks in all cases for security reasons, override to customize behaviour"
  (format nil "*******" ))

(defslotmethod form-print-object (obj slot-name (slot-type (eql 'password)) slot-value
				      &rest args)
  "Used to dispatch rendering slot-value to data-print-object with (slot-type standard-object)"
  (when slot-value
    (apply #'data-print-object obj slot-name t slot-value args)))

