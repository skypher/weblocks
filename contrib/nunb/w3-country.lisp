
(in-package :app)

;; (export '(*w3-countrys* *use-suggest-for-w3-countrys* w3-country
;; 	  w3-country-presentation w3-country-presentation-use-suggest-p
;; 	  w3-country-presentation-input-id
;; 	  w3-country-presentation-choices-id w3-country-parser))

(defparameter *w3-countrys*
  '(("Italia" . "IT") ("United States of America" . "US") ("China" . "CN")
    ("Andorra" . "AD") ("United Arab Emirates" . "AE") ("Afghanistan" . "AF") ("Antigua and Barbuda" . "AG") ("Anguilla" . "AI")
    ("Albania" . "AL") ("Armenia" . "AM") ("Netherlands Antilles" . "AN") ("Angola" . "AO") ("Antarctica" . "AQ") ("Argentina" . "AR")
    ("American Samoa" . "AS") ("Austria" . "AT") ("Australia" . "AU") ("Aruba" . "AW") ("Azerbaijan" . "AZ") ("Bosnia and Herzegovina" . "BA")
    ("Barbados" . "BB") ("Bangladesh" . "BD") ("Belgium" . "BE") ("Burkina Faso" . "BF") ("Bulgaria" . "BG") ("Bahrain" . "BH") ("Burundi" . "BI")
    ("Benin" . "BJ") ("Saint Barthelemy" . "BL") ("Bermuda" . "BM") ("Brunei Darussalam" . "BN") ("Bolivia" . "BO") ("Brazil" . "BR") ("Bahamas" . "BS")
    ("Bhutan" . "BT") ("Bouvet Island" . "BV") ("Botswana" . "BW") ("Belarus" . "BY") ("Belize" . "BZ") ("Canada" . "CA") ("Cocos (Keeling) Islands" . "CC")
    ("Central African Republic" . "CF") ("Congo" . "CG") ("Switzerland" . "CH") ("Cote d'Ivoire" . "CI") ("Cook Islands" . "CK") ("Chile" . "CL")
    ("Cameroon" . "CM") ("China" . "CN") ("Colombia" . "CO") ("Costa Rica" . "CR") ("Cuba" . "CU") ("Cape Verde" . "CV") ("Christmas Island" . "CX")
    ("Cyprus" . "CY") ("Czech Republic" . "CZ") ("Germany" . "DE") ("Djibouti" . "DJ") ("Denmark" . "DK") ("Dominica" . "DM") ("Dominican Republic" . "DO")
    ("Algeria" . "DZ") ("Ecuador" . "EC") ("Estonia" . "EE") ("Egypt" . "EG") ("Eritrea" . "ER") ("Spain" . "ES") ("Ethiopia" . "ET") ("Finland" . "FI")
    ("Fiji" . "FJ") ("Falkland Islands (Malvinas)" . "FK") ("Micronesia, Federated States of" . "FM") ("Faroe Islands" . "FO") ("France" . "FR")
    ("Gabon" . "GA") ("United Kingdom / Great Britain" . "GB") ("Grenada" . "GD") ("Georgia" . "GE") ("French Guiana" . "GF") ("Guernsey" . "GG")
    ("Ghana" . "GH") ("Gibraltar" . "GI") ("Greenland" . "GL") ("Gambia" . "GM") ("Guinea" . "GN") ("Guadeloupe" . "GP") ("Equatorial Guinea" . "GQ")
    ("Greece" . "GR") ("South Georgia and the South Sandwich Islands" . "GS") ("Guatemala" . "GT") ("Guam" . "GU") ("Guinea-Bissau" . "GW")
    ("Guyana" . "GY") ("Hong Kong" . "HK") ("Heard Island and McDonald Islands" . "HM") ("Honduras" . "HN") ("Croatia" . "HR")
    ("Haiti" . "HT") ("Hungary" . "HU") ("Indonesia" . "ID") ("Ireland" . "IE") ("Israel" . "IL") ("Isle of Man" . "IM") ("India" . "IN")
    ("British Indian Ocean Territory" . "IO") ("Iraq" . "IQ") ("Iran" . "IR") ("Iceland" . "IS") ("Italia" . "IT") ("Jersey" . "JE") ("Jamaica" . "JM")
    ("Jordan" . "JO") ("Japan" . "JP") ("Kenya" . "KE") ("Kyrgyzstan" . "KG") ("Cambodia" . "KH") ("Kiribati" . "KI") ("Comoros" . "KM")
    ("Saint Kitts and Nevis" . "KN") ("North Korea" . "KP") ("Republic of Korea" . "KR") ("Kuwait" . "KW") ("Cayman Islands" . "KY")
    ("Kazakhstan" . "KZ") ("Lao PDR" . "LA") ("Lebanon" . "LB") ("Saint Lucia" . "LC") ("Liechtenstein" . "LI") ("Sri Lanka" . "LK")
    ("Liberia" . "LR") ("Lesotho" . "LS") ("Lithuania" . "LT") ("Luxembourg" . "LU") ("Latvia" . "LV") ("Libya" . "LY") ("Morocco" . "MA")
    ("Monaco" . "MC") ("Moldova" . "MD") ("Montenegro" . "ME") ("Madagascar" . "MG") ("Marshall Islands" . "MH") ("Macedonia" . "MK")
    ("Mali" . "ML") ("Myanmar" . "MM") ("Mongolia" . "MN") ("Macao" . "MO") ("Northern Mariana Islands" . "MP") ("Martinique" . "MQ")
    ("Mauritania" . "MR") ("Montserrat" . "MS") ("Malta" . "MT") ("Mauritius" . "MU") ("Maldives" . "MV") ("Malawi" . "MW") ("Mexico" . "MX")
    ("Malaysia" . "MY") ("Mozambique" . "MZ") ("Namibia" . "NA") ("New Caledonia" . "NC") ("Niger" . "NE") ("Norfolk Island" . "NF")
    ("Nigeria" . "NG") ("Nicaragua" . "NI") ("Netherlands" . "NL") ("Norway" . "NO") ("Nepal" . "NP") ("Nauru" . "NR") ("Niue" . "NU")
    ("New Zealand" . "NZ") ("Oman" . "OM") ("Panama" . "PA") ("Peru" . "PE") ("French Polynesia" . "PF") ("Papua New Guinea" . "PG")
    ("Philippines" . "PH") ("Pakistan" . "PK") ("Poland" . "PL") ("Saint Pierre and Miquelon" . "PM") ("Pitcairn" . "PN")
    ("Puerto Rico" . "PR") ("Palestinian Territory" . "PS") ("Portugal" . "PT") ("Palau" . "PW") ("Paraguay" . "PY") ("Qatar" . "QA")
    ("Reunion" . "RE") ("Romania" . "RO") ("Serbia" . "RS") ("Russian Federation" . "RU") ("Rwanda" . "RW") ("Saudi Arabia" . "SA")
    ("Solomon Islands" . "SB") ("Seychelles" . "SC") ("Sudan" . "SD") ("Sweden" . "SE") ("Singapore" . "SG") ("Saint Helena" . "SH")
    ("Slovenia" . "SI") ("Svalbard and Jan Mayen" . "SJ") ("Slovakia" . "SK") ("Sierra Leone" . "SL") ("San Marino" . "SM")
    ("Senegal" . "SN") ("Somalia" . "SO") ("Suriname" . "SR") ("Sao Tome and Principe" . "ST") ("El Salvador" . "SV")
    ("Syrian Arab Republic" . "SY") ("Swaziland" . "SZ") ("Turks and Caicos Islands" . "TC") ("Chad" . "TD") ("French Southern Territories" . "TF")
    ("Thailand" . "TH") ("Tajikistan" . "TJ") ("Tokelau" . "TK") ("Turkmenistan" . "TM") ("Tunisia" . "TN") ("Tonga" . "TO") ("Turkey" . "TR")
    ("Trinidad and Tobago" . "TT") ("Tuvalu" . "TV") ("Taiwan" . "TW") ("Tanzania" . "TZ") ("Ukraine" . "UA") ("Uganda" . "UG")
    ("United States" . "US") ("Uruguay" . "UY") ("Uzbekistan" . "UZ") ("Holy See (Vatican City State)" . "VA")
    ("Saint Vincent and the Grenadines" . "VC") ("Venezuela" . "VE") ("Virgin Islands, British" . "VG") ("Virgin Islands, U.S." . "VI")
    ("Viet Nam" . "VN") ("Vanuatu" . "VU") ("Wallis and Futuna" . "WF") ("Samoa" . "WS") ("Yemen" . "YE") ("Mayotte" . "YT")
    ("South Africa" . "ZA") ("Zambia" . "ZM") ("Zimbabwe" . "ZW"))
  "A alist of W3C country names and their abbreviations/TLDs.")

(defparameter *w3-countrys-abreviation->name*
  (mapcar (lambda (state)
	    (cons (cdr state) (car state)))
	  *w3-countrys*)
  "An alist of W3C country abbreviations mapped to country names.")

(defparameter *use-suggest-for-w3-countrys* t
  "If true, suggest snippet will be used to render w3-country types in
forms. A simple dropdown will be used otherwise.")

(defun country-code->name (co)
  (assoc co *w3-countrys-abreviation->name*))

(defun w3-country-p (str)
  "Used by the type specifier 'w3-country' to determine if a string is a
country."
  (not (null (member str *w3-countrys* :test #'string= :key #'cdr))))

(deftype w3-country ()
  '(satisfies w3-country-p))

(defclass w3-country-presentation (input-presentation)
  ((use-suggest-p :initform *use-suggest-for-w3-countrys*
		  :initarg :use-suggest-p
		  :accessor w3-country-presentation-use-suggest-p
		  :documentation "If set to true, suggest snippet will
		  be used as a country input control. Otherwise,
		  dropdown will be used.")
   (input-id :initform (gensym)
	     :initarg :input-id
	     :accessor w3-country-presentation-input-id
	     :documentation "An input ID passed to suggest or
	     dropdown.")
   (choices-id :initform (gensym)
	       :initarg :choices-id
	       :accessor w3-country-presentation-choices-id
	       :documentation "A choices ID passed to suggest.")))

(defmethod render-view-field-value (value (presentation w3-country-presentation) 
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let ((selections (mapcar #'car *w3-countrys*))
	  (default-value (if intermediate-value-p
			     intermediate-value
			     (append (ensure-list value)
				     (ensure-list
				      (cdr (assoc value *w3-countrys-abreviation->name*
						  :test #'equalp))))))
	  (welcome-name "Nazione"))
      (if (w3-country-presentation-use-suggest-p presentation)
	  (render-suggest (view-field-slot-name field)
			  selections
			  :default-value default-value
			  :welcome-name welcome-name
			  :max-length (input-presentation-max-length presentation)
			  :input-id (w3-country-presentation-input-id presentation)
			  :choices-id (w3-country-presentation-choices-id presentation))
	  (render-dropdown (view-field-slot-name field) selections
			   :selected-value default-value
			   :welcome-name welcome-name
			   :id (w3-country-presentation-input-id presentation))))))

(defclass w3-country-parser (parser)
  ((error-message :initform "valid nazione"))
  (:documentation "A parser designed to parse strings into
  a w3c country."))

(defmethod parse-view-field-value ((parser w3-country-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let ((value (string-trim +whitespace-characters+ value)))
    (if (empty-p value)
	(values t nil)
	(if (eq (length value) 2)
	    (when (w3-country-p (string-upcase value))
	      (values t t (string-upcase value)))
	    (let ((state (assoc value *w3-countrys* :test #'equalp)))
	      (when state
		(values t t (cdr state))))))))

;;; Scaffolding magic
(defmethod typespec->view-field-presentation ((scaffold form-scaffold)
					      (typespec (eql 'w3-country)) args)
  (values t (make-instance 'w3-country-presentation)))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'w3-country)) args)
  (values t (make-instance 'w3-country-parser)))

