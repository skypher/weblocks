(in-package :weblocks-util)

(wexport '(*parts-md5-hash* *parts-md5-context-hash* *process-html-parts-p* get-html-part get-html-part-context get-html-part-children nested-html-part reset-html-parts-set update-html-parts-connections get-html-parts-root-hash)
         '(t util))

(defvar *parts-md5-hash*)
(setf (documentation '*parts-md5-hash* 'variable)
      "A hash with key of html part md5 hash 
       and the value of list with html part as car
       and list of children as cdr")

(defvar *parts-md5-context-hash*)
(setf (documentation '*parts-md5-context-hash* 'variable)
      "A hash with key of html part md5 hash 
       and the value of html part context")

(defvar *current-html-part-children* nil)
(setf (documentation '*current-html-part-children* 'variable)
      "A place where we can put md5 hashes of children parts of processed html part.
       Should be used from body of NESTED-HTML-PART macro")

(defvar *process-html-parts-p* (lambda () nil))
(setf (documentation '*current-html-part-children* 'variable)
      "Contains callback which should return boolean value 
       indicating whether html parts should be collected and processed")

(defun process-html-parts-p ()
  "Whether or not to capture html parts information"
  (funcall *process-html-parts-p*))

(defmacro nested-html-part (context &body body)
  "Adds part of html (a string received as a result of &body evaluation)
   and context associated with it to html parts set."
  `(let ((value))
     (declare (special *current-html-part-children* *parts-md5-hash* *parts-md5-context-hash*))
     (setf *current-html-part-children* nil)
     (setf value (progn ,@body))
     (when (process-html-parts-p)
       (setf (gethash (md5 value) *parts-md5-context-hash*) ,context)
       (setf (gethash (md5 value) *parts-md5-hash*)
             (cons value (mapcar #'md5 *current-html-part-children*))))
     value))

(defun reset-html-parts-set ()
  "Resets html parts set"
  (declare (special *parts-md5-hash* *parts-md5-context-hash*))

  (setf *parts-md5-hash* (make-hash-table :test 'equal))
  (setf *parts-md5-context-hash* (make-hash-table :test 'equal)))

(defun is-substring-of-p (string-child string-parent)
  "Checks whether one string is substring of another and has less length."
  (and 
    (> (length string-parent) (length string-child))
    (search string-child string-parent)
    t))

(defun get-html-part (key)
  "Returns html part string by its md5"
  (declare (special *parts-md5-hash*))
  (car (gethash key *parts-md5-hash*)))

(defun get-html-part-children (key)
  "Returns html part child parts by its md5"
  (declare (special *parts-md5-hash*))
  (cdr (gethash key *parts-md5-hash*)))

(defun set-html-part-children (key children)
  "Sets html part child parts by its md5"
  (declare (special *parts-md5-hash*))
  (setf (cdr (gethash key *parts-md5-hash*)) children))

(defun remove-html-part (key)
  "Removes html part by its md5"
  (declare (special *parts-md5-hash*))
  (remhash key *parts-md5-hash*))

(defun get-html-part-context (key)
  "Returns html part context by its md5"
  (declare (special *parts-md5-context-hash*))
  (gethash key *parts-md5-context-hash*))

(defun update-html-parts-connections ()
  "Builds html parts tree from html parts list. 
   Each html part child is a substring of its parent.
   Html parts which are empty strings removed from html parts set."
  (declare (special *parts-md5-hash*))
  (unless 
    (process-html-parts-p)
    (return-from update-html-parts-connections))

  ; Removing messy items
  (let ((empty-string-md5 (md5 "")))
    (loop for key being the hash-keys of *parts-md5-hash* do 
          ; Remove empty strings from html part children
          (set-html-part-children 
            key
            (remove 
              empty-string-md5 
              (get-html-part-children key)
              :test #'string=))
          ; Remove empty string html parts
          (when (string= key empty-string-md5)
            (remove-html-part key))))

  (loop for key being the hash-keys of *parts-md5-hash* do 
        (let ((value (get-html-part key))
              (child-value))

          ; Step first, adding child-parent connections
          (loop for child-key being the hash-keys of *parts-md5-hash* do 
                (setf child-value (get-html-part child-key))
                (if (and 
                      (not (zerop (length child-value)))
                      (is-substring-of-p child-value value))
                  (pushnew child-key (cdr (gethash key *parts-md5-hash*)) :test #'string=)))

          ; Step second, removing messy connections
          (loop for child-key being the hash-keys of *parts-md5-hash* do 
                (setf children (get-html-part-children child-key))

                (loop for child in children do 
                      (let ((child-children (get-html-part-children child)))
                        (loop for i in child-children do 
                              (when (find i children :test #'string=)
                                (setf children (remove i children :test #'string=))))))

                (set-html-part-children child-key children)))))

(defun update-current-html-part-children (args)
  "Takes strings list as an argument and adds strings 
   which are html parts (which are in html parts set)
   to *current-html-part-children* list "
  (declare (special *parts-md5-hash*))
  (loop for value on args
        if (and (stringp value) (gethash (md5 value) *parts-md5-hash*))
        do
        (push value *current-html-part-children*)))

(defun get-html-parts-root-hash ()
  "Returns md5 hash of root html part in parts tree"
  (declare (special *parts-md5-hash*))
  (let ((max-length (loop for key being the hash-keys of *parts-md5-hash* 
                          maximize (length (get-html-part key)))))
    (loop for key being the hash-keys of *parts-md5-hash* 
          if (= max-length (length (get-html-part key)))
          do 
          (return-from get-html-parts-root-hash key))))

