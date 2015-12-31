(in-package :cl-user)
(defpackage ql-manager
  (:use :cl)
  ;; Constants
  (:export :+ql-installer-url+
           :+ql-installer-sha256+
           :+ql-installer-path+)
  ;; Classes and accessors
  (:export :manager
           :manager-directory
           :install
           :install-name
           :install-directory)
  ;; Methods
  (:export :installer-path
           :download-quicklisp-installer
           :verify-quicklisp-installer
           :database-path
           :write-db
           :load-db)
  (:documentation "Quicklisp installation manager."))
(in-package :ql-manager)

;;; Constants

(defparameter +ql-installer-url+
  "http://beta.quicklisp.org/quicklisp.lisp"
  "The URL of the Quicklisp installer.")

(defparameter +ql-installer-sha256+
  "4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17"
  "The Quicklisp installer's SHA-256 hash.")

(defparameter +ql-installer-path+
  #p"ql.lisp"
  "The path, inside the Quicklisp manager's directory, where the Quicklisp
  installer file is ept.")

;;; Errors

(define-condition verification-failed ()
  ((hash :reader error-hash
         :initarg :hash
         :type string
         :documentation "The hash of the Quicklisp installer."))
  (:report
   (lambda (condition stream)
     (format stream "SHA-256 verification filed.~%~%Wanted hash: ~A~%~%File hash: ~A"
             +ql-installer-sha256+
             (error-hash condition))))
  (:documentation "Raised when SHA256 verification fails."))

;;; Classes

(defclass install ()
  ((name :reader install-name
         :initarg :name
         :type string
         :documentation "The name of the Quicklisp installation.")
   (directory :reader install-directory
              :initarg :directory
              :type pathname
              :documentation "The directory where the Quicklisp install is
              to be stored stored."))
  (:documentation "A Quicklisp install."))

(defclass manager ()
  ((directory :reader manager-directory
              :initarg :directory
              :type pathname
              :documentation "The directory where Quicklisp installations are
              stored.")
   (installs :accessor manager-installs
             :initarg :installs
             :initform (list)
             :type list
             :documentation "A list of Quicklisp installations."))
  (:documentation "A Quicklisp manager is an object that manager installations."))

;;; Simple methods

(defmethod installer-path ((man manager))
  "Return the absolute path to the Quicklisp installer."
  (merge-pathnames +ql-installer-path+ (manager-directory man)))

;;; Download Quicklisp installer

(defmethod download-quicklisp-installer ((man manager))
  "Ensure the Quicklisp installer has been downloaded."
  (trivial-download:download +ql-installer-url+ (installer-path man))
  (verify-quicklisp-installer man)
  nil)

;;; Verification

(defun sha256 (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array string))))

(defmethod verify-quicklisp-installer ((man manager))
  "Verify the Quicklisp installer has the proper hash."
  (let ((hash (sha256 (uiop:read-file-string (installer-path man)))))
    (if (string= +ql-installer-sha256+ hash)
        t
        (error 'verification-failed :hash hash))))

;;; Storage

(defmethod database-path ((man manager))
  "Return the path to the database file."
  (merge-pathnames #p"manager.json"
                   (manager-directory man)))

(defmethod write-db ((man manager))
  "Write the database of installs to the database file."
  (with-open-file (stream (database-path man)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        (yason:with-object-element ("installs")
          (yason:with-array ()
            (loop for install in (manager-installs man) do
              (yason:with-object ()
                (yason:Encode-object-elements
                 "name" (install-name install)
                 "directory" (namestring (install-directory install))))))))))
  man)

(defmethod load-db ((man manager))
  "Load the stored data for this manager. This clears and overwrites the list of
  Quicklisp installs."
  (let ((data (yason:parse (database-path man))))
    (setf (manager-installs man)
          (mapcar #'(lambda (table)
                      (make-instance 'install
                                     :name (gethash "name" table)
                                     :directory (parse-namestring
                                                 (gethash "directory" table))))
                  (gethash "installs" data))))
  man)

;;; Logging

(defmethod download-quicklisp-installer :before ((man manager))
  (format t "~%Downloading the Quicklisp installer..."))

(defmethod verify-quicklisp-installer :before ((man manager))
  (format t "~%Verifying installer..."))
