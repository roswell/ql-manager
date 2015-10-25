(in-package :cl-user)
(defpackage austral-ql-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :austral-ql-test)

(def-suite tests
  :description "austral-ql tests.")
(in-suite tests)

(defparameter +directory+
  (asdf:system-relative-pathname :austral-ql #p"t/ql/"))

(test get-installer
  (let ((manager (make-instance 'austral-ql:quicklisp-manager
                                :directory +directory+)))
    (finishes
      (austral-ql:download-quicklisp-installer manager))))

(test db
  (let ((manager (make-instance 'austral-ql:quicklisp-manager
                                :directory +directory+)))
    ;; Save and restore an empty database
    (is-false
     (probe-file (austral-ql:database-path manager)))
    (finishes
      (austral-ql:write-db manager))
    (finishes
      (austral-ql:load-db manager))
    (is-true
     (probe-file (austral-ql:database-path manager)))))

(defun run-tests ()
  (run! 'tests)
  (when (probe-file +directory+)
    (uiop:delete-directory-tree +directory+ :validate t)))
