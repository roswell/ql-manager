(in-package :cl-user)
(defpackage ql-manager-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :ql-manager-test)

(def-suite tests
  :description "ql-manager tests.")
(in-suite tests)

(defparameter +directory+
  (asdf:system-relative-pathname :ql-manager #p"t/ql/"))

(test get-installer
  (let ((manager (make-instance 'ql-manager:manager
                                :directory +directory+)))
    (finishes
      (ql-manager:download-quicklisp-installer manager))))

(test db
  (let ((manager (make-instance 'ql-manager:manager
                                :directory +directory+)))
    ;; Save and restore an empty database
    (is-false
     (probe-file (ql-manager:database-path manager)))
    (finishes
      (ql-manager:write-db manager))
    (finishes
      (ql-manager:load-db manager))
    (is-true
     (probe-file (ql-manager:database-path manager)))))

(defun run-tests ()
  (run! 'tests)
  (when (probe-file +directory+)
    (uiop:delete-directory-tree +directory+ :validate t)))
