(in-package :cl-user)
(defpackage austral-env-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :austral-env-test)

(def-suite tests
  :description "austral-env tests.")
(in-suite tests)

(defparameter +directory+
  (asdf:system-relative-pathname :austral-env #p"t/ql/"))

(test get-installer
  (let ((manager (make-instance 'austral-env.ql:quicklisp-manager
                                :directory +directory+)))
    (finishes
      (austral-env.ql:download-quicklisp-installer manager))))

(test db
  (let ((manager (make-instance 'austral-env.ql:quicklisp-manager
                                :directory +directory+)))
    ;; Save and restore an empty database
    (finishes
      (austral-env.ql:write-db manager))
    (finishes
      (austral-env.ql:load-db manager))))

(defun run-tests ()
  (run! 'tests)
  (when (probe-file +directory+)
    (uiop:delete-directory-tree +directory+ :validate t)))
