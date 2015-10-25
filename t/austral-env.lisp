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

(defun run-tests ()
  (run! 'tests)
  (when (probe-file +directory+)
    (uiop:delete-directory-tree +directory+ :validate t)))
