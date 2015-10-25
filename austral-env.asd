(defsystem austral-env
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:uiop
               :trivial-download
               :ironclad
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "ql"))))
  :description "An implementation & Quicklisp installation manager for Austral."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op austral-env-test))))
