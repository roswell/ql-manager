(defsystem ql-manager
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/roswell/ql-manager"
  :bug-tracker "https://github.com/roswell/ql-manager/issues"
  :source-control (:git "git@github.com:roswell/ql-manager.git")
  :depends-on (:uiop
               :trivial-download
               :ironclad
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "ql"))))
  :description "Download and manage Quicklisp environments."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ql-manager))))
