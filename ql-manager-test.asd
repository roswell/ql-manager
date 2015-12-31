(defsystem ql-manager-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:ql-manager
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "ql")))))
