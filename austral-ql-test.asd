(defsystem austral-ql-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:austral-ql
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "ql")))))
