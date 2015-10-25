(defsystem austral-env-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:austral-env
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "austral-env")))))
