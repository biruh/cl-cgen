(defsystem "cl-cgen"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:trivia :serapeum )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "tag")
                 (:file "transform")
                 (:file "api")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "cl-cgen/tests"))))

(defsystem "cl-cgen/tests"
  :author ""
  :license ""
  :depends-on ("cl-cgen"
               "rove")
  :components ((:module "tests"
                :components
                ()))
  :description "Test system for cl-cgen"
  :perform (test-op (op c) (symbol-call :rove :run c)))
