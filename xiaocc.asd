(defsystem "xiaocc"
  :version "0.0.1"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ("trivia" "str" "iterate" "serapeum")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "scanner")
		 (:file "ast" :depends-on ("scanner")))))
  :description "A small C compiler"
  :in-order-to ((test-op (test-op "xiaocc/tests"))))

(defsystem "xiaocc/tests"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ("xiaocc"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for xiaocc"
  :perform (test-op (op c) (symbol-call :rove :run c)))
