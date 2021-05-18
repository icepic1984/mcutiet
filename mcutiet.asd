(defsystem #:mcutiet
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria :babel :usocket :trivial-gray-streams :bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components
                ((:file "streams")
                 (:file "queue")
                 (:file "binary-parser")
                 (:file "message")
                 (:file "io")
                 (:file "client")
                 )))
  :description "Simple mqtt library for common lisp"
  :in-order-to ((test-op (test-op "mcutiet/tests"))))

(defsystem #:mcutiet/tests
  :author ""
  :license ""
  :depends-on (:mcutiet :fiveam)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "suites")
                 (:file "streams")
                 (:file "message")
                 (:file "queue")
                 (:file "io"))))
  :description "Test system for mcutiet library"
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:test-all :mcutiet.tests))))


(defsystem #:mcutiet/examples
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:mcutiet)
  :components ((:module "examples"
                :serial t
                :components
                ((:file "01-subscribe"))))
  :description "Examples for mcutiet")
