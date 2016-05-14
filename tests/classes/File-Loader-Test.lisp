;;;; File-Loader-Test.lisp

(in-package #:injection-test)

(fiveam:def-suite file-loader :description "Test the project")
(fiveam:in-suite file-loader)

(fiveam:test test-factory

  (signals
      (error "Missing file")
    (injection::File-Loader-Factory "/tmp/fake-file-FLT"))

  (let ((file-loader (injection::File-Loader-Factory "/dev/null")))
    (is (typep file-loader 'injection::file-loader))
    )

  )

;;(fiveam:run!)
