;;;; package.lisp

(defpackage #:injection
  (:use
   #:cl
   #:split-sequence
   #:cl-yaml)
  (:export
   #:*container-singleton*
   #:get-parameter
   #:get-service
   #:Container-Factory
   #:Container-Get-Parameter
   #:Container-Get-Service))
