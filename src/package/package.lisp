;;;; package.lisp

(defpackage #:injection
  (:use
   #:cl
   #:cl-yaml)
  (:export
   #:*container-singleton*
   #:get-parameter
   #:get-service
   #:Container-Factory
   #:Container-Get-Parameter
   #:Container-Get-Service))
