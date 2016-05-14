;; Injection - YAML based Dependency Injection for Common Lisp
;; Copyright (C) 2016 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; injection.asd

(in-package :cl-user)
(defpackage injection-asd
  (:use :cl :asdf))
(in-package :injection-asd)

(defsystem #:injection-asd
  :version "0.1"
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (:split-sequence
               :glyphs)
  :serial t
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("file-loader"))
                 (:file "file-loader"))))
  :description "A story driven game"
  :in-order-to ((test-op (load-op injection-test))))
