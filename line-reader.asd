(defpackage :line-reader-system
  (:use :common-lisp :asdf))

(in-package :line-reader-system)

(defsystem :line-reader
  :name "line-reader"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :maintainer"Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.2"
  :licence "MIT"
  :description "user customized line stream"
  :components
  ((:file "line-reader"))
  :serial t)