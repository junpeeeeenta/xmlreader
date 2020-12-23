(defpackage :xmlreader-system
  (:use :common-lisp :asdf))

(in-package :xmlreader-system)

(defsystem :xmlreader
  :name "xmlreader"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :maintainer"Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :licence "MIT"
  :description "XML stream reader"
  :depends-on ("line-reader")
  :components
  ((:file "xmlreader"))
  :serial t)