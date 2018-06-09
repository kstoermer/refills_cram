(in-package :cl-user)


(defpackage refills-cram
    (:use :cpl :roslisp :cl-transforms :cram-designators :cram-process-modules :cram-language-designator-support)
  (:export :main)
  (:import-from :cram-prolog :def-fact-group :<- :lisp-fun))
