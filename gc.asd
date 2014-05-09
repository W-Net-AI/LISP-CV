;;; -*- mode: lisp; indent-tabs: nil -*-

(asdf:defsystem #:gc
  :name "gc"
  :author "Joe W. BiMedina <wnetai@yahoo.com>."
  :version "0.1"
  :description "Lisp-CV garbage collection module"
  :depends-on (#:cffi
               #:trivial-garbage
               #:lisp-executable
               #:lisp-cv)
  :serial t
  :components ((:module "gc"
               ;:serial t
               :components
               ((:file "package") 
               (:file "gc" :depends-on ("package"))))))
