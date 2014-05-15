;; File lisp-executable-example.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "lisp-executable"))

(defsystem lisp-executable-example
  :components ((:modules "example/"
                         :serial t
                         :components ((:file "main")
                                      (lisp-executable:executable "example-program" :program ("LISP-EXECUTABLE.EXAMPLE" "EXAMPLE-PROGRAM"))))))  