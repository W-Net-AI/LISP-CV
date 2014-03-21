;;; -*- mode: lisp; indent-tabs: nil -*-

;;(ql:quickload "ieee-floats")
(asdf:defsystem #:lisp-cv
  :name "lisp-cv"
  :author "Joe W. BiMedina <wnetai@yahoo.com>. Thanks to J. Bromley <jbromley@gmail.com> inspiring me and for getting me started on this project"
  :version "0.1"
  :description "OpenCV bindings for SBCL"
  :depends-on (#:cffi
               #:swank 
               ;Commenting out swank above -> gets rid of - WARNING: Not reloading SWANK.  Package already exists. -
               ;in inferior-lisp at startup - re-add to get live code editing. 
)
  :serial t
  :components ((:file "package") 
	           (:file "lisp-cv" :depends-on ("package"))
               (:file "constants" :depends-on ("package" "lisp-cv"))
               (:file "core" :depends-on ("package" "lisp-cv" "constants"))
               (:file "macros" :depends-on ("package" "lisp-cv" "constants" "core"))
	           (:file "imgproc" :depends-on ("package" "lisp-cv" "constants" "core" "macros"))
	           (:file "highgui" :depends-on ("package" "lisp-cv" "constants" "core" "macros"))))
