;;; -*- mode: lisp; indent-tabs: nil -*-

;;(ql:quickload "ieee-floats")
(asdf:defsystem #:lisp-cv
  :name "lisp-cv"
  :author "Joe W. BiMedina <wnetai@yahoo.com>. Thanks to J. Bromley <jbromley@gmail.com> inspiring me and for getting me started on this project"
  :version "0.1"
  :description "OpenCV bindings for SBCL"
  :depends-on (#:cffi
               #:swank 
               #:trivial-garbage
               ;Commenting out swank above -> gets rid of - WARNING: Not reloading SWANK.  Package already exists. -
               ;in inferior-lisp at startup - re-add to get live code editing. 
)
  :serial t
  :components ((:file "package") 
               (:file "lookup") 
	           (:file "lisp-cv" :depends-on ("package"))
               (:file "constants" :depends-on ("package" "lisp-cv"))
               (:file "types" :depends-on ("package" "lisp-cv"))
               (:file "vector" :depends-on ("package" "lisp-cv"))
               (:file "delete" :depends-on ("package" "lisp-cv"))
               (:file "destruct" :depends-on ("package" "lisp-cv"))
               (:file "with-macros" :depends-on ("package" "lisp-cv" "delete"))
               (:file "core" :depends-on ("package" "lisp-cv" "constants" "types"))
	           (:file "imgproc" :depends-on ("package" "lisp-cv" "constants" "types"))
	           (:file "highgui" :depends-on ("package" "lisp-cv" "constants" "types"))
               (:file "calib3d" :depends-on ("package" "lisp-cv" "constants" "types"))
               (:file "objdetect" :depends-on ("package" "lisp-cv" "constants" "types"))
               (:file "features2d" :depends-on ("package" "lisp-cv" "constants" "types"))
               (:file "nonfree" :depends-on ("package" "lisp-cv" "constants" "types"))
               (:file "macros" :depends-on ("package" "lisp-cv" "constants" "types" "delete" "destruct" "with-macros" "vector" "core" 
                                            "imgproc" "highgui" "calib3d" "objdetect" "features2d" "nonfree"))))
