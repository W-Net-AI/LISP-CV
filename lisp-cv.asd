;;; -*- mode: lisp; indent-tabs: nil -*-

(asdf:defsystem #:lisp-cv
  :name "lisp-cv"
  :author "Joe W. BiMedina <wnetai@yahoo.com>."
  :version "0.1"
  :description "Common Lisp bindings to OpenCV."
  :depends-on (#:cffi
               ;#:cffi-libffi
               #:swank 
               #:trivial-garbage
               #:simple-utils
               #:lisp-executable
               ;Commenting out swank above -> gets rid of - WARNING: Not reloading SWANK.  Package already exists. -
               ;in inferior-lisp at startup - re-add to get live code editing. 
)
  :serial t
  :components ((:file "package") 
               (:file "lookup") 
	           (:file "lisp-cv" :depends-on ("package"))
               (:file "constants" :depends-on ("package" "lisp-cv"))
               (:file "types" :depends-on ("package" "lisp-cv"))
               (:file "finalizers" :depends-on ("package" "lisp-cv"))
               (:file "vector" :depends-on ("package" "lisp-cv" "finalizers"))
               (:file "delete" :depends-on ("package" "lisp-cv" "finalizers"))
               (:file "with-macros" :depends-on ("package" "lisp-cv" "delete" "finalizers"))
               (:file "core" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
	           (:file "imgproc" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
	           (:file "highgui" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "calib3d" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "objdetect" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "features2d" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "nonfree" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "contrib" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers"))
               (:file "macros" :depends-on ("package" "lisp-cv" "constants" "types" "finalizers" "delete" "with-macros" "vector" "core" 
                                            "imgproc" "highgui" "calib3d" "objdetect" "features2d" "nonfree" "contrib"))))
