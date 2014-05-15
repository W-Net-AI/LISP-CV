;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; lisp-cv.lisp
;;;; OpenCV bindings
;;;; Library loading and common code
(in-package :lisp-cv)

;;; Foreign library setup
(when (member :darwin cl:*features*)
  (pushnew #p"/opt/local/lib/" *foreign-library-directories*))


(define-foreign-library highgui
    (:unix "/usr/local/lib/libopencv_highgui.so.3.0.0"))
(use-foreign-library highgui)

(define-foreign-library calib3d
    (:unix "/usr/local/lib/libopencv_calib3d.so.3.0.0"))
(use-foreign-library calib3d)

(define-foreign-library contrib
    (:unix "/usr/local/lib/libopencv_contrib.so.3.0.0"))
(use-foreign-library contrib)

(define-foreign-library core
    (:unix "/usr/local/lib/libopencv_core.so.3.0.0"))
(use-foreign-library core)

(define-foreign-library features2d
    (:unix "/usr/local/lib/libopencv_features2d.so.3.0.0"))
(use-foreign-library features2d)

(define-foreign-library flann
    (:unix "/usr/local/lib/libopencv_flann.so.3.0.0"))
(use-foreign-library flann)

;;(define-foreign-library gpu
;;    (:unix "/usr/local/lib/libopencv_gpu.so.3.0.0"))
;;(use-foreign-library gpu)

(define-foreign-library imgproc
    (:unix "/usr/local/lib/libopencv_imgproc.so.3.0.0"))
(use-foreign-library imgproc)

(define-foreign-library legacy
    (:unix "/usr/local/lib/libopencv_legacy.so.3.0.0"))
(use-foreign-library legacy)

(define-foreign-library ml 
    (:unix "/usr/local/lib/libopencv_ml.so.3.0.0"))
(use-foreign-library ml)

(define-foreign-library nonfree
    (:unix "/usr/local/lib/libopencv_nonfree.so.3.0.0"))
(use-foreign-library nonfree)

(define-foreign-library objdetect
    (:unix "/usr/local/lib/libopencv_objdetect.so.3.0.0"))
(use-foreign-library objdetect)

(define-foreign-library photo
    (:unix "/usr/local/lib/libopencv_photo.so.3.0.0"))
(use-foreign-library photo)

(define-foreign-library stitching
    (:unix "/usr/local/lib/libopencv_stitching.so.3.0.0"))
(use-foreign-library stitching)

(define-foreign-library superres
    (:unix "/usr/local/lib/libopencv_superres.so.3.0.0" ))
(use-foreign-library superres)

(define-foreign-library video
    (:unix "/usr/local/lib/libopencv_video.so.3.0.0"))
(use-foreign-library video)

(define-foreign-library videostab
    (:unix "/usr/local/lib/libopencv_videostab.so.3.0.0"))
(use-foreign-library videostab)

;; On new 3.0.0 OpenCV build and 154 lisp-cv-master
;; cd ~/Documents/opencv-master/build/modules/c/src
;; g++ -Wall -shared -fPIC -o opencv_generated.so opencv_generated.cpp
(define-foreign-library opencv_c
    (:unix "/usr/local/lib/opencv_generated.so"))
(use-foreign-library opencv_c)

;; On new 3.0.0 OpenCV build
;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o mat.so mat.cpp
(define-foreign-library mat
  (:darwin "mat.dylib")
  (:unix "/usr/local/lib/mat.so")
  (t (:default "mat")))
(use-foreign-library mat)

;; On new 3.0.0 OpenCV build
;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o point.so point.cpp
(define-foreign-library point
  (:darwin "point.dylib")
  (:unix "/usr/local/lib/point.so")
  (t (:default "point")))
(use-foreign-library point)

;; On new 3.0.0 OpenCV build
;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o rect.so rect.cpp
(define-foreign-library rect
  (:darwin "rect.dylib")
  (:unix "/usr/local/lib/rect.so")
  (t (:default "rect")))
(use-foreign-library rect)

;; On new 3.0.0 OpenCV build
;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o scalar.so scalar.cpp
(define-foreign-library scalar
  (:darwin "scalar.dylib")
  (:unix "/usr/local/lib/scalar.so")
  (t (:default "scalar")))
(use-foreign-library scalar)

;; On new 3.0.0 OpenCV build
;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o size.so size.cpp
(define-foreign-library size
  (:darwin "size.dylib")
  (:unix "/usr/local/lib/size.so")
  (t (:default "size")))
(use-foreign-library size)

;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o excluded_functions.so excluded_functions.cpp
(define-foreign-library excluded_functions
  (:darwin "excluded_functions.dylib")
  (:unix "/usr/local/lib/excluded_functions.so")
  (t (:default "excluded_functions")))
(use-foreign-library excluded_functions)

;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o interop.so interop.cpp
(define-foreign-library interop
  (:darwin "interop.dylib")
  (:unix "/usr/local/lib/interop.so")
  (t (:default "interop")))
(use-foreign-library interop)

;; cd ~/Documents/opencv-master/modules/c/src
;; g++ -Wall -shared -fPIC -o extra_functions.so extra_functions.cpp
(define-foreign-library extra_functions
  (:darwin "extra_functions.dylib")
  (:unix "/usr/local/lib/extra_functions.so")
  (t (:default "extra_functions")))
(use-foreign-library extra_functions)

;;; General macros and functions
(defmacro defanonenum (&body enums)
  "Converts anonymous enums to Lisp constants."
  `(cl:progn ,@(cl:loop for value in enums
			for index = 0 then (cl:1+ index)
			when (cl:listp value) 
			do (cl:setf index (cl:second value)
				    value (cl:first value))
			collect `(cl:defconstant ,value ,index))))
