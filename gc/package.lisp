;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :gc
  (:nicknames #:gc #:t)
  (:use #:cffi #:common-lisp #:swank #:trivial-garbage #:lisp-executable ;#:lisp-cv
) 
  (:export 

;; Macros

   #:alloc


;; Vectors
   #:%mean
   #:vec-char
   #:c-arr-to-vec-char
   #:arr-to-vec-char
   #:vec-dmatch
   #:c-arr-to-vec-dmatch
   #:arr-to-vec-dmatch
   #:vec-double
   #:c-arr-to-vec-double
   #:arr-to-vec-double
   #:vec-float
   #:c-arr-to-vec-float
   #:arr-to-vec-float
   #:vec-int
   #:c-arr-to-vec-int
   #:arr-to-vec-int
   #:vec-key-point
   #:c-arr-to-vec-key-point
   #:arr-to-vec-key-point
   #:vec-point
   #:c-arr-to-vec-point
   #:arr-to-vec-point
   #:vec-point-2f
   #:c-arr-to-vec-point-2f
   #:arr-to-vec-point-2f
   #:vec-rect
   #:c-arr-to-vec-rect
   #:arr-to-vec-rect
   #:vec-uchar
   #:c-arr-to-vec-uchar
   #:arr-to-vec-uchar




;; core - Basic Structures

;; Mat*

   #:<<
   #:>>
   #:add
   #:adjust-roi
   #:clone
   #:col-range
   #:diag
   #:div
   #:dmatch
   #:force
   #:inv
   #:keypoint
   #:mat
   #:mat-expr-t
   #:mat-eye
   #:mat-ones
   #:mat-size
   #:mat-zeros
   #:mul
   #:point
   #:point-init
   #:point-2d
   #:point-2d-x
   #:point-2d-y
   #:point-2f
   #:point-2f-x
   #:point-2f-y
   #:point-3d
   #:point-3d-x
   #:point-3d-y
   #:point-3d-z
   #:point-3f
   #:point-3f-x
   #:point-3f-y
   #:point-3f-z
   #:point-3i
   #:point-3i-x
   #:point-3i-y
   #:point-3i-z
   #:promote
   #:rect
   #:rect-br
   #:rect-clone
   #:rect-size
   #:rect-tl
   #:reshape
   #:reshape-rows
   #:roi
   #:rotated-rect
   #:row-range
   #:scale
   #:scalar
   #:scalar-all
   #:size
   #:size-assgn-to
   #:size-from-point
   #:size2f
   #:sub

;; core - Dynamic Structures

;; core - Operations on Arrays

   #:mean
   #:sum

;; core - Drawing Functions

   #:bgr
   #:rgb

;; core - Utility and System Functions and Macros

;; imgproc - Image Filtering

   #:morphology-default-border-value
   #:get-structuring-element

;; imgproc - Geometric Image Transformations

;; imgproc - Miscellaneous Image Transformations
   
;; imgproc - Histograms

;; imgproc - Structural Analysis and Shape Descriptors

;; imgproc - Motion Analysis and Object Tracking

;; imgproc - Feature Detection

;; imgproc - Object Detection

;; highgui - User Interface


;; highgui - Reading and Writing Images and Video

   #:imread

;; highgui - Qt New Functions

;; calib3d - Camera Calibration and 3D Reconstruction


;; features2d - Feature Detection and Description

   #:brisk

;; features2d - Common Interfaces of Feature Detectors

;; features2d - Common Interfaces of Descriptor Extractors

;; features2d - Common Interfaces of Descriptor Matchers

   #:bf-matcher 

;;; features2d - Drawing Function of Keypoints and Matches

   #:draw-matches

;;; objdetect - Cascade Classification

   #:cascade-classifier
   #:cascade-classifier-load

;;; nonfree - Feature Detection and Description

   #:surf

;;; contrib - ColorMaps in OpenCV 

   #:apply-color-map



))

