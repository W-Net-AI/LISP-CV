;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :gc
  (:nicknames #:gc #:t)
  (:use #:cffi #:common-lisp #:swank #:trivial-garbage #:lisp-executable ;#:lisp-cv
) 
  (:export 

;; Macros

   #:alloc


;; Types

   #:bf-matcher
   #:brisk
   #:cascade-classifier
   #:descriptor-matcher
   #:dmatch
   #:feature-2d
   #:feature-detector
   #:key-point
   #:mat
   #:mat-expr
   #:mouse-callback
   #:point
   #:point2d
   #:point2f
   #:point3d
   #:point3f
   #:point3i
   #:rect
   #:rng
   #:rotated-rect
   #:rotated-rect-bounding-rect
   #:rotated-rect-center
   #:rotated-rect-size
   #:scalar
   #:size
   #:size2f
   #:*string
   #:surf
   #:svm
   #:svm-params
   #:term-criteria
   #:trackbar-callback
   #:vector-char
   #:vector-dmatch
   #:vector-double
   #:vector-float
   #:vector-int
   #:vector-key-point
   #:vector-mat
   #:vector-point
   #:vector-point2f
   #:vector-rect
   #:vector-uchar
   #:video-capture
   #:video-writer


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
   #:point2d
   #:point2d-x
   #:point2d-y
   #:point2f
   #:point2f-x
   #:point2f-y
   #:point3d
   #:point3d-x
   #:point3d-y
   #:point3d-z
   #:point3f
   #:point3f-x
   #:point3f-y
   #:point3f-z
   #:point3i
   #:point3i-x
   #:point3i-y
   #:point3i-z
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

