;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :gc
  (:nicknames #:gc #:t)
  (:use #:cffi #:common-lisp #:swank #:trivial-garbage #:lisp-executable ;#:lisp-cv
) 
  (:export 

;; Macros

   #:alloc


;; C-Interop

   #:c-string-to-string


;; Vectors

   #:vec-char
   #:c-arr-to-vec-char
   #:arr-to-vec-char
   #:vec-char-to-c-arr
   #:vec-char-length
   #:vec-char-to-lisp-list
   #:vec-char-to-lisp-vec
   #:vec-dmatch
   #:c-arr-to-vec-dmatch
   #:arr-to-vec-dmatch
   #:vec-dmatch-to-c-arr
   #:vec-dmatch-length
   #:vec-dmatch-to-lisp-list
   #:vec-dmatch-to-lisp-vec
   #:vec-double
   #:c-arr-to-vec-double
   #:arr-to-vec-double
   #:vec-double-to-c-arr
   #:vec-double-length
   #:vec-double-to-lisp-list
   #:vec-double-to-lisp-vec
   #:vec-float
   #:c-arr-to-vec-float
   #:arr-to-vec-float
   #:vec-float-to-c-arr
   #:vec-float-length
   #:vec-float-to-lisp-list
   #:vec-float-to-lisp-vec
   #:vec-int
   #:c-arr-to-vec-int
   #:arr-to-vec-int
   #:vec-int-to-c-arr
   #:vec-int-length
   #:vec-int-to-lisp-list
   #:vec-int-to-lisp-vec
   #:vec-key-point
   #:c-arr-to-vec-key-point
   #:arr-to-vec-key-point
   #:vec-key-point-to-c-arr
   #:vec-key-point-length
   #:vec-key-point-to-lisp-list
   #:vec-key-point-to-lisp-vec
   #:vec-mat
   #:c-arr-to-vec-mat
   #:arr-to-vec-mat
   #:vec-mat-to-c-arr
   #:vec-mat-length
   #:vec-mat-to-lisp-list
   #:vec-mat-to-lisp-vec
   #:vec-point
   #:c-arr-to-vec-point
   #:arr-to-vec-point
   #:vec-point-to-c-arr
   #:vec-point-length
   #:vec-point-to-lisp-list
   #:vec-point-to-lisp-vec
   #:vec-point-2f
   #:c-arr-to-vec-point-2f
   #:arr-to-vec-point-2f
   #:vec-point-2f-to-c-arr
   #:vec-point-2f-length
   #:vec-point-2f-to-lisp-list
   #:vec-point-2f-to-lisp-vec
   #:vec-rect
   #:c-arr-to-vec-rect
   #:arr-to-vec-rect
   #:vec-rect-to-c-arr
   #:vec-rect-length
   #:vec-rect-to-lisp-list
   #:vec-rect-to-lisp-vec
   #:vec-uchar
   #:c-arr-to-vec-uchar
   #:arr-to-vec-uchar
   #:vec-uchar-to-c-arr
   #:vec-uchar-length
   #:vec-uchar-to-lisp-list
   #:vec-uchar-to-lisp-vec
   #:vec-vec-4i
   #:c-arr-to-vec-vec-4i
   #:arr-to-vec-vec-4i
   #:vec-vec-4i-to-c-arr
   #:vec-vec-4i-length
   #:vec-vec-4i-to-lisp-list
   #:vec-vec-4i-to-lisp-vec



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
   #:range
   #:range-all
   #:rect
   #:rect-br
   #:rect-clone
   #:rect-size
   #:rect-tl
   #:reshape
   #:reshape-rows
   #:roi
   #:rotated-rect
   #:row
   #:row-range
   #:scale
   #:scalar
   #:scalar-all
   #:size
   #:size-assgn-to
   #:size-from-point
   #:size2f
   #:sub
   #:vec-4i

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

;;; ml - Normal Bayes Classifier

   #:normal-bayes-classifier
   #:normal-bayes-classifier-predict

;;; ml - K-Nearest Neighbors

   #:k-nearest
   #:k-nearest-find-nearest

;;; ml - Decision Trees

   #:d-tree
   #:d-tree-params

;;; ml - Neural Networks

   #:ann-mlp
   #:ann-mlp-train-params

;;; nonfree - Feature Detection and Description

   #:surf

;;; contrib - ColorMaps in OpenCV 

   #:apply-color-map



))

