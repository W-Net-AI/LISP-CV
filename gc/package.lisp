;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :gc
  (:nicknames #:gc #:t)
  (:use #:cffi #:common-lisp #:swank #:trivial-garbage #:lisp-executable) 
  (:shadow #:abs)
  (:export 

;; Macros

   #:alloc


;; Interop

   #:c-string-to-string
   #:c-string


;; CFFI

    #:alloc


;; Vectors

   #:vector-char
   #:make-vector-char
   #:c-arr-to-vec-char
   #:arr-to-vec-char
   #:vec-char-to-c-arr
   #:vec-char-length
   #:vec-char-to-lisp-list
   #:vec-char-to-lisp-vec
   #:make-vector-dmatch
   #:vector-dmatch
   #:c-arr-to-vec-dmatch
   #:arr-to-vec-dmatch
   #:vec-dmatch-to-c-arr
   #:vec-dmatch-length
   #:vec-dmatch-to-lisp-list
   #:vec-dmatch-to-lisp-vec
   #:make-vector-double
   #:vector-double
   #:c-arr-to-vec-double
   #:arr-to-vec-double
   #:vec-double-to-c-arr
   #:vec-double-length
   #:vec-double-to-lisp-list
   #:vec-double-to-lisp-vec
   #:make-vector-float
   #:vector-float
   #:c-arr-to-vec-float
   #:arr-to-vec-float
   #:vec-float-to-c-arr
   #:vec-float-length
   #:vec-float-to-lisp-list
   #:vec-float-to-lisp-vec
   #:make-vector-int
   #:vector-int
   #:c-arr-to-vec-int
   #:arr-to-vec-int
   #:vec-int-to-c-arr
   #:vec-int-length
   #:vec-int-to-lisp-list
   #:vec-int-to-lisp-vec
   #:make-vector-key-point
   #:vector-key-point
   #:c-arr-to-vec-key-point
   #:arr-to-vec-key-point
   #:vec-key-point-to-c-arr
   #:vec-key-point-length
   #:vec-key-point-to-lisp-list
   #:vec-key-point-to-lisp-vec
   #:make-vector-mat
   #:vector-mat
   #:c-arr-to-vec-mat
   #:arr-to-vec-mat
   #:vec-mat-to-c-arr
   #:vec-mat-length
   #:vec-mat-to-lisp-list
   #:vec-mat-to-lisp-vec
   #:make-vector-point
   #:vector-point
   #:c-arr-to-vec-point
   #:arr-to-vec-point
   #:vec-point-to-c-arr
   #:vec-point-length
   #:vec-point-to-lisp-list
   #:vec-point-to-lisp-vec
   #:make-vector-point-2f
   #:vector-point-2f
   #:c-arr-to-vec-point-2f
   #:arr-to-vec-point-2f
   #:vec-point-2f-to-c-arr
   #:vec-point-2f-length
   #:vec-point-2f-to-lisp-list
   #:vec-point-2f-to-lisp-vec
   #:make-vector-rect
   #:vector-rect
   #:c-arr-to-vec-rect
   #:arr-to-vec-rect
   #:vec-rect-to-c-arr
   #:vec-rect-length
   #:vec-rect-to-lisp-list
   #:vec-rect-to-lisp-vec
   #:make-vector-uchar
   #:vector-uchar
   #:c-arr-to-vec-uchar
   #:arr-to-vec-uchar
   #:vec-uchar-to-c-arr
   #:vec-uchar-length
   #:vec-uchar-to-lisp-list
   #:vec-uchar-to-lisp-vec
   #:make-vector-vec-2b
   #:vector-vec-2b
   #:c-arr-to-vec-vec-2b
   #:arr-to-vec-vec-2b
   #:vec-vec-2b-to-c-arr
   #:vec-vec-2b-length
   #:vec-vec-2b-to-lisp-list
   #:vec-vec-2b-to-lisp-vec
   #:make-vector-vec-3b
   #:vector-vec-3b
   #:c-arr-to-vec-vec-3b
   #:arr-to-vec-vec-3b
   #:vec-vec-3b-to-c-arr
   #:vec-vec-3b-length
   #:vec-vec-3b-to-lisp-list
   #:vec-vec-3b-to-lisp-vec
   #:make-vector-vec-4b
   #:vector-vec-4b
   #:c-arr-to-vec-vec-4b
   #:arr-to-vec-vec-4b
   #:vec-vec-4b-to-c-arr
   #:vec-vec-4b-length
   #:vec-vec-4b-to-lisp-list
   #:vec-vec-4b-to-lisp-vec
   #:make-vector-vec-2d
   #:vector-vec-2d
   #:c-arr-to-vec-vec-2d
   #:arr-to-vec-vec-2d
   #:vec-vec-2d-to-c-arr
   #:vec-vec-2d-length
   #:vec-vec-2d-to-lisp-list
   #:vec-vec-2d-to-lisp-vec
   #:make-vector-vec-3d
   #:vector-vec-3d
   #:c-arr-to-vec-vec-3d
   #:arr-to-vec-vec-3d
   #:vec-vec-3d-to-c-arr
   #:vec-vec-3d-length
   #:vec-vec-3d-to-lisp-list
   #:vec-vec-3d-to-lisp-vec
   #:make-vector-vec-4d
   #:vector-vec-4d
   #:c-arr-to-vec-vec-4d
   #:arr-to-vec-vec-4d
   #:vec-vec-4d-to-c-arr
   #:vec-vec-4d-length
   #:vec-vec-4d-to-lisp-list
   #:vec-vec-4d-to-lisp-vec
   #:make-vector-vec-6d
   #:vector-vec-6d
   #:c-arr-to-vec-vec-6d
   #:arr-to-vec-vec-6d
   #:vec-vec-6d-to-c-arr
   #:vec-vec-6d-length
   #:vec-vec-6d-to-lisp-list
   #:vec-vec-6d-to-lisp-vec
   #:make-vector-vec-2f
   #:vector-vec-2f
   #:c-arr-to-vec-vec-2f
   #:arr-to-vec-vec-2f
   #:vec-vec-2f-to-c-arr
   #:vec-vec-2f-length
   #:vec-vec-2f-to-lisp-list
   #:vec-vec-2f-to-lisp-vec
   #:make-vector-vec-3f
   #:vector-vec-3f
   #:c-arr-to-vec-vec-3f
   #:arr-to-vec-vec-3f
   #:vec-vec-3f-to-c-arr
   #:vec-vec-3f-length
   #:vec-vec-3f-to-lisp-list
   #:vec-vec-3f-to-lisp-vec
   #:make-vector-vec-4f
   #:vector-vec-4f
   #:c-arr-to-vec-vec-4f
   #:arr-to-vec-vec-4f
   #:vec-vec-4f-to-c-arr
   #:vec-vec-4f-length
   #:vec-vec-4f-to-lisp-list
   #:vec-vec-4f-to-lisp-vec
   #:make-vector-vec-6f
   #:vector-vec-6f
   #:c-arr-to-vec-vec-6f
   #:arr-to-vec-vec-6f
   #:vec-vec-6f-to-c-arr
   #:vec-vec-6f-length
   #:vec-vec-6f-to-lisp-list
   #:vec-vec-6f-to-lisp-vec
   #:make-vector-vec-2i
   #:vector-vec-2i
   #:c-arr-to-vec-vec-2i
   #:arr-to-vec-vec-2i
   #:vec-vec-2i-to-c-arr
   #:vec-vec-2i-length
   #:vec-vec-2i-to-lisp-list
   #:vec-vec-2i-to-lisp-vec
   #:make-vector-vec-3i
   #:vector-vec-3i
   #:c-arr-to-vec-vec-3i
   #:arr-to-vec-vec-3i
   #:vec-vec-3i-to-c-arr
   #:vec-vec-3i-length
   #:vec-vec-3i-to-lisp-list
   #:vec-vec-3i-to-lisp-vec
   #:make-vector-vec-4i
   #:vector-vec-4i
   #:c-arr-to-vec-vec-4i
   #:arr-to-vec-vec-4i
   #:vec-vec-4i-to-c-arr
   #:vec-vec-4i-length
   #:vec-vec-4i-to-lisp-list
   #:vec-vec-4i-to-lisp-vec
   #:make-vector-vec-6i
   #:vector-vec-6i
   #:c-arr-to-vec-vec-6i
   #:arr-to-vec-vec-6i
   #:vec-vec-6i-to-c-arr
   #:vec-vec-6i-length
   #:vec-vec-6i-to-lisp-list
   #:vec-vec-6i-to-lisp-vec
   #:make-vector-vec-8i
   #:vector-vec-8i
   #:c-arr-to-vec-vec-8i
   #:arr-to-vec-vec-8i
   #:vec-vec-8i-to-c-arr
   #:vec-vec-8i-length
   #:vec-vec-8i-to-lisp-list
   #:vec-vec-8i-to-lisp-vec
   #:make-vector-vec-2s
   #:vector-vec-2s
   #:c-arr-to-vec-vec-2s
   #:arr-to-vec-vec-2s
   #:vec-vec-2s-to-c-arr
   #:vec-vec-2s-length
   #:vec-vec-2s-to-lisp-list
   #:vec-vec-2s-to-lisp-vec
   #:make-vector-vec-3s
   #:vector-vec-3s
   #:c-arr-to-vec-vec-3s
   #:arr-to-vec-vec-3s
   #:vec-vec-3s-to-c-arr
   #:vec-vec-3s-length
   #:vec-vec-3s-to-lisp-list
   #:vec-vec-3s-to-lisp-vec
   #:make-vector-vec-4s
   #:vector-vec-4s
   #:c-arr-to-vec-vec-4s
   #:arr-to-vec-vec-4s
   #:vec-vec-4s-to-c-arr
   #:vec-vec-4s-length
   #:vec-vec-4s-to-lisp-list
   #:vec-vec-4s-to-lisp-vec
   #:make-vector-vec-2w
   #:vector-vec-2w
   #:c-arr-to-vec-vec-2w
   #:arr-to-vec-vec-2w
   #:vec-vec-2w-to-c-arr
   #:vec-vec-2w-length
   #:vec-vec-2w-to-lisp-list
   #:vec-vec-2w-to-lisp-vec
   #:make-vector-vec-3w
   #:vector-vec-3w
   #:c-arr-to-vec-vec-3w
   #:arr-to-vec-vec-3w
   #:vec-vec-3w-to-c-arr
   #:vec-vec-3w-length
   #:vec-vec-3w-to-lisp-list
   #:vec-vec-3w-to-lisp-vec
   #:make-vector-vec-4w
   #:vector-vec-4w
   #:c-arr-to-vec-vec-4w
   #:arr-to-vec-vec-4w
   #:vec-vec-4w-to-c-arr
   #:vec-vec-4w-length
   #:vec-vec-4w-to-lisp-list
   #:vec-vec-4w-to-lisp-vec

;;; Functions and methods used to 
;;; re-import shadowed symbols.

   #:abs

;; core - Basic Structures

   #:<<
   #:>>
   #:add
   #:adjust-roi
   #:br
   #:clone
   #:col-range
   #:diag
   #:div
   #:dmatch
   #:force
   #:inv
   #:key-point
   #:make-dmatch
   #:make-key-point
   #:make-mat
   #:make-point
   #:make-point-2d
   #:make-point-2f
   #:make-point-3d
   #:make-point-3f
   #:make-point-3i
   #:make-range
   #:make-range-all
   #:make-rect
   #:make-rotated-rect
   #:make-scalar
   #:make-scalar-all
   #:make-size
   #:make-term-criteria
   #:make-vec-2b
   #:make-vec-3b
   #:make-vec-4b
   #:make-vec-2d
   #:make-vec-3d
   #:make-vec-4d
   #:make-vec-6d
   #:make-vec-2f
   #:make-vec-3f
   #:make-vec-4f
   #:make-vec-6f
   #:make-vec-2i
   #:make-vec-3i
   #:make-vec-4i
   #:make-vec-6i
   #:make-vec-8i
   #:make-vec-2s
   #:make-vec-3s
   #:make-vec-4s
   #:make-vec-2w
   #:make-vec-3w
   #:make-vec-4w
   #:mat
   #:mat-data
   #:mat-dot
   #:mat-element
   #:mat-expr-t
   #:mat-eye
   #:mat-ones
   #:mat-range
   #:mat-size
   #:mat-type
   #:mat-typed
   #:mat-value
   #:mat-zeros
   #:mul
   #:point
   #:point-2d
   #:point-2f
   #:point-3d
   #:point-3f
   #:point-3i
   #:promote
   #:range
   #:range-all
   #:rect
   #:rect-size
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
   #:size-from-point
   #:sub
   #:term-criteria
   #:tl
   #:trace*
   #:vec-2b
   #:vec-3b
   #:vec-4b
   #:vec-2d
   #:vec-3d
   #:vec-4d
   #:vec-6d
   #:vec-2f
   #:vec-3f
   #:vec-4f
   #:vec-6f
   #:vec-2i
   #:vec-3i
   #:vec-4i
   #:vec-6i
   #:vec-8i
   #:vec-2s
   #:vec-3s
   #:vec-4s
   #:vec-2w
   #:vec-3w
   #:vec-4w


;; core - Dynamic Structures

;; core - Operations on Arrays

   #:inv
   #:mean
   #:make-rng
   #:rng
   #:sum

;; core - Drawing Functions

   #:bgr
   #:make-bgr
   #:make-rgb
   #:rgb

;; core - XML/YAML Persistence

   #:file-storage
   #:make-file-storage

;; core - Utility and System Functions and Macros

;; imgproc - Image Filtering

   #:make-morphology-default-border-value
   #:morphology-default-border-value
   #:get-structuring-element

;; imgproc - Geometric Image Transformations

   #:get-affine-transform
   #:get-perspective-transform
   #:get-rotation-matrix-2d

;; imgproc - Miscellaneous Image Transformations
   
;; imgproc - Histograms

;; imgproc - Structural Analysis and Shape Descriptors

;; imgproc - Motion Analysis and Object Tracking

   #:phase-correlate

;; imgproc - Feature Detection

;; imgproc - Object Detection

;; highgui - User Interface


;; highgui - Reading and Writing Images and Video

   #:imdecode
   #:imread
   #:make-video-capture
   #:make-video-writer
   #:video-capture
   #:video-writer

;; highgui - Qt New Functions

;; calib3d - Camera Calibration and 3D Reconstruction


;; features2d - Feature Detection and Description

   #:brisk
   #:make-brisk

;; features2d - Common Interfaces of Feature Detectors

;; features2d - Common Interfaces of Descriptor Extractors

;; features2d - Common Interfaces of Descriptor Matchers

   #:bf-matcher 
   #:make-bf-matcher 

;;; features2d - Drawing Function of Keypoints and Matches

   #:draw-matches

;;; objdetect - Cascade Classification

   #:cascade-classifier
   #:cascade-classifier-load
   #:make-cascade-classifier

;;; ml - LISP-CV specific

   #:make-training-matrix

;;; ml - Normal Bayes Classifier

   #:make-normal-bayes-classifier
   #:normal-bayes-classifier
   #:normal-bayes-classifier-predict

;;; ml - K-Nearest Neighbors

   #:k-nearest
   #:k-nearest-find-nearest
   #:make-k-nearest

;;; ml - Support Vector Machines 

   #:make-svm
   #:make-svm-params
   #:svm
   #:svm-params

;;; ml - Decision Trees

   #:d-tree
   #:d-tree-params
   #:make-d-tree
   #:make-d-tree-params

;;; ml - Neural Networks

   #:ann-mlp
   #:ann-mlp-train-params
   #:make-ann-mlp
   #:make-ann-mlp-train-params

;;; nonfree - Feature Detection and Description

   #:make-surf   
   #:surf

;;; contrib - ColorMaps in OpenCV 

   #:apply-color-map



))

