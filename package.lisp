;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :lisp-cv
  (:nicknames #:lisp-cv #:lcv #:cv)
  (:use #:cffi #:common-lisp #:swank #:trivial-garbage #:lisp-executable) 
  (:export 


;; Default parameters.

   #:*camera-index* 
   #:*default-width*
   #:*default-height*
   #:*frames-per-second*
   #:*millis-per-frame*


;; utils - Utilities 

   #:->
   #:rename-package-nicknames
   #:full-pathname
   #:*lisp-cv-data-dir*
   #:*lisp-cv-src-dir*
   #:run-program
   #:println
   #:mklist
   #:partition
   #:cat
   #:dup
   #:do-while


;; Change default parameters

   #:def-params


;; DEFPARAMETER macro

   #:d


;; Live code editing

   #:continuable
   #:update-swank


;; Macros

   #:$
   #:?
   #:alloc
   #:d
   #:free
   #:print-mat
   #:size-of


;; C-Interop

   #:c-string-to-string


;; Extra OpenCV constants

   #:+max-dim+

;; C Constants

;; C Integer Limits

   #:+char-bit+
   #:+schar-min+
   #:+schar-max+
   #:+uchar-max+
   #:+char-min+
   #:+char-min-j+
   #:+char-max+
   #:+char-max-j+
   #:+mb-len-max+
   #:+shrt-min+
   #:+shrt-max+
   #:+ushrt-max+
   #:+int-min+
   #:+int-max+
   #:+uint-max+
   #:+long-min+
   #:+long-max+
   #:+ulong-max+
   #:+dbl-max+
   #:+flt-max+


;; DELETE

   #:del
   #:del-ann-mlp
   #:del-ann-mlp-train-params
   #:del-casc-class
   #:del-dmatch
   #:del-d-tree
   #:del-d-tree-params
   #:del-file-node
   #:del-file-storage
   #:del-hog-descriptor
   #:del-k-nearest
   #:del-key-point
   #:del-mat
   #:del-mat-expr
   #:del-normal-bayes-classifier
   #:del-point
   #:del-point-2d
   #:del-point-2f
   #:del-point-3d
   #:del-point-3f
   #:del-point-3i
   #:del-range
   #:del-rect
   #:del-rng
   #:del-rot-rect
   #:del-scalar
   #:del-size
   #:del-size-2f
   #:del-std-string
   #:del-term-crit
   #:del-vec-2b
   #:del-vec-3b
   #:del-vec-4b
   #:del-vec-2d
   #:del-vec-3d
   #:del-vec-4d
   #:del-vec-6d
   #:del-vec-2f
   #:del-vec-3f
   #:del-vec-4f 
   #:del-vec-6f
   #:del-vec-2i
   #:del-vec-3i
   #:del-vec-4i
   #:del-vec-6i
   #:del-vec-8i
   #:del-vec-2s
   #:del-vec-3s
   #:del-vec-4s
   #:del-vec-2w
   #:del-vec-3w
   #:del-vec-4w
   #:del-vector-char
   #:del-vector-double
   #:del-vector-dmatch
   #:del-vector-float
   #:del-vector-int
   #:del-vector-key-point
   #:del-vector-mat
   #:del-vector-point
   #:del-vector-point-2f
   #:del-vector-rect
   #:del-vector-uchar
   #:del-vector-vec-2b
   #:del-vector-vec-3b
   #:del-vector-vec-4b
   #:del-vector-vec-2d
   #:del-vector-vec-3d
   #:del-vector-vec-4d
   #:del-vector-vec-6d
   #:del-vector-vec-2f
   #:del-vector-vec-3f
   #:del-vector-vec-4f 
   #:del-vector-vec-6f
   #:del-vector-vec-2i
   #:del-vector-vec-3i
   #:del-vector-vec-4i
   #:del-vector-vec-6i
   #:del-vector-vec-8i
   #:del-vector-vec-2s
   #:del-vector-vec-3s
   #:del-vector-vec-4s
   #:del-vector-vec-2w
   #:del-vector-vec-3w
   #:del-vector-vec-4w
   #:del-vid-cap
   #:del-vid-writer


;; WITH-MACROS


   #:with-capture
   #:with-captured-camera
   #:with-captured-file
   #:with-cascade-classifier
   #:with-dmatch
   #:with-d-tree
   #:with-d-tree-params
   #:with-file-node
   #:with-file-storage
   #:with-hog-descriptor
   #:with-key-point
   #:with-k-nearest
   #:with-mat
   #:with-mat-expr
   #:with-named-window
   #:with-normal-bayes-classifier
   #:with-object
   #:with-point
   #:with-point-2d
   #:with-point-2f
   #:with-point-3d
   #:with-point-3f
   #:with-point-3i
   #:with-rect
   #:with-range
   #:with-rng
   #:with-rotated-rect
   #:with-scalar
   #:with-size
   #:with-size-2f
   #:with-term-criteria
   #:with-vec-2b
   #:with-vec-3b
   #:with-vec-4b
   #:with-vec-2d
   #:with-vec-3d
   #:with-vec-4d
   #:with-vec-6d
   #:with-vec-2f
   #:with-vec-3f
   #:with-vec-4f
   #:with-vec-6f
   #:with-vec-2i
   #:with-vec-3i
   #:with-vec-4i
   #:with-vec-6i
   #:with-vec-8i
   #:with-vec-2s
   #:with-vec-3s
   #:with-vec-4s
   #:with-vec-2w
   #:with-vec-3w
   #:with-vec-4w
   #:with-vector-char
   #:with-vector-double
   #:with-vector-dmatch
   #:with-vector-float
   #:with-vector-int
   #:with-vector-key-point
   #:with-vector-mat
   #:with-vector-point
   #:with-vector-point-2f
   #:with-vector-rect
   #:with-vector-uchar
   #:with-vector-vec-2b
   #:with-vector-vec-3b
   #:with-vector-vec-4b
   #:with-vector-vec-2d
   #:with-vector-vec-3d
   #:with-vector-vec-4d
   #:with-vector-vec-6d
   #:with-vector-vec-2f
   #:with-vector-vec-3f
   #:with-vector-vec-4f 
   #:with-vector-vec-6f
   #:with-vector-vec-2i
   #:with-vector-vec-3i
   #:with-vector-vec-4i
   #:with-vector-vec-6i
   #:with-vector-vec-8i
   #:with-vector-vec-2s
   #:with-vector-vec-3s
   #:with-vector-vec-4s
   #:with-vector-vec-2w
   #:with-vector-vec-3w
   #:with-vector-vec-4w
   #:with-video-capture
   #:with-video-writer


;; DEFCTYPE's

   #:ann-mlp
   #:ann-mlp-train-params
   #:bf-matcher
   #:brisk
   #:cascade-classifier
   #:dmatch
   #:d-tree
   #:d-tree-node
   #:d-tree-params
   #:file-node
   #:file-storage
   #:hog-descriptor
   #:key-point
   #:k-nearest
   #:make-mat
   #:mat
   #:mat-expr
   #:mouse-callback
   #:normal-bayes-classifier
   #:point
   #:point-2d
   #:point-2f
   #:point-3d
   #:point-3f
   #:point-3i
   #:range
   #:rect
   #:rng
   #:rotated-rect
   #:scalar
   #:size-2f
   #:*string
   #:svm
   #:svm-params
   #:surf
   #:term-criteria
   #:trackbar-callback
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
   #:vector-char
   #:vector-dmatch
   #:vector-double
   #:vector-float
   #:vector-int
   #:vector-key-point
   #:vector-mat
   #:vector-point
   #:vector-point-2f
   #:vector-rect
   #:vector-uchar
   #:vector-vec-2b
   #:vector-vec-3b
   #:vector-vec-4b
   #:vector-vec-2d
   #:vector-vec-3d
   #:vector-vec-4d
   #:vector-vec-6d
   #:vector-vec-2f
   #:vector-vec-3f
   #:vector-vec-4f 
   #:vector-vec-6f
   #:vector-vec-2i
   #:vector-vec-3i
   #:vector-vec-4i
   #:vector-vec-6i
   #:vector-vec-8i
   #:vector-vec-2s
   #:vector-vec-3s
   #:vector-vec-4s
   #:vector-vec-2w
   #:vector-vec-3w
   #:vector-vec-4w
   #:video-capture
   #:video-writer


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


;; DEFMETHODS

   #:angle
   #:bounding-rect
   #:center
   #:clone
   #:compute
   #:create
   #:detect
   #:dot
   #:height
   #:match
   #:*open
   #:release
   #:size
   #:width
   #:*write
   #:x
   #:y
   #:z

;; core - Basic Structures
   
   #:+8uc1+
   #:+8uc2+
   #:+8uc3+
   #:+8uc4+
   #:+8sc1+
   #:+8sc2+
   #:+8sc3+
   #:+8sc4+
   #:+16uc1+
   #:+16uc2+
   #:+16uc3+ 
   #:+16uc4+ 
   #:+16sc1+
   #:+16sc2+ 
   #:+16sc3+ 
   #:+16sc4+ 
   #:+32sc1+
   #:+32sc2+ 
   #:+32sc3+ 
   #:+32sc4+
   #:+32f+ 
   #:+32fc1+
   #:+32fc2+ 
   #:+32fc3+ 
   #:+32fc4+ 
   #:+64fc1+
   #:+64fc2+ 
   #:+64fc3+ 
   #:+64fc4+
   #:+termcrit-iter+
   #:+termcrit-number+
   #:+termcrit-eps+
   

   #:<<
   #:>>
   #:add
   #:adjust-roi
   #:area
   #:area-2f
   #:br
   #:channels
   #:class-id
   #:col-range
   #:cols
   #:convert-to
   #:cross
   #:copy-to
   #:data
   #:depth
   #:diag
   #:distance
   #:div
   #:dmatch
   #:dot-2d
   #:dot-2f
   #:dot-2i
   #:dot-3d
   #:dot-3f
   #:dot-3i
   #:elem-size
   #:elem-size1
   #:empty
   #:force
   #:height-2f
   #:img-idx
   #:inv
   #:is-continuous
   #:key-point
   #:locate-roi
   #:make-dmatch
   #:make-key-point
   #:make-mat
   #:make-mat-data
   #:make-mat-range
   #:make-mat-typed
   #:make-mat-value
   #:make-mat-zeros
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
   #:make-size-2f
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
   #:mat-expr-t
   #:mat-eye
   #:mat-ones
   #:mat-range
   #:mat-type
   #:mat-typed
   #:mat-value
   #:mat-zeros
   #:mul
   #:octave
   #:point
   #:point-x
   #:point-y
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
   #:ptr
   #:query-idx
   #:range
   #:range-end
   #:range-all
   #:range-start
   #:rect
   #:reshape
   #:reshape-rows
   #:response
   #:roi
   #:rotated-rect
   #:rotated-rect-bounding-rect
   #:rotated-rect-center
   #:rotated-rect-size
   #:row
   #:row-range
   #:rows
   #:scalar
   #:scalar-all
   #:scale
   #:size-assgn-to
   #:size-from-point
   #:size-2f
   #:step1
   #:width-2f
   #:*step
   #:sub
   #:tl
   #:term-criteria
   #:total
   #:train-idx
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

   #:+C+
   #:+cmp-eq+ 
   #:+cmp-gt+ 
   #:+cmp-ge+ 
   #:+cmp-lt+ 
   #:+cmp-le+ 
   #:+cmp-ne+
   #:+dft-inverse+
   #:+dft-scale+
   #:+dft-rows+
   #:+dft-complex-output+
   #:+dft-real-output+ 
   #:+dxt-forward+
   #:+dxt-inverse+
   #:+dxt-inv-scale+
   #:+dxt-inverse-scale+
   #:+dxt-rows+
   #:+dxt-mul-conj+
   #:+decomp-lu+ 
   #:+decomp-svd+ 
   #:+decomp-eig+ 
   #:+decomp-cholesky+ 
   #:+decomp-qr+ 
   #:+decomp-normal+
   #:+l1+
   #:+l2+
   #:+norm-inf+
   #:+norm-l1+
   #:+norm-l2+
   #:+norm-type-mask+
   #:+norm-relative+
   #:+norm-minmax+
   #:+minmax+
   #:+relative-c+
   #:+relative-l1+
   #:+relative-l2+
   #:+covar-scrambled+
   #:+covar-normal+ 
   #:+covar-use-avg+ 
   #:+covar-scale+ 
   #:+covar-rows+ 
   #:+covar-cols+ 

   #:*abs
   #:*exp
   #:*log
   #:*max
   #:*min
   #:*trace
   #:abs-diff
   #:add-weighted
   #:bitwise-and
   #:bitwise-not
   #:bitwise-or
   #:bitwise-xor
   #:calc-covar-matrix
   #:check-range
   #:complete-symm
   #:convert-scale-abs
   #:determinant
   #:divide
   #:flip
   #:in-range-s
   #:inv
   #:invert
   #:magnitude
   #:mahalanobis
   #:make-rng
   #:mean
   #:min-max-loc
   #:mul-transposed
   #:multiply
   #:norm
   #:normalize
   #:phase
   #:pow
   #:randu
   #:repeat
   #:rng
   #:scale-add
   #:subtract
   #:sum
   #:uniform

;; core - Drawing Functions


   #:+aa+
   #:+font-hershey-simplex+
   #:+font-hershey-plain+
   #:+font-hershey-duplex+
   #:+font-hershey-complex+
   #:+font-hershey-triplex+
   #:+font-hershey-complex-small+
   #:+font-hershey-script-simplex+
   #:+font-hershey-script-complex+
   #:+font-italic+ 

   #:bgr
   #:circle
   #:clip-line
   #:ellipse
   #:get-text
   #:line
   #:make-bgr
   #:make-rgb
   #:put-text
   #:rgb

;; core - XML/YAML Persistence

   #:+file-storage-read+
   #:+file-storage-write+
   #:+file-storage-append+
   #:+file-storage-memory+

   #:file-storage
   #:make-file-storage
   #:file-storage-open
   #:file-storage-release
   #:file-storage-write

;; core - Utility and System Functions and Macros

   #:+cpu-none+
   #:+cpu-mm+
   #:+cpu-sse+
   #:+cpu-sse2+
   #:+cpu-sse3+
   #:+cpu-ssse3+
   #:+cpu-sse4-1+
   #:+cpu-sse4-2+
   #:+cpu-popcnt+
   #:+cpu-avx+
   #:+hardware-max-feature+
   #:+max-feature+
   #:+stsok+ 
   #:+stsbacktrace+ 
   #:+stserror+
   #:+stsinternal+ 
   #:+stsnomem+
   #:+stsbadarg+
   #:+stsbadfunc+
   #:+stsnoconv+
   #:+stsautotrace+
   #:+headerisnull+
   #:+badimagesize+
   #:+badoffset+
   #:+baddataptr+
   #:+badstep+
   #:+badmodelorchseq+
   #:+badnumchannels+
   #:+badnumchannel1u+
   #:+baddepth+
   #:+badalphachannel+
   #:+badorder+
   #:+badorigin+
   #:+badalign+
   #:+badcallback+
   #:+badtilesize+
   #:+badcoi+
   #:+badroisize+
   #:+maskistiled+
   #:+stsnullptr+
   #:+stsveclengtherr+
   #:+stsfilterstructcontenterr+
   #:+stskernelstructcontenterr+
   #:+stsfilteroffseterr+
   #:+stsbadsize+
   #:+stsdivbyzero+
   #:+stsinplacenotsupported+
   #:+stsobjectnotfound+
   #:+stsunmatchedformats+
   #:+stsbadflag+
   #:+stsbadpoint+
   #:+stsbadmask+  
   #:+stsunmatchedsizes+
   #:+stsunsupportedformat+
   #:+stsoutofrange+ 
   #:+stsparseerror+ 
   #:+stsnotimplemented+
   #:+stsbadmembreshapelock+
   #:+stsassert+ 
   #:+gpunotsupported+ 
   #:+gpuapicallerror+ 
   #:+openglnotsupported+
   #:+openglapicallerror+

   #:check-hardware-support
   #:cube-root
   #:fast-atan2
   #:get-tick-count
   #:get-tick-frequency

;; imgproc - Image Filtering

   #:+ipl-border-constant+
   #:+ipl-border-replicate+
   #:+border-constant+
   #:+border-replicate+
   #:+border-reflect+ 
   #:+border-wrap+
   #:+border-reflect-101+
   #:+border-reflect101+
   #:+border-default+ 
   #:+border-transparent+
   #:+border-isolated+
   #:+cv-shape-rect+
   #:+cv-shape-cross+
   #:+cv-shape-ellipse+
   #:+morph-rect+
   #:+morph-cross+
   #:+morph-ellipse+
   #:+cv-shape-custom+
   #:+gaussian-5x5+
   #:+blur-no-scale+
   #:+blur+
   #:+gaussian+
   #:+median+
   #:+bilateral+
   #:+scharr+

   #:bilateral-filter
   #:blur
   #:copy-make-border
   #:erode
   #:dilate
   #:filter-2d
   #:gaussian-blur
   #:laplacian
   #:make-morphology-default-border-value
   #:median-blur
   #:morphology-default-border-value
   #:morphology-ex
   #:pyr-down
   #:pyr-up
   #:scharr
   #:sobel

;; imgproc - Geometric Image Transformations

   #:+warp-fill-outliers+
   #:+warp-inverse-map+
   #:+inter-nearest+ 
   #:+inter-linear+ 
   #:+inter-cubic+ 
   #:+inter-area+ 
   #:+inter-lanczos4+ 

   #:get-affine-transform
   #:get-perspective-transform
   #:get-rotation-matrix-2d
   #:invert-affine-transform
   #:remap
   #:resize
   #:warp-affine
   #:warp-perspective

;; imgproc - Miscellaneous Image Transformations

   #:+bgr2bgra+ 
   #:+rgb2rgba+ 
   #:+bgra2bgr+ 
   #:+rgba2rgb+ 
   #:+bgr2rgba+ 
   #:+rgb2bgra+ 
   #:+rgba2bgr+ 
   #:+bgra2rgb+ 
   #:+bgr2rgb+ 
   #:+rgb2bgr+ 
   #:+bgra2rgba+ 
   #:+rgba2bgra+ 
   #:+bgr2gray+ 
   #:+rgb2gray+ 
   #:+gray2bgr+ 
   #:+gray2rgb+ 
   #:+gray2bgra+ 
   #:+gray2rgba+ 
   #:+bgra2gray+ 
   #:+rgba2gray+ 
   #:+bgr2bgr565+
   #:+rgb2bgr565+
   #:+bgr5652bgr+
   #:+bgr5652rgb+
   #:+bgra2bgr565+
   #:+rgba2bgr565+
   #:+bgr5652bgra+
   #:+bgr5652rgba+
   #:+gray2bgr565+
   #:+bgr5652gray+
   #:+bgr2bgr555+
   #:+rgb2bgr555+
   #:+bgr5552bgr+
   #:+bgr5552rgb+
   #:+bgra2bgr555+
   #:+rgba2bgr555+
   #:+bgr5552bgra+
   #:+bgr5552rgba+
   #:+gray2bgr555+
   #:+bgr5552gray+
   #:+bgr2xyz+
   #:+rgb2xyz+
   #:+xyz2bgr+
   #:+xyz2rgb+
   #:+bgr2ycrcb+
   #:+rgb2ycrcb+
   #:+ycrcb2bgr+
   #:+ycrcb2rgb+
   #:+bgr2hsv+
   #:+rgb2hsv+
   #:+bgr2lab+
   #:+rgb2lab+
   #:+bayerbg2bgr+ 
   #:+bayergb2bgr+ 
   #:+bayerrg2bgr+ 
   #:+bayergr2bgr+
   #:+bayerbg2rgb+ 
   #:+bayergb2rgb+ 
   #:+bayerrg2rgb+ 
   #:+bayergr2rgb+
   #:+bgr2luv+
   #:+rgb2luv+
   #:+bgr2hls+
   #:+rgb2hls+ 
   #:+hsv2bgr+ 
   #:+hsv2rgb+ 
   #:+lab2bgr+ 
   #:+lab2rgb+ 
   #:+luv2bgr+
   #:+luv2rgb+
   #:+hls2bgr+ 
   #:+hls2rgb+
   #:+bayerbg2bgr-vng+
   #:+bayergb2bgr-vng+ 
   #:+bayerrg2bgr-vng+ 
   #:+bayergr2bgr-vng+ 
   #:+bayerbg2rgb-vng+
   #:+bayergb2rgb-vng+ 
   #:+bayerrg2rgb-vng+ 
   #:+bayergr2rgb-vng+
   #:+bgr2hsv-full+
   #:+rgb2hsv-full+ 
   #:+bgr2hls-full+ 
   #:+rgb2hls-full+
   #:+hsv2bgr-full+ 
   #:+hsv2rgb-full+ 
   #:+hls2bgr-full+
   #:+hls2rgb-full+ 
   #:+lbgr2lab+ 
   #:+lrgb2lab+ 
   #:+lbgr2luv+ 
   #:+lrgb2luv+ 
   #:+lab2lbgr+ 
   #:+lab2lrgb+ 
   #:+luv2lbgr+ 
   #:+luv2lrgb+ 
   #:+bgr2yuv+ 
   #:+rgb2yuv+ 
   #:+yuv2bgr+ 
   #:+yuv2rgb+ 
   #:+bayerbg2gray+ 
   #:+bayergb2gray+ 
   #:+bayerrg2gray+ 
   #:+bayergr2gray+ 
   #:+yuv2rgb-nv12+ 
   #:+yuv2bgr-nv12+ 
   #:+yuv2rgb-nv21+ 
   #:+yuv2bgr-nv21+ 
   #:+yuv420sp2rgb+ 
   #:+yuv420sp2bgr+ 
   #:+yuv2rgba-nv12+ 
   #:+yuv2bgra-nv12+
   #:+yuv2rgba-nv21+ 
   #:+yuv2bgra-nv21+ 
   #:+yuv420sp2rgba+ 
   #:+yuv420sp2bgra+ 
   #:+yuv2rgb-yv12+ 
   #:+yuv2bgr-yv12+ 
   #:+yuv2rgb-iyuv+ 
   #:+yuv2bgr-iyuv+ 
   #:+yuv2rgb-i420+ 
   #:+yuv2bgr-i420+ 
   #:+yuv420p2rgb+
   #:+yuv420p2bgr+ 
   #:+yuv2rgba-yv12+ 
   #:+yuv2bgra-yv12+ 
   #:+yuv2rgba-iyuv+ 
   #:+yuv2bgra-iyuv+ 
   #:+yuv2rgba-i420+ 
   #:+yuv2bgra-i420+ 
   #:+yuv420p2rgba+ 
   #:+yuv420p2bgra+ 
   #:+yuv2gray-420+ 
   #:+yuv2gray-nv21+ 
   #:+yuv2gray-nv12+ 
   #:+yuv2gray-yv12+ 
   #:+yuv2gray-iyuv+ 
   #:+yuv2gray-i420+ 
   #:+yuv420sp2gray+ 
   #:+yuv420p2gray+ 
   #:+yuv2rgb-uyvy+ 
   #:+yuv2bgr-uyvy+ 
  ;;+yuv2rgb-vyuy+ 
  ;;+yuv2bgr-vyuy+ 
   #:+yuv2rgb-y422+ 
   #:+yuv2bgr-y422+ 
   #:+yuv2rgb-uynv+ 
   #:+yuv2bgr-uynv+ 
   #:+yuv2rgba-uyvy+ 
   #:+yuv2bgra-uyvy+ 
  ;;+yuv2rgba-vyuy+
  ;;+yuv2bgra-vyuy+ 
   #:+yuv2rgba-y422+ 
   #:+yuv2bgra-y422+ 
   #:+yuv2rgba-uynv+ 
   #:+yuv2bgra-uynv+ 
   #:+yuv2rgb-yuy2+ 
   #:+yuv2bgr-yuy2+
   #:+yuv2rgb-yvyu+ 
   #:+yuv2bgr-yvyu+ 
   #:+yuv2rgb-yuyv+ 
   #:+yuv2bgr-yuyv+
   #:+yuv2rgb-yunv+ 
   #:+yuv2bgr-yunv+ 
   #:+yuv2rgba-yuy2+
   #:+yuv2bgra-yuy2+
   #:+yuv2rgba-yvyu+ 
   #:+yuv2bgra-yvyu+
   #:+yuv2rgba-yuyv+
   #:+yuv2bgra-yuyv+ 
   #:+yuv2rgba-yunv+ 
   #:+yuv2bgra-yunv+
   #:+yuv2gray-uyvy+
   #:+yuv2gray-yuy2+
  ;;+yuv2gray-vyuy+ 
   #:+yuv2gray-y422+
   #:+yuv2gray-uynv+ 
   #:+yuv2gray-yvyu+ 
   #:+yuv2gray-yuyv+ 
   #:+yuv2gray-yunv+ 
   #:+rgba2mrgba+
   #:+mrgba2rgba+ 
   #:+rgb2yuv-i420+ 
   #:+bgr2yuv-i420+
   #:+rgb2yuv-iyuv+
   #:+bgr2yuv-iyuv+
   #:+rgba2yuv-i420+
   #:+bgra2yuv-i420+
   #:+rgba2yuv-iyuv+ 
   #:+bgra2yuv-iyuv+
   #:+rgb2yuv-yv12+
   #:+bgr2yuv-yv12+ 
   #:+rgba2yuv-yv12+
   #:+bgra2yuv-yv12+
   #:+colorcvt-max+
   #:+thresh-binary+
   #:+thresh-binary-inv+
   #:+thresh-trunc+
   #:+thresh-tozero+
   #:+thresh-tozero-inv+
   #:+adaptitor-doubve-thresh-mean-c+
   #:+adaptive-thresh-gaussian-c+
   #:+dist-l1+ 
   #:+dist-l2+
   #:+dist-c+
   #:+dist-label-ccomp+
   #:+dist-label-pixel+
   #:+dist-mask-3+
   #:+dist-mask-5+
   #:+dist-mask-precise+ 

   #:adaptive-threshold 
   #:cvt-color
   #:distance-transform
   #:threshold
   
;; imgproc - Histograms

   #:equalize-hist

;; imgproc - Structural Analysis and Shape Descriptors

;; imgproc - Motion Analysis and Object Tracking

   #:phase-correlate

;; imgproc - Feature Detection

   #:+lsd-refine-none+
   #:+lsd-refine-std+
   #:+lsd-refine-adv+

   #:canny

;; imgproc - Object Detection

   #:+tm-sqdiff+   
   #:+tm-sqdiff-normed+
   #:+tm-ccorr+ 
   #:+tm-ccorr-normed+
   #:+tm-ccoeff+
   #:+tm-ccoeff-normed+

;; highgui - User Interface

   #:+cvtimg-flip+
   #:+cvtimage-swap-rb+
   #:+event-mousemove+ 
   #:+event-lbuttondown+ 
   #:+event-rbuttondown+ 
   #:+event-mbuttondown+ 
   #:+event-lbuttonup+ 
   #:+event-rbuttonup+ 
   #:+event-mbuttonup+ 
   #:+event-lbuttondblclk+ 
   #:+event-rbuttondblclk+ 
   #:+event-mbuttondblclk+ 
   #:+event-flag-lbutton+ 
   #:+event-flag-rbutton+ 
   #:+event-flag-mbutton+ 
   #:+event-flag-ctrlkey+ 
   #:+event-flag-shiftkey+ 
   #:+event-flag-altkey+ 
   #:+window-normal+
   #:+window-autosize+

   #:convert-image
   #:create-trackbar
   #:destroy-all-windows
   #:destroy-window
   #:get-trackbar-pos
   #:imshow
   #:move-window
   #:set-mouse-callback
   #:set-trackbar-pos
   #:named-window
   #:start-window-thread
   #:wait-key


;; highgui - Reading and Writing Images and Video

   #:+cap-any+
   #:+cap-prop-pos-msec+
   #:+cap-prop-pos-frames+
   #:+cap-prop-pos-avi-ratio+
   #:+cap-prop-frame-width+
   #:+cap-prop-frame-height+
   #:+cap-prop-fps+
   #:+cap-prop-fourcc+
   #:+cap-prop-frame-count+
   #:+cap-prop-format+
   #:+cap-prop-mode+
   #:+cap-prop-brightness+
   #:+cap-prop-contrast+
   #:+cap-prop-saturation+
   #:+cap-prop-hue+
   #:+cap-prop-gain+
   #:+cap-prop-exposure+
   #:+cap-prop-convert-rgb+
   #:+cap-prop-white-balance+
   #:+cap-prop-rectification+
   #:+imread_unchanged+
   #:+imread_grayscale+
   #:+imread_color+
   #:+imread_anydepth+
   #:+imread_anycolor+
   #:+load-image-unchanged+
   #:+load-image-grayscale+
   #:+load-image-color+
   #:+load-image-anydepth+
   #:+load-image-anycolor+

   #:cap-get
   #:cap-is-open
   #:cap-read
   #:cap-release
   #:cap-set
 

   #:imread
   #:imwrite
   #:make-video-capture
   #:make-video-writer
   #:video-capture
   #:video-writer
   #:video-writer-is-open
   #:video-writer-write


;; highgui - Qt New Functions

   #:+window-fullscreen+ 
   #:+window-freeratio+ 
   #:+window-keepratio+
   #:+wnd-prop-fullscreen+
   #:+wnd-prop-autosize+ 
   #:+wnd-prop-aspectratio+

   #:display-overlay 
   #:get-window-property
   #:set-window-property


;; calib3d - Camera Calibration and 3D Reconstruction


   #:+calib-cb-adaptive-thresh+ 
   #:+calib-cb-normalize-image+ 
   #:+calib-cb-filter-quads+ 
   #:+calib-cb-fast-check+ 


;; features2d - Feature Detection and Description

   #:brisk
   #:make-brisk
   #:feature-2d-create

;; features2d - Common Interfaces of Feature Detectors

   #:feature-detector-create
   #:feature-detector-detect

;; features2d - Common Interfaces of Descriptor Extractors

   #:descriptor-extractor-compute
   #:descriptor-extractor-create

;; features2d - Common Interfaces of Descriptor Matchers

   #:bf-matcher
   #:make-bf-matcher
   #:descriptor-matcher-create
   #:descriptor-matcher-match

;;; features2d - Drawing Function of Keypoints and Matches

   #:+default+
   #:+draw-over-outimg+
   #:+not-draw-single-points+
   #:+draw-rich-keypoints+

   #:draw-matches

;;; objdetect

   #:+hog-descriptor-l-2-hys+
   #:+hog-descriptor-default-nlevels+

;;; objdetect - Cascade Classification

   #:+cascade-do-canny-pruning+
   #:+cascade-scale-image+
   #:+cascade-find-biggest-object+
   #:+cascade-do-rough-search+


   #:cascade-classifier
   #:cascade-classifier-load
   #:detect-multi-scale
   #:make-cascade-classifier

;;; ml

   #:+var-numerical+
   #:+var-ordered+
   #:+var-categorical+
   #:+col-sample+
   #:+row-sample+

;;; ml - Normal Bayes Classifier

   #:make-normal-bayes-classifier
   #:normal-bayes-classifier
   #:normal-bayes-classifier-predict

;;; ml - K-Nearest Neighbors

   #:k-nearest
   #:k-nearest-find-nearest
   #:make-k-nearest

;;; ml - Decision Trees

   #:d-tree
   #:d-tree-params
   #:d-tree-predict
   #:d-tree-train
   #:make-d-tree
   #:make-d-tree-params

;;; ml - Neural Networks

   #:+ann-mlp-identity+
   #:+ann-mlp-sigmoid-sym+
   #:+ann-mlp-gaussian+  
   #:+ann-mlp-train-params-backprop+ 
   #:+ann-mlp-train-params-rprop+
   #:+update-weights+ 
   #:+no-input-scale+ 
   #:+no-output-scale+

   #:ann-mlp
   #:ann-mlp-create
   #:ann-mlp-predict
   #:ann-mlp-train
   #:ann-mlp-train-params
   #:make-ann-mlp
   #:make-ann-mlp-train-params


;;; photo - Inpainting

   #:+inpaint-ns+
   #:+inpaint-telea+

   #:in-paint

;;; photo - Decolorization

   #:decolor

;;; photo - Seamless Cloning

   #:+normal-clone+
   #:+mixed-clone+
   #:+feature-exchange+

   #:color-change
   #:illumination-change
   #:seamless-clone
   #:texture-flattening

;;; photo - Non-Photorealistic Rendering

   #:+recurs-filter+
   #:+normconv-filter+
   #:+monochrome-transfer+ 

   #:detail-enhance
   #:edge-preserving-filter
   #:pencil-sketch
   #:stylization

;;; nonfree - Feature Detection and Description

   #:make-surf
   #:surf

;;; contrib - ColorMaps in OpenCV

   #:+colormap-autumn+ 
   #:+colormap-bone+ 
   #:+colormap-jet+ 
   #:+colormap-winter+
   #:+colormap-rainbow+ 
   #:+colormap-ocean+ 
   #:+colormap-summer+ 
   #:+colormap-spring+ 
   #:+colormap-cool+ 
   #:+colormap-hsv+ 
   #:+colormap-pink+ 
   #:+colormap-hot+ 

   #:apply-color-map
))
