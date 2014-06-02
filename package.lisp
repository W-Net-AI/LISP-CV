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
   #:del-feature-2d
   #:del-hog-descriptor
   #:del-k-nearest
   #:del-kp
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
   #:del-size2f
   #:del-std-string
   #:del-term-crit
   #:del-vec-4i
   #:del-vec-char
   #:del-vec-dbl
   #:del-vec-dm
   #:del-vec-flt
   #:del-vec-int
   #:del-vec-kp
   #:del-vec-mat
   #:del-vec-point
   #:del-vec-point-2f
   #:del-vec-rect
   #:del-vec-uchar
   #:del-vec-vec-4i
   #:del-vid-cap
   #:del-vid-writer35


;; WITH-MACROS

   #:with-cascade-classifier
   #:with-dmatch
   #:with-d-tree
   #:with-d-tree-params
   #:with-feature-2d
   #:with-hog-descriptor
   #:with-keypoint
   #:with-k-nearest
   #:with-mat
   #:with-mat-expr
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
   #:with-size2f
   #:with-term-criteria
   #:with-vec-4i
   #:with-vec-char
   #:with-vec-dbl
   #:with-vec-dmatch
   #:with-vec-flt
   #:with-vec-int
   #:with-vec-key-point
   #:with-vec-mat
   #:with-vec-point
   #:with-vec-point-2f
   #:with-vec-rect
   #:with-vec-uchar
   #:with-vec-vec-4i
   #:with-video-capture
   #:with-video-writer


;; DEFCTYPE's

   #:ann-mlp
   #:ann-mlp-train-params
   #:cascade-classifier
   #:dmatch
   #:d-tree
   #:d-tree-node
   #:d-tree-params
   #:feature-2d
   #:hog-descriptor
   #:key-point
   #:k-nearest
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
   #:size
   #:size2f
   #:*string
   #:svm
   #:svm-params
   #:term-criteria
   #:trackbar-callback
   #:vec-4i
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
   #:vector-vec-4i
   #:video-capture
   #:video-writer


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
   #:area2f
   #:rect-br
   #:channels
   #:clone
   #:col-range
   #:cols
   #:convert-to
   #:cross
   #:copy-to
   #:data
   #:depth
   #:diag
   #:div
   #:dmatch
   #:dot
   #:dot2d
   #:dot2f
   #:dot3d
   #:dot3f
   #:dot3i
   #:elem-size
   #:elem-size1
   #:empty
   #:force
   #:height
   #:inv
   #:height2f
   #:is-continuous
   #:keypoint
   #:locate-roi
   #:mat
   #:mat-expr-t
   #:mat-eye
   #:mat-ones
   #:mat-type
   #:mat-zeros
   #:mul
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
   #:range
   #:range-end
   #:range-all
   #:range-start
   #:rect
   #:rect-clone
   #:rect-height
   #:rect-size
   #:rect-width
   #:rect-x
   #:rect-y
   #:reshape
   #:reshape-rows
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
   #:size
   #:size-assgn-to
   #:size-from-point
   #:size2f
   #:step1
   #:width
   #:width2f
   #:*step
   #:sub
   #:rect-tl
   #:total
   #:vec-4i

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
   #:abs-diff
   #:add-weighted
   #:bitwise-and
   #:bitwise-not
   #:bitwise-or
   #:bitwise-xor
   #:calc-covar-matrix
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
   #:mean
   #:min-max-loc
   #:multiply
   #:norm
   #:normalize
   #:phase
   #:pow
   #:randu
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


   #:circle
   #:ellipse
   #:get-text
   #:line
   #:put-text

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
   #:morphology-default-border-value

   #:bilateral-filter
   #:blur
   #:copy-make-border
   #:erode
   #:dilate
   #:filter-2d
   #:gaussian-blur
   #:laplacian
   #:median-blur
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

   #:remap
   #:resize

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
   #:wait-key
   #:with-named-window


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
   #:video-capture
   #:video-writer
   #:video-writer-is-open
   #:video-writer-write
   #:with-capture
   #:with-captured-camera
   #:with-captured-file

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

;; features2d - Common Interfaces of Feature Detectors

   #:feature-detector-create
   #:feature-detector-detect

;; features2d - Common Interfaces of Descriptor Extractors

   #:feat-2d-compute


;; features2d - Common Interfaces of Descriptor Matchers

   #:bf-matcher
   #:descrip-matcher-create 
   #:descrip-matcher-match

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


;;; ml

   #:+var-numerical+
   #:+var-ordered+
   #:+var-categorical+
   #:+col-sample+
   #:+row-sample+

;;; ml - Normal Bayes Classifier

   #:normal-bayes-classifier
   #:normal-bayes-classifier-predict

;;; ml - K-Nearest Neighbors

   #:k-nearest
   #:k-nearest-find-nearest

;;; ml - Decision Trees

   #:d-tree
   #:d-tree-params
   #:d-tree-predict
   #:d-tree-train

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


