;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :lisp-cv
  (:nicknames #:lisp-cv #:lcv)
  (:use #:common-lisp #:cffi)
  (:export 

;; Default parameters

   #:*camera-index* 
   #:*default-width*
   #:*default-height*
   #:*frames-per-second*
   #:*millis-per-frame*


;; Change default parameters

   #:def-params

;; DEFPARAMETER macro

   #:d

;; Live code editing

   #:continuable
   #:update-swank

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
   #:+int-max+
   #:+uint-max+
   #:+long-max+
   #:+ulong-max+



;; DEFCTYPE's

   #:bf-matcher
   #:brisk
   #:descriptor-matcher
   #:feature-2d
   #:feature-detector
   #:mat
   #:mat-expr
   #:point
   #:point2d
   #:point2f
   #:point3d
   #:point3f
   #:point3i
   #:rect
   #:scalar
   #:size
   #:string*
   #:svm
   #:svm-params
   #:vec2b
   #:vec2d
   #:vec2f
   #:vec2i
   #:vec2s
   #:vec3b
   #:vec3d
   #:vec3f
   #:vec3i
   #:vec3s
   #:vec4b
   #:vec4d
   #:vec4f
   #:vec4i
   #:vec4s
   #:vector-char
   #:vector-dmatch
   #:vector-float
   #:vector-int
   #:vector-point
   #:vector-point2f
   #:vector-keypoint





;; core - Basic Structures


;; Mat*
   
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

   #:<<
   #:>>
   #:area
   #:area2f
   #:add
   #:br
   #:channels
   #:clone
   #:cols
   #:convert-scale-abs
   #:convert-to
   #:copy-to
   #:data
   #:del-mat
   #:del-mat-expr
   #:div
   #:dot
   #:dot2d
   #:dot2f
   #:dot3d
   #:dot3f
   #:dot3i
   #:empty
   #:force
   #:height
   #:height2f
   #:is-continuous
   #:mat-diag
   #:mat-expr-t
   #:mat-expr-s
   #:mat-eye
   #:mat-ones
   #:mat-value
   #:mat-type
   #:mat-typed
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
   #:ptr
   #:randu
   #:rect
   #:rect-init
   #:reshape
   #:roi
   #:rows
   #:scalar
   #:scalar-all
   #:size
   #:size2f
   #:mat-size
   #:width
   #:width2f
   #:step*
   #:sub
   #:sz
   #:tl
   #:total
   #:x
   #:y

   #:ipl-image
   #:+ipl-depth-1u+
   #:+ipl-depth-8u+
   #:+ipl-depth-16u+
   #:+ipl-depth-32f+
   #:+ipl-depth-64f+
   #:+ipl-depth-8s+
   #:+ipl-depth-16s+
   #:+ipl-depth-32s+
   #:+ipl-data-order-pixel+
   #:+ipl-data-order-plane+
   #:+ipl-origin-tl+
   #:+ipl-origin-bl+
   #:+termcrit-iter+
   #:+termcrit-number+
   #:+termcrit-eps+
   
;; core - Dynamic Structures

;; core - Operations on Arrays

   #:abs-diff

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
 
   #:flip
   #:in-range-s
   #:invert

   #:+l1+
   #:+l2+

   #:mean
   #:multiply
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

;; core - Drawing Functions
   #:+aa+
   #:circle

   #:+font-hershey-simplex+
   #:+font-hershey-plain+
   #:+font-hershey-duplex+
   #:+font-hershey-complex+
   #:+font-hershey-triplex+
   #:+font-hershey-complex-small+
   #:+font-hershey-script-simplex+
   #:+font-hershey-script-complex+
   #:+font-italic+ 

   #:line
   #:ellipse
   #:get-text
   #:put-text

;; core - Utility and System Functions and Macros

   #:get-tick-count
   #:get-tick-frequency
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
   #:+stsbadmemblock+
   #:+stsassert+ 
   #:+gpunotsupported+ 
   #:+gpuapicallerror+ 
   #:+openglnotsupported+
   #:+openglapicallerror+

;; imgproc - Image Filtering

   #:ipl-conv-kernel
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

   #:gaussian-blur
   #:pyr-down
   #:pyr-up

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

;; imgproc - Geometric Image Transformations

   #:+warp-fill-outliers+
   #:+warp-inverse-map+

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
   #:+adaptive-thresh-mean-c+
   #:+adaptive-thresh-gaussian-c+
   #:+dist-l1+ 
   #:+dist-l2+
   #:+dist-c+
   #:+dist-label-ccomp+
   #:+dist-label-pixel+
   #:+dist-mask-3+
   #:+dist-mask-5+
   #:+dist-mask-precise+ 

   #:cvt-color
   
;; imgproc - Histograms

;; imgproc - Structural Analysis and Shape Descriptors

;; imgproc - Motion Analysis and Object Tracking

;; imgproc - Feature Detection

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
   #:convert-image
   #:create-trackbar
   #:destroy-all-windows
   #:destroy-window
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
   #:imshow
   #:move-window
   #:set-mouse-callback
   #:+window-normal+
   #:+window-autosize+
   #:named-window
   #:wait-key

;; highgui - Reading and Writing Images and Video

   #:cap-cam
   #:cap-file
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
   #:cap-get
   #:cap-is-open
   #:cap-read
   #:cap-release
   #:cap-set
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
   #:imread
   #:imwrite
   #:video-writer
   #:video-writer-init
   #:video-writer-is-open
   #:video-writer-write
   #:with-capture

;; highgui - Qt New Functions

   #:+window-fullscreen+ 
   #:+window-freeratio+ 
   #:+window-keepratio+
   #:+wnd-prop-fullscreen+
   #:+wnd-prop-autosize+ 
   #:+wnd-prop-aspectratio+
   #:get-window-property
   #:set-window-property


;; features2d - Feature Detection and Description

   #:brisk

;; features2d - Common Interfaces of Feature Detectors

   #:feat-detector-create
   #:feat-detector-detect

;; features2d - Common Interfaces of Descriptor Extractors

   #:feat-2d-compute


;; features2d - Common Interfaces of Descriptor Matchers

   #:bf-matcher
   #:del-bfmatcher
   #:delete-brisk
   #:descrip-matcher-create 
   #:descrip-matcher-match

;;; features2d - Drawing Function of Keypoints and Matches

   #:+default+
   #:+draw-over-outimg+
   #:+not-draw-single-points+
   #:+draw-rich-keypoints+
   #:draw-matches


;;; nonfree - Feature Detection and Description

   #:surf0
   #:surf5
))

