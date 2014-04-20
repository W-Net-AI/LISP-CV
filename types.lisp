;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; Bindings for OpenCV Types

(in-package :lisp-cv)




;; BFMatcher*
(defctype bf-matcher :pointer)

;; BRISK*
(defctype brisk :pointer)

;; DescriptorMatcher*
(defctype descriptor-matcher :pointer)

;; Feature2D*
(defctype feature-2d :pointer)

;; FeatureDetector*
(defctype feature-detector :pointer)

;; Mat*
(defctype mat :pointer)

;; MatExpr*
(defctype mat-expr :pointer)

;; MouseCallback
(defctype mouse-callback :pointer)

;; Point*
(defctype point :pointer)

;; Point2d*
(defctype point2d :pointer)

;; Point2f*
(defctype point2f :pointer)

;; Point3d*
(defctype point3d :pointer)

;; Point3f*
(defctype point3f :pointer)

;; Point3i*
(defctype point3i :pointer)

;; Scalar*
(defctype scalar :pointer)

;; Rect*
(defctype rect :pointer)

;; RNG*
(defctype rng :pointer)

;; RotatedRect*
(defctype rotated-rect :pointer)

;; Size*
(defctype size :pointer)

;; Size2f*
(defctype size2f :pointer)

;; String*
(defctype string* :pointer)

;; SURF*
(defctype surf :pointer)

;; CvSVM*
(defctype svm :pointer)

;; CvSVMParams*
(defctype svm-params :pointer)

;; TermCriteria*
(defctype term-criteria :pointer)

;; TrackbarCallback*
(defctype trackbar-callback :pointer)

;; Vec2b*
(defctype vec2b :pointer)

;; Vec2d*
(defctype vec2d :pointer)

;; Vec2f*
(defctype vec2f :pointer)

;; Vec2i*
(defctype vec2i :pointer)

;; Vec2s*
(defctype vec2s :pointer)

;; Vec3b*
(defctype vec3b :pointer)

;; Vec3d*
(defctype vec3d :pointer)

;; Vec3f*
(defctype vec3f :pointer)

;; Vec3i*
(defctype vec3i :pointer)
 
;; Vec3s*
(defctype vec3s :pointer)

;; Vec4b*
(defctype vec4b :pointer)

;; Vec4d*
(defctype vec4d :pointer)

;; Vec4f*
(defctype vec4f :pointer)

;; Vec4i*
(defctype vec4i :pointer)

;; Vec4s*
(defctype vec4s :pointer)

;; vector_char*
(defctype vector-char :pointer)

;; vector_DMatch*
(defctype vector-dmatch :pointer)

;; vector_double*
(defctype vector-double :pointer)

;; vector_float*
(defctype vector-float :pointer)

;; vector_int*
(defctype vector-int :pointer)

;; vector_KeyPoint*
(defctype vector-keypoint :pointer)

;; vector_Mat*
(defctype vector-mat :pointer)

;; vector_Point*
(defctype vector-point :pointer)

;; vector_Point2f*
(defctype vector-point2f :pointer)

;; vector_uchar*
(defctype vector-uchar :pointer)

;; VideoCapture*
(defctype video-capture :pointer)

;; VideoWriter*
(defctype video-writer :pointer)






