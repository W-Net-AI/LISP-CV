;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; Bindings for OpenCV types

(in-package :lisp-cv)


;; BFMatcher*
(defctype bf-matcher :pointer)

;; BRISK*
(defctype brisk :pointer)

;; DescriptorMatcher*
(defctype descriptor-matcher :pointer)

;; Feature2D*
(defctype feature-2d :pointer)

;; MouseCallback
(defctype mouse-callback :pointer)

;; CvSVM*
(defctype svm :pointer)

;; CvSVMParams*
(defctype svm-params :pointer)

;; TrackbarCallback*
(defctype trackbar-callback :pointer)

;; vector_DMatch*
(defctype vector-dmatch :pointer)

;; vector_KeyPoint*
(defctype vector-key-point :pointer)

;; vector_Point2f*
(defctype vector-point2f :pointer)

;; vector_Rect*
(defctype vector-rect :pointer)





