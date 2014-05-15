;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; Bindings for OpenCV types

(in-package :lisp-cv)

;; MouseCallback.
(defctype mouse-callback :pointer)

;; CvSVM*
(defctype svm :pointer)

;; CvSVMParams*
(defctype svm-params :pointer)

;; TrackbarCallback*
(defctype trackbar-callback :pointer)




