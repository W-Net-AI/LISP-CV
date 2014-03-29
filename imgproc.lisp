;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; imgproc.lisp
;;;; OpenCV bindings
;;;; Image Processing
(in-package :lisp-cv)


;;; Image Filtering


;; void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0, int borderType=BORDER_DEFAULT )
;; void cv_GaussianBlur(Mat* src, Mat* dst, Size* ksize, double sigmaX, double sigmaY, int borderType)
(defcfun ("cv_GaussianBlur" %gaussian-blur) :void
  (src (:pointer mat))
  (dest (:pointer mat))
  (ksize (:pointer size))
  (sigma-x :double)
  (sigma-y :double)
  (border-type :int))


(defun gaussian-blur(src dest ksize sigma-x &optional (sigma-y 0) (border-type +border-default+))
  "Blurs an image using a Gaussian filter."
   (%gaussian-blur src dest ksize sigma-x sigma-y border-type))


;;; Miscellaneous Image Transformations


;; void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )
;; void cv_cvtColor(Mat* src, Mat* dst, int code, int dstCn) 
(defcfun ("cv_cvtColor" %cvt-color) :void
  (self (:pointer mat))
  (dest (:pointer mat))
  (code :int)
  (dest-cn :int))

(defun cvt-color (src dest code &optional (dest-cn 0))
  "Converts an image from one color space to another."
   (%cvt-color src dest code dest-cn))


;;; Histograms

;;; Structural Analysis and Shape Descriptors

;;; Motion Analysis and Object Tracking

;;; Feature Detection

;;; Object Detection




