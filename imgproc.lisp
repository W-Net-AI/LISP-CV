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


;;void pyrDown(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )
;;void cv_pyrDown(Mat* src, Mat* dst, Size* dstsize, int borderType)
(defcfun ("cv_pyrDown" %pyr-down) :void 
	 (src (:pointer mat))
	 (dest (:pointer mat))
	 (dstsize (:pointer size))
	 (border-type :int))

(defun pyr-down (src dest &optional (dstsize (size)) (border-type +border-default+))
  "Blurs an image and downsamples it."
  (%pyr-down src dest dstsize border-type))


;;void pyrUp(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )
;;void cv_pyrUp(Mat* src, Mat* dst, Size* dstsize, int borderType)
(defcfun ("cv_pyrUp" %pyr-up) :void 
  (src (:pointer mat))
  (dest (:pointer mat))
  (dstsize (:pointer size))
  (border-type :int))

(defun pyr-up (src dest &optional (dstsize (size)) (border-type +border-default+))
  "Upsamples an image and then blurs it."
  (%pyr-up src dest dstsize border-type))



;;; Miscellaneous Image Transformations


;; void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )
;; void cv_cvtColor(Mat* src, Mat* dst, int code, int dstCn) 
(defcfun ("cv_cvtColor" %cvt-color) :void
  (self (:pointer mat))
  (dest (:pointer mat))
  (code :int)
  (dest-cn :int))

(defun cvt-color (src dest code &optional (dest-cn 0))
  "Converts an image from one cofmisclor space to another."
   (%cvt-color src dest code dest-cn))


;; double threshold(InputArray src, OutputArray dst, double thresh, double maxval, int type)
;; double cv_threshold(Mat* src, Mat* dst, double thresh, double maxval, int type) 
(defcfun ("cv_threshold" threshold) :double
  (src (:pointer mat))
  (dest (:pointer mat))
  (thresh :double)
  (max-val :double)
  (type :int))


;;; Histograms

;;; Structural Analysis and Shape Descriptors

;;; Motion Analysis and Object Tracking


;;; Feature Detection


;; void Canny(InputArray image, OutputArray edges, double low-thresh, double high-thresh, int apertureSize=3, bool L2gradient=false )
;; void cv_Canny(Mat* image, Mat* edges, double low-thresh, double high-thresh, int apertureSize, bool L2gradient)
(defcfun ("cv_Canny" %canny) :void 
	 (image (:pointer mat))
	 (edges (:pointer mat))
	 (low-thresh :double)
	 (high-thresh :double)
	 (aperture-size :int)
	 (l2-gradient :boolean))

(defun canny (image edges low-thresh high-thresh &optional (aperture-size 3) (l2-gradient nil))       
  "Finds edges in an image using the [Canny86] algorithm."
       (%canny image edges low-thresh high-thresh aperture-size l2-gradient))


;;; Object Detection




