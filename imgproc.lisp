;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; imgproc.lisp
;;;; OpenCV bindings
;;;; Image Processing

(in-package :lisp-cv)


;;; Image Filtering


;; void copyMakeBorder(InputArray src, OutputArray dst, int top, int bottom, int left, int right, int borderType, 
;; const Scalar& value=Scalar())
;; void cv_copyMakeBorder(Mat* src, Mat* dst, int top, int bottom, int left, int right, int borderType, Scalar* value) 
(defcfun ("cv_copyMakeBorder" copy-make-border) :void 
  "Forms a border around an image."
  (src (:pointer mat))
  (dest (:pointer mat))
  (top :int)
  (bottom :int)
  (left :int)
  (right :int)
  (border-type :int)
  (value (:pointer scalar)))


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


;; void Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy, int ksize=3, double scale=1, double delta=0, 
;; int borderType=BORDER_DEFAULT)
;; void cv_Sobel(Mat* src, Mat* dst, int ddepth, int dx, int dy, int ksize, double scale, double delta, int borderType) 
(defcfun ("cv_Sobel" %sobel) :void
  (src (:pointer mat))
  (dest (:pointer mat))
  (ddepth :int)
  (dx :int)
  (dy :int)
  (ksize :int)
  (scale :double)
  (delta :double)
  (border-type :int))

(defun sobel (src dest ddepth dx dy &optional (ksize 3) (scale 1d0) (delta 0d0) (border-type +border-default+))
       "Calculates the first, second, third, or mixed image derivatives using an extended Sobel operator."
       (%sobel src dest ddepth dx dy ksize scale delta border-type))


;;; Geometric Image Transformations


;; void resize(InputArray src, OutputArray dst, Size dsize, double fx=0, double fy=0, int interpolation=INTER_LINEAR )
;; void cv_resize(Mat* src, Mat* dst, Size* dsize, double fx, double fy, int interpolation)
(defcfun ("cv_resize" %resize) :void
  (src (:pointer mat))
  (dest (:pointer mat))
  (dsize (:pointer size))
  (fx :double)
  (fy :double)
  (interpolation :int))

(defun resize (src dest dsize &optional (fx 0d0) (fy 0d0) (interpolation +inter-linear+))
  "Resizes an image."
  (%resize src dest dsize fx fy interpolation))


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


;; void equalizeHist(InputArray src, OutputArray dst)
;; void cv_equalizeHist(Mat* src, Mat* dst)
(defcfun ("cv_equalizeHist" equalize-hist) :void
  "Equalizes the histogram of a grayscale image."
  (src (:pointer mat))
  (dest (:pointer mat)))


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


;; void matchTemplate(InputArray image, InputArray templ, OutputArray result, int method)
;; void cv_matchTemplate(Mat* image, Mat* templ, Mat* result, int method) 
(defcfun ("cv_matchTemplate" match-template) :void
  "Compares a template against overlapped image regions."
  (image (:pointer mat))
  (templ (:pointer mat))
  (result (:pointer mat))
  (method :int))




