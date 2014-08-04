;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; imgproc.lisp
;;;; OpenCV bindings
;;;; Image Processing.


(in-package :lisp-cv)



;;; Macros

;; static inline Scalar morphologyDefaultBorderValue() { return Scalar::all(DBL_MAX); }
;; Scalar* cv_create_morphologyDefaultBorderValue()
(defcfun ("cv_create_morphologyDefaultBorderValue" morphology-default-border-value) scalar)


;; static inline Scalar morphologyDefaultBorderValue() { return Scalar::all(DBL_MAX); }
;; Scalar* cv_create_morphologyDefaultBorderValue()
(defcfun ("cv_create_morphologyDefaultBorderValue" make-morphology-default-border-value) scalar)



;;; Image Filtering


;; void bilateralFilter(InputArray src, OutputArray dst, int d, double sigmaColor, double sigmaSpace, int borderType=BORDER_DEFAULT )
;; void cv_bilateralFilter(Mat* src, Mat* dst, int d, double sigmaColor, double sigmaSpace, int borderType)
(defcfun ("cv_bilateralFilter" %bilateral-filter) :void
  (src mat)
  (dest mat)
  (d :int)
  (sigma-color :double)
  (sigma-space :double)
  (border-type :int))


(defun bilateral-filter (src dest d sigma-color sigma-space &optional (border-type +border-default+))
       "Applies the bilateral filter to an image."
       (%bilateral-filter src dest d sigma-color sigma-space border-type))


;; void blur(InputArray src, OutputArray dst, Size ksize, Point anchor=Point(-1,-1), int borderType=BORDER_DEFAULT )
;; cv_blur(Mat* src, Mat* dst, Size* ksize, Point* anchor, int borderType)
(defcfun ("cv_blur" %blur) :void
  (src mat)
  (dest mat)
  (ksize size)
  (anchor point)
  (border-type :int))


(defun blur (src dest ksize &optional (anchor (point -1 -1) given-anchor) (border-type +border-default+))
  "Blurs an image using the normalized box filter."
  (%blur src dest ksize anchor border-type)
  (if given-anchor nil (del-point anchor))) 


;; void copyMakeBorder(InputArray src, OutputArray dst, int top, int bottom, int left, int right, int borderType, 
;; const Scalar& value=Scalar())
;; void cv_copyMakeBorder(Mat* src, Mat* dst, int top, int bottom, int left, int right, int borderType, Scalar* value) 
(defcfun ("cv_copyMakeBorder" %copy-make-border) :void 
  (src mat)
  (dest mat)
  (top :int)
  (bottom :int)
  (left :int)
  (right :int)
  (border-type :int)
  (value scalar))


(defun copy-make-border (src dest top bottom left right border-type &optional (value (scalar-0)))
  "Forms a border around an image."
  (%copy-make-border src dest top bottom left right border-type value))


;; void dilate(InputArray src, OutputArray dst, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
;; int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue())
;; void cv_dilate(Mat* src, Mat* dst, Mat* kernel, Point* anchor, int iterations, int borderType, Scalar* borderValue)
(defcfun ("cv_dilate" %dilate) :void
  (src mat)
  (dest mat)
  (kernel mat)
  (anchor point)
  (iterations :int)
  (border-type :int)
  (border-value scalar))


(defun dilate (src dest kernel &optional (anchor (point -1 -1) given-anchor) (iterations 1) (border-type +border-constant+) (border-value (morphology-default-border-value) given-border-value))
  "Dilates an image by using a specific structuring element."
  (%dilate src dest kernel anchor iterations border-type border-value)
  (if given-anchor nil (del-point anchor)) (if given-border-value nil (del-scalar border-value)))


;; void erode(InputArray src, OutputArray dst, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
;; int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue() )
;; void cv_erode(Mat* src, Mat* dst, Mat* kernel, Point* anchor, int iterations, int borderType, Scalar* borderValue)
(defcfun ("cv_erode" %erode) :void
  (src mat)
  (dest mat)
  (kernel mat)
  (anchor point)
  (iterations :int)
  (border-type :int)
  (border-value scalar))


(defun erode (src dest kernel &optional (anchor (point -1 -1) given-anchor) (iterations 1) (border-type +border-constant+) (border-value (morphology-default-border-value) given-border-value))
  "Dilates an image by using a specific structuring element."
  (%erode src dest kernel anchor iterations border-type border-value)
  (if given-anchor nil (del-point anchor)) (if given-border-value nil (del-scalar border-value)))


;; void filter2D(InputArray src, OutputArray dst, int ddepth, InputArray kernel, Point anchor=Point(-1,-1), double delta=0, 
;; int borderType=BORDER_DEFAULT )
;; void cv_filter2D(Mat* src, Mat* dst, int ddepth, Mat* kernel, Point* anchor, double delta, int borderType)
(defcfun ("cv_filter2D" %filter-2d) :void
  (src mat)
  (dest mat)
  (ddepth :int)
  (kernel mat)
  (anchor point)
  (delta :double)
  (border-type :int))


(defun filter-2d (src dest ddepth kernel &optional (anchor (point -1 -1) given-anchor) (delta 0d0) (border-type +border-default+))
  "Convolves an image with the kernel."
  (%filter-2d src dest ddepth kernel anchor delta border-type)
  (if given-anchor nil (del-point anchor)))


;; void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0, int borderType=BORDER_DEFAULT )
;; void cv_GaussianBlur(Mat* src, Mat* dst, Size* ksize, double sigmaX, double sigmaY, int borderType)
(defcfun ("cv_GaussianBlur" %gaussian-blur) :void
  (src mat)
  (dest mat)
  (ksize size)
  (sigma-x :double)
  (sigma-y :double)
  (border-type :int))


(defun gaussian-blur(src dest ksize sigma-x &optional (sigma-y 0) (border-type +border-default+))
  "Blurs an image using a Gaussian filter."
   (%gaussian-blur src dest ksize sigma-x sigma-y border-type))


;; void medianBlur(InputArray src, OutputArray dst, int ksize)
;; void cv_medianBlur(Mat* src, Mat* dst, int ksize)
(defcfun ("cv_medianBlur" median-blur) :void
  "Blurs an image using the median filter."
  (src mat)
  (dest mat)
  (ksize :int))


;; void morphologyEx(InputArray src, OutputArray dst, int op, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
;; int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue() )
;; void cv_morphologyEx(Mat* src, Mat* dst, int op, Mat* kernel, Point* anchor, int iterations, int borderType, Scalar* borderValue)
(defcfun ("cv_morphologyEx" %morphology-ex) :void
  (src mat)
  (dest mat)
  (op :int)
  (kernel mat)
  (anchor point)
  (iterations :int)
  (border-type :int)
  (border-value scalar))


(defun morphology-ex (src dest op kernel &optional (anchor (point -1 -1) given-anchor) (iterations 1) (border-type +border-constant+) 
					   (border-value (morphology-default-border-value)))
  "Performs advanced morphological transformations."
  (%morphology-ex src dest op kernel anchor iterations border-type border-value)
  (if given-anchor nil (del-point anchor)))


;; Mat getStructuringElement(int shape, Size ksize, Point anchor=Point(-1,-1))
;; Mat* cv_getStructuringElement(int shape, Size* ksize, Point* anchor)
(defcfun ("cv_getStructuringElement" %get-structuring-element) mat
  (shape :int)
  (ksize size)
  (kernel point))


(defun get-structuring-element (shape ksize &optional (kernel (point -1 -1) given-kernel) return) 
  "Returns a structuring element of the specified size and shape for morphological operations."
  (setf return (%get-structuring-element shape ksize kernel))
  (if given-kernel nil (del-point kernel)) 
  return)


;; void Laplacian(InputArray src, OutputArray dst, int ddepth, int ksize=1, double scale=1, double delta=0, int borderType=BORDER_DEFAULT )
;; void cv_Laplacian(Mat* src, Mat* dst, int ddepth, int ksize, double scale, double delta, int borderType)
(defcfun ("cv_Laplacian" %laplacian) :void
  (src mat)
  (dest mat)
  (ddepth :int)
  (ksize :int)
  (scale :double)
  (delta :double)
  (border-type :int))


(defun laplacian (src dest ddepth &optional (ksize 1) (scale 1d0) (delta 0d0) (border-type +border-default+))
       "Calculates the Laplacian of an image."
       (%laplacian src dest ddepth ksize scale delta border-type))


;;void pyrDown(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )
;;void cv_pyrDown(Mat* src, Mat* dst, Size* dstsize, int borderType)
(defcfun ("cv_pyrDown" %pyr-down) :void 
	 (src mat)
	 (dest mat)
	 (dstsize size)
	 (border-type :int))


(defun pyr-down (src dest &optional (dstsize (size-0) given-dstsize) (border-type +border-default+))
  "Blurs an image and downsamples it."
  (%pyr-down src dest dstsize border-type)
  (if given-dstsize nil (del-size dstsize)))


;;void pyrUp(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )
;;void cv_pyrUp(Mat* src, Mat* dst, Size* dstsize, int borderType)
(defcfun ("cv_pyrUp" %pyr-up) :void 
  (src mat)
  (dest mat)
  (dstsize size)
  (border-type :int))


(defun pyr-up (src dest &optional (dstsize (size-0) given-dstsize) (border-type +border-default+))
  "Upsamples an image and then blurs it."
  (%pyr-up src dest dstsize border-type)
  (if given-dstsize nil (del-size dstsize)))


;; void Scharr(InputArray src, OutputArray dst, int ddepth, int dx, int dy, double scale=1, double delta=0, int borderType=BORDER_DEFAULT )
;; void cv_Scharr(Mat* src, Mat* dst, int ddepth, int dx, int dy, double scale, double delta, int borderType)
(defcfun ("cv_Scharr" %scharr) :void
  (src mat)
  (dest mat)
  (ddepth :int)
  (dx :int)
  (dy :int)
  (scale :double)
  (delta :double)
  (border-type :int))


(defun scharr (src dest ddepth dx dy &optional (scale 1) (delta 1d0) (border-type +border-default+))
       "Calculates the first x- or y- image derivative using Scharr operator."
       (%scharr src dest ddepth dx dy scale delta border-type))


;; void Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy, int ksize=3, double scale=1, double delta=0, 
;; int borderType=BORDER_DEFAULT)
;; void cv_Sobel(Mat* src, Mat* dst, int ddepth, int dx, int dy, int ksize, double scale, double delta, int borderType) 
(defcfun ("cv_Sobel" %sobel) :void
  (src mat)
  (dest mat)
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



;; Mat getAffineTransform(InputArray src, InputArray dst)
;; Mat* cv_getAffineTransform(Mat* src, Mat* dst) 
(defcfun ("cv_getAffineTransform" get-affine-transform) mat
  "Calculates an affine transform from three pairs of the corresponding points."
  (src mat)
  (dest mat))


;; Mat getPerspectiveTransform(InputArray src, InputArray dst)
;; Mat* cv_getPerspectiveTransform(Mat* src, Mat* dst)
(defcfun ("cv_getPerspectiveTransform" get-perspective-transform) mat
  "Calculates a perspective transform from four pairs of the corresponding points."
  (src mat)
  (dst mat))


;; Mat getRotationMatrix2D(Point2f center, double angle, double scale)
;; Mat* cv_getRotationMatrix2D(Point2f* center, double angle, double scale)
(defcfun ("cv_getRotationMatrix2D" get-rotation-matrix-2d) mat
  "Calculates an affine matrix of 2D rotation."
  (center point-2f)
  (angle :double)  
  (scale :double))


;; void invertAffineTransform(InputArray M, OutputArray iM)
;; void cv_invertAffineTransform(Mat* M, Mat* iM)
(defcfun ("cv_invertAffineTransform" invert-affine-transform) :void
  "Inverts an affine transformation."
  (m mat)
  (i-m mat))


;; void remap(InputArray src, OutputArray dst, InputArray map1, InputArray map2, int interpolation, int borderMode=BORDER_CONSTANT, const Scalar& borderValue=Scalar())
;; void cv_remap(Mat* src, Mat* dst, Mat* map1, Mat* map2, int interpolation, int borderMode, Scalar* borderValue) 
(defcfun ("cv_remap" %remap) :void
  "Sets all or some of the array elements to the specified value."
  (src mat)
  (dest mat)
  (map1 mat)
  (map2 mat)
  (iterpolation :int)
  (border-mode :int)
  (border-value scalar))


(defun remap (src dest map1 map2 interpolation &optional (border-mode +border-constant+) (border-value (scalar) given-border-value))
  "Sets all or some of the array elements to the specified value."
  (%remap src dest map1 map2 interpolation border-mode border-value)
  (if given-border-value nil (del-scalar border-value)))


;; void resize(InputArray src, OutputArray dst, Size dsize, double fx=0, double fy=0, int interpolation=INTER_LINEAR )
;; void cv_resize(Mat* src, Mat* dst, Size* dsize, double fx, double fy, int interpolation)
(defcfun ("cv_resize" %resize) :void
  (src mat)
  (dest mat)
  (dsize size)
  (fx :double)
  (fy :double)
  (interpolation :int))


(defun resize (src dest dsize &optional (fx 0d0) (fy 0d0) (interpolation +inter-linear+))
  "Resizes an image."
  (%resize src dest dsize fx fy interpolation))


;; void warpAffine(InputArray src, OutputArray dst, InputArray M, Size dsize, int flags=INTER_LINEAR, 
;; int borderMode=BORDER_CONSTANT, const Scalar& borderValue=Scalar())
;; void cv_warpAffine(Mat* src, Mat* dst, Mat* M, Size* dsize, int flags, int borderMode, Scalar* borderValue) 
(defcfun ("cv_warpAffine" %warp-affine) :void
  (src mat)
  (dest mat)
  (m mat)
  (dsize size)
  (flags :int)
  (border-mode :int)
  (border-value scalar))


(defun warp-affine (src dest m dsize &optional (flags +inter-linear+) (border-mode +border-constant+) 
                   (border-value (scalar-0) given-border-value)) 
       "Applies an affine transformation to an image."
       (%warp-affine src dest m dsize flags border-mode border-value)
       (if given-border-value nil (del-scalar border-value)))


;; void warpPerspective(InputArray src, OutputArray dst, InputArray M, Size dsize, int flags=INTER_LINEAR, 
;;                      int borderMode=BORDER_CONSTANT, const Scalar& borderValue=Scalar())
;; void cv_warpPerspective(Mat* src, Mat* dst, Mat* M, Size* dsize, int flags, int borderMode, Scalar* borderValue)
(defcfun ("cv_warpPerspective" %warp-perspective) :void
  (src mat)
  (dest mat)
  (m mat)
  (dsize size)
  (flags :int)
  (border-mode :int)
  (border-value scalar))


(defun warp-perspective (src dest m dsize &optional (flags +inter-linear+) (border-mode +border-constant+) 
					    (border-value (scalar-0) given-border-value)) 
					    "Applies a perspective transformation to an image."
					    (%warp-perspective src dest m dsize flags border-mode border-value)
					    (if given-border-value nil (del-scalar border-value)))



;;; Miscellaneous Image Transformations



;; void adaptiveThreshold(InputArray src, OutputArray dst, double maxValue, int adaptiveMethod, int thresholdType, int blockSize, double C)
;; void cv_adaptiveThreshold(Mat* src, Mat* dst, double maxValue, int adaptiveMethod, int thresholdType, int blockSize, double C)
(defcfun ("cv_adaptiveThreshold" adaptive-threshold) :void
  (src mat)
  (dest mat)
  (max-value :double)
  (adaptive-method :int)
  (threshold-type :int) 
  (blocksize :int) 
  (c :double))


;; void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )
;; void cv_cvtColor(Mat* src, Mat* dst, int code, int dstCn) 
(defcfun ("cv_cvtColor" %cvt-color) :void
  (self mat)
  (dest mat)
  (code :int)
  (dest-cn :int))


(defun cvt-color (src dest code &optional (dest-cn 0))
  "Converts an image from one cofmisclor space to another."
   (%cvt-color src dest code dest-cn))


;; void distanceTransform( InputArray src, OutputArray dst, int distanceType, int maskSize, int dstType=CV_32F)
;; void cv_distanceTransform5(Mat* src, Mat* dst, int distanceType, int maskSize, int dstType)
(defcfun ("cv_distanceTransform5" %distance-transform5) :void
  "Computes the distance transform map"
  (src mat)
  (dest mat)
  (distance-type :int)
  (mask-size :int)
  (dst-type :int))


;; void distanceTransform(InputArray src, OutputArray dst, OutputArray labels, int distanceType, int maskSize, 
;; int labelType=DIST_LABEL_CCOMP )
;; void cv_distanceTransform(Mat* src, Mat* dst, Mat* labels, int distanceType, int maskSize, int labelType)
(defcfun ("cv_distanceTransform" %%distance-transform) :void
  "Builds the discrete Voronoi diagram"
  (src mat)
  (dest mat)
  (*labels mat)
  (distance-type :int)
  (mask-size :int)
  (label-type :int))


(defun %distance-transform (src dest *labels distance-type mask-size &optional (label-type +dist-label-ccomp+))
  (%%distance-transform src dest *labels distance-type mask-size label-type))


(defun distance-transform5 (src dest distance-type mask-size &optional (dst-type +32f+))
  (%distance-transform5 src dest distance-type mask-size dst-type))


(defun distance-transform (&rest args)
  (if (typep (third args) 'cv-mat)
      (apply #'%distance-transform args)
      (apply #'distance-transform5 args)))


;; int floodFill(InputOutputArray image, InputOutputArray mask, Point seedPoint, Scalar newVal, Rect* rect=0, 
;;               Scalar loDiff=Scalar(), Scalar upDiff=Scalar(), int flags=4 )
;; int cv_floodFill(Mat* image, Mat* mask, Point* seedPoint, Scalar* newVal, Rect* rect, Scalar* loDiff, Scalar* upDiff, int flags) 
(defcfun ("cv_floodFill" %flood-fill) :int
  (image mat)
  (mask mat)
  (seed-point point)
  (new-val scalar)
  (rect rect)
  (lo-diff scalar)
  (up-diff scalar)
  (flags :int))


(defun flood-fill (image mask seed-point new-val &optional (rect (null-pointer)) (lo-diff (scalar-0)) (up-diff (scalar-0)) (flags 4))
  (%flood-fill image mask seed-point new-val rect lo-diff up-diff flags))


;; double threshold(InputArray src, OutputArray dst, double thresh, double maxval, int type)
;; double cv_threshold(Mat* src, Mat* dst, double thresh, double maxval, int type) 
(defcfun ("cv_threshold" threshold) :double
  (src mat)
  (dest mat)
  (thresh :double)
  (max-val :double)
  (type :int))



;;; Histograms


;; void equalizeHist(InputArray src, OutputArray dst)
;; void cv_equalizeHist(Mat* src, Mat* dst)
(defcfun ("cv_equalizeHist" equalize-hist) :void
  "Equalizes the histogram of a grayscale image."
  (src mat)
  (dest mat))


;;; Structural Analysis and Shape Descriptors


;;; Motion Analysis and Object Tracking


;; void createHanningWindow(OutputArray dst, Size winSize, int type)
;; void cv_createHanningWindow(Mat* dst, Size* winSize, int type)
(defcfun ("cv_createHanningWindow" create-hanning-window) :void
  (dest mat)
  (win-size size)
  (type :int))


;; Point2d phaseCorrelate(InputArray src1, InputArray src2, InputArray window=noArray(), double* response=0)
;; Point2d* cv_phaseCorrelate(Mat* src1, Mat* src2, Mat* window, double* response) 
(defcfun ("cv_phaseCorrelate" %phase-correlate) point-2d
  (src-1 mat)
  (src-2 mat)
  (window mat)
  (response :pointer))


(defun phase-correlate (src-1 src-2 &optional (window (mat) given-window) (response (null-pointer)))
       (let ((return (%phase-correlate src-1 src-2 window response)))
	 (if given-window nil (del-mat window))
	 return))


;;; Feature Detection


;; void Canny(InputArray image, OutputArray edges, double low-thresh, double high-thresh, int apertureSize=3, bool L2gradient=false )
;; void cv_Canny(Mat* image, Mat* edges, double low-thresh, double high-thresh, int apertureSize, bool L2gradient)
(defcfun ("cv_Canny" %canny) :void 
	 (image mat)
	 (edges mat)
	 (low-thresh :double)
	 (high-thresh :double)
	 (aperture-size :int)
	 (l2-gradient :boolean))


(defun canny (image edges low-thresh high-thresh &optional (aperture-size 3) (l2-gradient nil))       
  "Finds edges in an image using the [Canny86] algorithm."
       (%canny image edges low-thresh high-thresh aperture-size l2-gradient))


;; void cv_cornerSubPix(Mat* image, Mat* corners, Size* winSize, Size* zeroZone, TermCriteria* criteria)
;; void cv_cornerSubPix2(Mat* image, Mat* corners, Size* winSize, Size* zeroZone, TermCriteria* criteria)
(defcfun ("cv_cornerSubPix" corner-sub-pix) :void
  "Refines the corner locations."
  (image mat)
  (corners mat)
  (win-size size)
  (zero-zone size)
  (criteria term-criteria))


;; void goodFeaturesToTrack(InputArray image, OutputArray corners, int maxCorners, double qualityLevel, double minDistance, 
;; InputArray mask=noArray(), int blockSize=3, bool useHarrisDetector=false, double k=0.04 )

;; void cv_goodFeaturesToTrack(Mat* image, Mat* corners, int maxCorners, double qualityLevel, double minDistance, Mat* mask, 
;; int blockSize, bool useHarrisDetector, double k) 

(defcfun ("cv_goodFeaturesToTrack" %good-features-to-track) :void
  (image mat)
  (corners mat)
  (max-corners :int)
  (quality-level :double)
  (min-distance :double)
  (mask mat)
  (block-size :int)
  (use-harris-detector :boolean)
  (k :double))


(defun good-features-to-track (image corners max-corners quality-level min-distance &optional (mask (%mat) given-mask) (block-size 3) 
										      (use-harris-detector nil) (k 0.04d0))
  "Determines strong corners on an image."
  (%good-features-to-track image corners max-corners quality-level min-distance mask block-size use-harris-detector k)
  (if given-mask nil (del-mat mask)))



;;; Object Detection



;; void matchTemplate(InputArray image, InputArray templ, OutputArray result, int method)
;; void cv_matchTemplate(Mat* image, Mat* templ, Mat* result, int method) 
(defcfun ("cv_matchTemplate" match-template) :void
  "Compares a template against overlapped image regions."
  (image mat)
  (templ mat)
  (result mat)
  (method :int))




