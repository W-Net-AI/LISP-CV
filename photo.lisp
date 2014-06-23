;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; photo.lisp
;;;; OpenCV bindings
;;;; Computational Photography


(in-package :lisp-cv)


;;; Inpainting


;; void inpaint(InputArray src, InputArray inpaintMask, OutputArray dst, double inpaintRadius, int flags)
;; void cv_inpaint(Mat* src, Mat* inpaintMask, Mat* dst, double inpaintRadius, int flags)
(defcfun ("cv_inpaint" in-paint) :void
  "Restores the selected region in an image using the region neighborhood."
  (src mat)
  (in-paint-mask mat)
  (dest mat)
  (in-paint-radius :double)
  (flags :int))


;;; Decolorization


;; void decolor(InputArray src, OutputArray grayscale, OutputArray color_boost)
;; void cv_decolor(Mat* src, Mat* grayscale, Mat* color_boost)
(defcfun ("cv_decolor" decolor) :void
  "Transforms a color image to a grayscale image. It is a basic tool in digital 
   printing, stylized black-and-white photograph rendering, and in many single 
   channel image processing applications."
   (src mat)
   (grayscale mat)
   (color-boost mat))



;;; Seamless Cloning


;; void colorChange(InputArray src, InputArray mask, OutputArray dst, float red_mul=1.0f, float green_mul=1.0f, float blue_mul=1.0f)
;; void cv_colorChange(Mat* src, Mat* mask, Mat* dst, float red_mul, float green_mul, float blue_mul)
(defcfun ("cv_colorChange" %color-change) :void
  (src mat)
  (mask mat)
  (dest mat)
  (red-mul :float)
  (green-mul :float)
  (blue-mul :float))


(defun color-change (src mask dest &optional (red-mul 1.0f0) (green-mul 1.0f0) (blue-mul 1.0f0))
  "Given an original color image, two differently colored 
   versions of this image can be mixed seamlessly."
       (%color-change src mask dest red-mul green-mul blue-mul))


;; void illuminationChange(InputArray src, InputArray mask, OutputArray dst, float alpha=0.2f, float beta=0.4f)
;; void cv_illuminationChange(Mat* src, Mat* mask, Mat* dst, float alpha, float beta) 
(defcfun ("cv_illuminationChange" %illumination-change) :void
   (src mat)
   (mask mat)
   (dest mat)
   (alpha :float)
   (beta :float))


(defun illumination-change (src mask dest &optional (alpha 0.2f0) (beta 0.4f0))
  "Applying an appropriate non-linear transformation to the gradient field 
   inside the selection and then integrating back with a Poisson solver, m-
   odifies locally the apparent illumination of an image."
       (%illumination-change src mask dest alpha beta))


;; void seamlessClone(InputArray src, InputArray dst, InputArray mask, Point p, OutputArray blend, int flags)
;; void cv_seamlessClone(Mat* src, Mat* dst, Mat* mask, Point* p, Mat* blend, int flags)
(defcfun ("cv_seamlessClone" seamless-clone) :void
  "Image editing tasks concern either global changes (color/intensity corrections, filters, 
   deformations) or local changes concerned to a selection. Here we are interested in achie-
   ving local changes, ones that are restricted to a region manually selected (ROI), in a s-
   eamless and effortless manner. The extent of the changes ranges from slight distortions 
   to complete replacement by novel content."
   (src mat)
   (dest mat)
   (mask mat)
   (p point)
   (blend mat)
   (flags :int))


;; void textureFlattening(InputArray src, InputArray mask, OutputArray dst, double low_threshold=30 , double high_threshold=45, 
;;                        int kernel_size=3)
;; void cv_textureFlattening(Mat* src, Mat* mask, Mat* dst, double low_threshold, double high_threshold, int kernel_size)
(defcfun ("cv_textureFlattening" %texture-flattening) :void
   (src mat)
   (mask mat)
   (dest mat)
   (low-threshold :double)
   (high-threshold :double)
   (kernal-size :int))


(defun texture-flattening (src mask dest &optional (low-threshold 30d0) (high-threshold 45d0) (kernal-size 3))
  "By retaining only the gradients at edge locations, before integrating with the Poisson solver, one 
   washes out the texture of the selected region, giving its contents a flat aspect. Here Canny Edge 
   Detector is used."
       (%texture-flattening src mask dest low-threshold high-threshold kernal-size))



;;; Non-Photorealistic Rendering



;; void detailEnhance(InputArray src, OutputArray dst, float sigma_s=10, float sigma_r=0.15f)
;; void cv_detailEnhance(Mat* src, Mat* dst, float sigma_s, float sigma_r) 
(defcfun ("cv_detailEnhance" %detail-enhance) :void
  (src mat)
  (dest mat)
  (sigma-s :float)
  (sigma-r :float))


(defun detail-enhance (src dest &optional (sigma-s 60f0) (sigma-r 0.45f0))
  "This filter enhances the details of a particular image."
  (%detail-enhance src dest sigma-s sigma-r))


;; void edgePreservingFilter(InputArray src, OutputArray dst, int flags=1, float sigma_s=60, float sigma_r=0.4f)
;; void cv_edgePreservingFilter(Mat* src, Mat* dst, int flags, float sigma_s, float sigma_r) 
(defcfun ("cv_edgePreservingFilter" %edge-preserving-filter) :void
   (src mat)
   (dest mat)
   (flags :int)
   (sigma-s :float)
   (sigma-r :float))


(defun edge-preserving-filter (src dest &optional (flags 1) (sigma-s 60f0) (sigma-r 0.4f0))
  "Filtering is the fundamental operation in image and video processing. Edge
   preserving smoothing filters are used in many different applications."
       (%edge-preserving-filter src dest flags sigma-s sigma-r))


;; void pencilSketch(InputArray src, OutputArray dst1, OutputArray dst2, float sigma_s=60, float sigma_r=0.07f, float shade_factor=0.02f)
;; void cv_pencilSketch(Mat* src, Mat* dst1, Mat* dst2, float sigma_s, float sigma_r, float shade_factor) 
(defcfun ("cv_pencilSketch" %pencil-sketch) :void
  (src mat)
  (dest1 mat)
  (dest2 mat)
  (sigma-s :float)
  (sigma-r :float)
  (shade-factor :float))


(defun pencil-sketch (src dest1 dest2 &optional (sigma-s 60f0) (sigma-r 0.07f0) (shade-factor 0.02f0))
  "Pencil-like non-photorealistic line drawing."
  (%pencil-sketch src dest1 dest2 sigma-s sigma-r shade-factor))


;; void stylization(InputArray src, OutputArray dst, float sigma_s=60, float sigma_r=0.45f)
;; void cv_stylization(Mat* src, Mat* dst, float sigma_s, float sigma_r)
(defcfun ("cv_stylization" %stylization) :void
   (src mat)
   (dest mat)
   (sigma-s :float)
   (sigma-r :float))


(defun stylization (src dest &optional (sigma-s 60f0) (sigma-r 0.45f0))
  "Stylization aims to produce digital imagery with a wide variety 
   of effects not focused on photorealism. Edge-aware filters are 
   ideal for stylization, as they can abstract regions of low cont-
   rast while preserving, or enhancing, high-contrast features."
       (%stylization src dest sigma-s sigma-r))
