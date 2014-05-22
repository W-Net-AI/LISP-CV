;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; photo.lisp
;;;; OpenCV bindings
;;;; Non-Photorealistic Rendering


(in-package :lisp-cv)


;;; Inpainting


;; void inpaint(InputArray src, InputArray inpaintMask, OutputArray dst, double inpaintRadius, int flags)
;; void cv_inpaint(Mat* src, Mat* inpaintMask, Mat* dst, double inpaintRadius, int flags)
(defcfun ("cv_inpaint" in-paint) :void
  (src mat)
  (in-paint-mask mat)
  (dest mat)
  (in-paint-radius :double)
  (flags :int))


;;; Seamless Cloning


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



;;; Non-Photorealistic Rendering


;; void detailEnhance(InputArray src, OutputArray dst, float sigma_s=10, float sigma_r=0.15f)
;; void cv_detailEnhance(Mat* src, Mat* dst, float sigma_s, float sigma_r) 
(defcfun ("cv_detailEnhance" %detail-enhance) :void
  "This filter enhances the details of a particular image."
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
  "Filtering is the fundamental operation in image and 
   video processing. Edge-preserving smoothing filters 
   are used in many different applications."
   (src mat)
   (dest mat)
   (flags :int)
   (sigma-s :float)
   (sigma-r :float))

(defun edge-preserving-filter (src dest &optional (flags 1) (sigma-s 60f0) (sigma-r 0.4f0))
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