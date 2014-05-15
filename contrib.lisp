;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; contrib.lisp
;;;; OpenCV bindings
;;;; Contributed/Experimental Stuff

(in-package :lisp-cv)


;;; ColorMaps in OpenCV.


;; void applyColorMap(InputArray src, OutputArray dst, int colormap)
;; void cv_applyColorMap(Mat* src, Mat* dst, int colormap)
(defcfun ("cv_applyColorMap" apply-color-map) :void
  (src mat)
  (dest mat)
  (colormap :int))