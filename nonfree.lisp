;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; nonfree.lisp
;;;; OpenCV bindings
;;;; Non-free functionality

(in-package :lisp-cv)


;;; Feature Detection and Description


;; SURF::SURF()
;; SURF* cv_create_SURF() 
(defcfun ("cv_create_SURF" surf-0) surf)


;; SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )
;; SURF* cv_create_SURF5(double hessianThreshold, int nOctaves, int nOctaveLayers, bool extended, bool upright)
(defcfun ("cv_create_SURF5" surf-5) surf
  (hessian-threshold :double)
  (n-octaves :int)
  (n-octave-layers :int)
  (extended :boolean)
  (upright :boolean))


(defun surf (&optional hessian-threshold (n-octaves 4) (n-octave-layers 2) (extended t) (upright nil))
	   (cond ((eq hessian-threshold nil)
		  (surf-0))
		 (hessian-threshold
		  (surf-5 hessian-threshold n-octaves n-octave-layers extended upright))
		 (t nil)))


(defun make-surf (&optional hessian-threshold (n-octaves 4) (n-octave-layers 2) (extended t) (upright nil))
	   (cond ((eq hessian-threshold nil)
		  (surf-0))
		 (hessian-threshold
		  (surf-5 hessian-threshold n-octaves n-octave-layers extended upright))
		 (t nil)))
