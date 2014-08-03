;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; video.lisp
;;;; OpenCV bindings
;;;; Video Analysis

(in-package :lisp-cv)

;;; Motion Analysis and Object Tracking

;; void calcOpticalFlowPyrLK(InputArray prevImg, InputArray nextImg, InputArray prevPts, InputOutputArray nextPts, 
;;                           OutputArray status, OutputArray err, Size winSize=Size(21,21), int maxLevel=3, 
;;                           TermCriteria criteria=TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01), 
;;                           int flags=0, double minEigThreshold=1e-4 )
;; void cv_calcOpticalFlowPyrLK(Mat* prevImg, Mat* nextImg, Mat* prevPts, Mat* nextPts, Mat* status, Mat* err, Size* winSize, 
;;                              int maxLevel, TermCriteria* criteria, int flags, double minEigThreshold)
(defcfun ("cv_calcOpticalFlowPyrLK" %calc-optical-flow-pyr-lk) :void
  (prev-img mat)
  (next-img mat)
  (prev-pts mat)
  (next-pts mat)
  (status mat)
  (err mat)
  (win-size size)
  (max-level :int)
  (criteria term-criteria)
  (flags :int)
  (min-eigen-threshold :double))


(defun calc-optical-flow-pyr-lk (prev-img next-img prev-pts next-pts status err &optional (win-size (size-2 21 21) given-win-size) (max-level 3) (criteria (term-criteria-3 (+ +term-criteria-count+ +term-criteria-eps+) 30 0.01d0) given-criteria) (flags 0)
										  (min-eigen-threshold 1.d-4))
  (%calc-optical-flow-pyr-lk prev-img next-img prev-pts next-pts status err win-size max-level criteria flags 
			     min-eigen-threshold)
  (if given-win-size nil (del-size win-size))
  (if given-criteria nil (del-term-crit criteria)))
