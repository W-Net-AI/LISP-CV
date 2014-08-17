;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; calib3d.lisp
;;;; OpenCV bindings
;;;; Camera Calibration and 3D Reconstruction


(in-package :lisp-cv)



;;;; Camera Calibration and 3D Reconstruction



;; Mat findHomography(InputArray srcPoints, InputArray dstPoints, int method=0, double ransacReprojThreshold=3, 
;;                    OutputArray mask=noArray() )
;; Mat* cv_findHomography(Mat* srcPoints, Mat* dstPoints, int method, double ransacReprojThreshold, Mat* mask)
(defcfun ("cv_findHomography" %find-homography) mat
  (src-points mat)
  (dest-points mat)
  (method :int)
  (ransac-reproj-threshold :double)
  (mask mat))


(defun find-homography (src-points dest-points &optional (method 0) (ransac-reproj-threshold 3d0) (mask (%mat) given-mask))
  (let ((ret (%find-homography src-points dest-points method ransac-reproj-threshold mask)))
    (if given-mask nil (del-mat mask))
    ret))
