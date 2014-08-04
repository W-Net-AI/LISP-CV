;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; predicates.lisp
;;;; OpenCV bindings
;;;; Predicate functions

(in-package :lisp-cv)


(defun string*-p (object)
  (typep object 'std-string))

(defun vector-char-p (object)
  (typep object 'std-vector-char))

(defun vector-dmatch-p (object)
  (typep object 'std-vector-dmatch))

(defun vector-double-p (object)
  (typep object 'std-vector-double))

(defun vector-float-p (object)
  (typep object 'std-vector-float))

(defun vector-int-p (object)
  (typep object 'std-vector-int))

(defun vector-key-point-p (object)
  (typep object 'std-vector-key-point))

(defun vector-mat-p (object)
  (typep object 'std-vector-mat))

(defun vector-point-p (object)
  (typep object 'std-vector-point))

(defun vector-point-2f-p (object)
  (typep object 'std-vector-point-2f))

(defun vector-rect-p (object)
  (typep object 'std-vector-rect))

(defun vector-uchar-p (object)
  (typep object 'std-vector-uchar))

(defun vector-vec-2b-p (object)
  (typep object 'std-vector-vec-2b))

(defun vector-vec-3b-p (object)
  (typep object 'std-vector-vec-3b))

(defun vector-vec-4b-p (object)
  (typep object 'std-vector-vec-4b))

(defun vector-vec-2d-p (object)
  (typep object 'std-vector-vec-2d))

(defun vector-vec-3d-p (object)
  (typep object 'std-vector-vec-3d))

(defun vector-vec-4d-p (object)
  (typep object 'std-vector-vec-4d))

(defun vector-vec-6d-p (object)
  (typep object 'std-vector-vec-6d))

(defun vector-vec-2f-p (object)
  (typep object 'std-vector-vec-2f))

(defun vector-vec-3f-p (object)
  (typep object 'std-vector-vec-3f))

(defun vector-vec-4f-p (object)
  (typep object 'std-vector-vec-4f))

(defun vector-vec-6f-p (object)
  (typep object 'std-vector-vec-6f))

(defun vector-vec-2i-p (object)
  (typep object 'std-vector-vec-2i))

(defun vector-vec-3i-p (object)
  (typep object 'std-vector-vec-3i))

(defun vector-vec-4i-p (object)
  (typep object 'std-vector-vec-4i))

(defun vector-vec-6i-p (object)
  (typep object 'std-vector-vec-6i))

(defun vector-vec-8i-p (object)
  (typep object 'std-vector-vec-8i))

(defun vector-vec-2s-p (object)
  (typep object 'std-vector-vec-2s))

(defun vector-vec-3s-p (object)
  (typep object 'std-vector-vec-3s))

(defun vector-vec-4s-p (object)
  (typep object 'std-vector-vec-4s))

(defun vector-vec-2w-p (object)
  (typep object 'std-vector-vec-2w))

(defun vector-vec-3w-p (object)
  (typep object 'std-vector-vec-3w))

(defun vector-vec-4w-p (object)
  (typep object 'std-vector-vec-4w))

(defun cv-mat-p (object)
  (typep object 'cv-cv-mat))

(defun dmatch-p (object)
  (typep object 'cv-dmatch))

(defun file-node-p (object)
  (typep object 'cv-file-node))

(defun file-storage-p (object)
  (typep object 'cv-file-storage))

(defun key-point-p (object)
  (typep object 'cv-key-point))

(defun mat-p (object)
  (typep object 'cv-mat))

(defun matrix-expressions-p (object)
  (typep object 'matrix-expressions))

(defun point-p (object)
  (typep object 'cv-point))

(defun point-2d-p (object)
  (typep object 'cv-point-2d))

(defun point-2f-p (object)
  (typep object 'cv-point-2f))

(defun point-3d-p (object)
  (typep object 'cv-point-3d))

(defun point-3f-p (object)
  (typep object 'cv-point-3f))

(defun point-3i-p (object)
  (typep object 'cv-point-3i))

(defun range-p (object)
  (typep object 'cv-range))

(defun rect-p (object)
  (typep object 'cv-rect))

(defun rng-p (object)
  (typep object 'cv-rng))

(defun rotated-rect-p (object)
  (typep object 'cv-rotated-rect))

(defun scalar-p (object)
  (typep object 'cv-scalar))

(defun size-p (object)
  (typep object 'cv-size))

(defun term-criteria-p (object)
  (typep object 'cv-term-criteria))

(defun vec-2b-p (object)
  (typep object 'cv-vec-2b))

(defun vec-3b-p (object)
  (typep object 'cv-vec-3b))

(defun vec-4b-p (object)
  (typep object 'cv-vec-4b))

(defun vec-2d-p (object)
  (typep object 'cv-vec-2d))

(defun vec-3d-p (object)
  (typep object 'cv-vec-3d))

(defun vec-4d-p (object)
  (typep object 'cv-vec-4d))

(defun vec-2f-p (object)
  (typep object 'cv-vec-2f))

(defun vec-3f-p (object)
  (typep object 'cv-vec-3f))

(defun vec-4f-p (object)
  (typep object 'cv-vec-4f))

(defun vec-6d-p (object)
  (typep object 'cv-vec-6d))

(defun vec-6f-p (object)
  (typep object 'cv-vec-6f))

(defun vec-2i-p (object)
  (typep object 'cv-vec-2i))

(defun vec-3i-p (object)
  (typep object 'cv-vec-3i))

(defun vec-4i-p (object)
  (typep object 'cv-vec-4i))

(defun vec-6i-p (object)
  (typep object 'cv-vec-6i))

(defun vec-8i-p (object)
  (typep object 'cv-vec-8i))

(defun vec-2s-p (object)
  (typep object 'cv-vec-2s))

(defun vec-3s-p (object)
  (typep object 'cv-vec-3s))

(defun vec-4s-p (object)
  (typep object 'cv-vec-4s))

(defun vec-2w-p (object)
  (typep object 'cv-vec-2w))

(defun vec-3w-p (object)
  (typep object 'cv-vec-3w))

(defun vec-4w-p (object)
  (typep object 'cv-vec-4w))

(defun video-capture-p (object)
  (typep object 'cv-video-capture))

(defun video-writer-p (object)
  (typep object 'cv-video-writer))

(defun background-subtractor-mog-2-p (object)
  (typep object 'cv-background-subtractor-mog-2))

(defun bf-matcher-p (object)
  (typep object 'cv-bf-matcher))

(defun brisk-p (object)
  (typep object 'cv-brisk))

(defun surf-p (object)
  (typep object 'cv-surf))

(defun cascade-classifier-p (object)
  (typep object 'cv-cascade-classifier))

(defun hog-descriptor-p (object)
  (typep object 'cv-hog-descriptor))

(defun ann-mlp-p (object)
  (typep object 'cv-ann-mlp))

(defun ann-mlp-train-params-p (object)
  (typep object 'cv-ann-mlp-train-params))

(defun d-tree-p (object)
  (typep object 'cv-d-tree))

(defun d-tree-params-p (object)
  (typep object 'cv-d-tree-params))

(defun k-nearest-p (object)
  (typep object 'cv-k-nearest))

(defun normal-bayes-classifier-p (object)
  (typep object 'cv-normal-bayes-classifier))

(defun svm-p (object)
  (typep object 'cv-svm))

(defun svm-params-p (object)
  (typep object 'cv-svm-params))
