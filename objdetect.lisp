;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; objdetect.lisp
;;;; OpenCV bindings
;;;; Object Detection


(in-package :lisp-cv)


;;; Cascade Classification


;; CascadeClassifier::CascadeClassifier()
;; CascadeClassifier* cv_create_CascadeClassifier() 
(defcfun ("cv_create_CascadeClassifier" cascade-classifier0) cascade-classifier
  "CASCADE-CLASSIFIER construct.")

;; CascadeClassifier::CascadeClassifier(const string& filename)
;; CascadeClassifier* cv_create_CascadeClassifier1(String* filename)
(defcfun ("cv_create_CascadeClassifier1" cascade-classifier1) cascade-classifier
  "Loads a classifier from a file."
  (filename *string))

(defun cascade-classifier (&optional filename)
  (cond ((eq filename nil)
	 (cascade-classifier0))
	(filename
	 (cascade-classifier1 (c-string-to-string filename (length filename))))
	(t nil)))


;; bool CascadeClassifier::load(const string& filename)
;; bool cv_CascadeClassifier_load1(CascadeClassifier* self, String* filename) 
(defcfun ("cv_CascadeClassifier_load1" %cascade-classifier-load) :boolean
  "Loads a classifier from a file."
  (self cascade-classifier)
  (filename *string))

(defun cascade-classifier-load (self filename)
  "Loads a classifier from a file."
  (%cascade-classifier-load self (c-string-to-string filename (length filename))))