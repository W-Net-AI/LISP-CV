;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; objdetect.lisp
;;;; OpenCV bindings
;;;; Object Detection


(in-package :lisp-cv)


;;; Cascade Classification


;; CascadeClassifier::CascadeClassifier()
;; CascadeClassifier* cv_create_CascadeClassifier() 
(defcfun ("cv_create_CascadeClassifier" cascade-classifier-0) cascade-classifier
  "CASCADE-CLASSIFIER construct.")


;; CascadeClassifier::CascadeClassifier(const string& filename)
;; CascadeClassifier* cv_create_CascadeClassifier1(String* filename)
(defcfun ("cv_create_CascadeClassifier1" cascade-classifier-1) cascade-classifier
  "Loads a classifier from a file."
  (filename string*))


(defun cascade-classifier (&optional filename)
  (cond ((eq filename nil)
	 (cascade-classifier-0))
	(filename
	 (cascade-classifier-1 (%c-string-to-string filename (length filename))))
	(t nil)))


(defun make-cascade-classifier (&optional filename)
  (cond ((eq filename nil)
	 (cascade-classifier-0))
	(filename
	 (cascade-classifier-1 (%c-string-to-string filename (length filename))))
	(t nil)))


;; bool CascadeClassifier::load(const string& filename)
;; bool cv_CascadeClassifier_load1(CascadeClassifier* self, String* filename) 
(defcfun ("cv_CascadeClassifier_load1" %cascade-classifier-load) :boolean
  "Loads a classifier from a file."
  (self cascade-classifier)
  (filename string*))


(defun cascade-classifier-load (self filename)
  "Loads a classifier from a file."
  (%cascade-classifier-load self (%c-string-to-string filename (length filename))))


;; void CascadeClassifier::detectMultiScale(const Mat& image, vector<Rect>& objects, double scaleFactor=1.1, int minNeighbors=3, 
;; int flags=0, Size minSize=Size(), Size maxSize=Size())

;; void cv_CascadeClassifier_detectMultiScale(CascadeClassifier* self, Mat* image, vector_Rect* objects, double scaleFactor, 
;; int minNeighbors, int flags, Size* minSize, Size* maxSize)


(defcfun ("cv_CascadeClassifier_detectMultiScale" %%detect-multi-scale) :void
  (self cascade-classifier)
  (image mat)
  (objects vector-rect)
  (scale-factor :double)
  (min-neighbors :int)
  (flags :int)
  (min-size size)
  (max-size size))


(defun %detect-multi-scale (self image objects &optional (scale-factor 1.1d0) (min-neighbors 3) (flags 0) (min-size (size-0) given-min-size) (max-size (size-0) given-max-size))
  "Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles."
  (%%detect-multi-scale self image objects scale-factor min-neighbors flags min-size max-size)
  (if given-min-size nil (del-size min-size)) (if given-max-size nil (del-size max-size)))


;; void CascadeClassifier::detectMultiScale(InputArray image, vector<Rect>& objects, vector<int>& numDetections, 
;; double scaleFactor=1.1, int minNeighbors=3, int flags=0, Size minSize=Size(), Size maxSize=Size())

;; void cv_CascadeClassifier_detectMultiScale8(CascadeClassifier* self, Mat* image, vector_Rect* objects, 
;; vector_int* numDetections, double scaleFactor, int minNeighbors, int flags, Size* minSize, Size* maxSize)


(defcfun ("cv_CascadeClassifier_detectMultiScale8" %%detect-multi-scale8) :void
  (self cascade-classifier)
  (image mat)
  (objects vector-rect)
  (num-detections vector-int)
  (scale-factor :double)
  (min-neighbors :int)
  (flags :int)
  (min-size size)
  (max-size size))


(defun %detect-multi-scale8 (self image objects num-detections &optional (scale-factor 1.1d0) (min-neighbors 3) (flags 0) (min-size (size-0) given-min-size) (max-size (size-0) given-max-size))
  "Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles."
  (%%detect-multi-scale8 self image objects num-detections scale-factor min-neighbors flags min-size max-size) 
  (if given-min-size nil (del-size min-size)) (if given-max-size nil (del-size max-size)))

 
;; void CascadeClassifier::detectMultiScale( const Mat& image, vector<Rect>& objects, std::vector<int>& rejectLevels, 
;; vector<double>& levelWeights, double scaleFactor = 1.1, int minNeighbors = 3, int flags = 0, Size minSize = Size(),  
;; Size maxSize = Size(), bool outputRejectLevels = false )

;; void cv_CascadeClassifier_detectMultiScale10(CascadeClassifier* self, Mat* image, vector_Rect* objects, 
;; vector_int* rejectLevels, vector_double* levelWeights, double scaleFactor, int minNeighbors, int flags, 
;; Size* minSize, Size* maxSize, bool outputRejectLevels) 

(defcfun ("cv_CascadeClassifier_detectMultiScale" %%detect-multi-scale10) :void
  (self cascade-classifier)
  (image mat)
  (objects vector-rect)
  (reject-levels vector-int)
  (level-weights vector-double)
  (scale-factor :double)
  (min-neighbors :int)
  (flags :int)
  (min-size size)
  (max-size size)
  (output-reject-levels :boolean))


(defun %detect-multi-scale10 (self image objects reject-levels level-weights &optional (scale-factor 1.1d0) (min-neighbors 3) (flags 0) (min-size (size-0) given-min-size) (max-size (size-0) given-max-size) (output-reject-levels nil))
  "Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles."
			       (%%detect-multi-scale10 self image objects reject-levels level-weights scale-factor min-neighbors flags min-size 
                                           max-size output-reject-levels)
                                  (if given-min-size nil (del-size min-size))
                                  (if given-max-size nil (del-size max-size)))


(defun detect-multi-scale (&rest args)

  (cond  ((typep (fifth args) 'std-vector-double)
	  (apply #'%detect-multi-scale10 args))

	 ((typep (fourth args) 'std-vector-int)
	  (apply #'%detect-multi-scale8 args))

	 ((typep (third args) 'std-vector-rect)
	  (apply #'%detect-multi-scale args))

	 (t nil)))


;; void groupRectangles(vector<Rect>& rectList, int groupThreshold, double eps=0.2)
;; void cv_groupRectangles_3(vector_Rect* rectList, int groupThreshold, double eps)
(defcfun ("cv_groupRectangles_3" %group-rectangles) :void
  (rect-list vector-rect)
  (group-threshold :int)
  (eps :double))


(defun group-rectangles (rect-list group-threshold &optional (eps 0.2d0))
  (%group-rectangles rect-list group-threshold eps))

