;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; features2d.lisp
;;;; OpenCV bindings
;;;; 2D Features Framework

(in-package :lisp-cv)


;;; Feature Detection and Description


;; BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)
;; BRISK* cv_create_BRISK(int thresh, int octaves, float patternScale)
(defcfun ("cv_create_BRISK" %brisk) (:pointer brisk)
  (thresh :int)
  (octaves :int)
  (pattern-scale :float))


(defun brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
  "The BRISK constructor"
   (%brisk thresh octaves pattern-scale))


;;; Common Interfaces of Feature Detectors


;; Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)
;; FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, String* detectorType) 
(defcfun ("cv_FeatureDetector_create2" %feat-detector-create) (:pointer feature-detector) 
  (self (:pointer feature-detector))
  (detector-type :string))

(defun feat-detector-create (self detector-type)
  "Creates a feature detector by its name."
   (%feat-detector-create self detector-type))


;; void FeatureDetector::detect(const Mat& image, vector<KeyPoint>& keypoints, const Mat& mask=Mat() ) const
;; void cv_FeatureDetector_detect3(FeatureDetector* self, Mat* image, vector_KeyPoint* keypoints, Mat* mask)
(defcfun ("cv_FeatureDetector_detect3" %feat-detector-detect) :void
  (self (:pointer feature-detector))
  (image (:pointer mat))
  (keypoints (:pointer vector-keypoint))
  (mask (:pointer mat)))

(defun feat-detector-detect (self image keypoints &optional (mask (mat)))
  "Detects keypoints in an image."
   (%feat-detector-detect self image keypoints mask))


;; KeyPoint::KeyPoint()
;; KeyPoint* cv_create_KeyPoint()
(defcfun ("cv_create_KeyPoint" keypoint0) (:pointer keypoint)
  "Keypoint constructor")

;; KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)
;; KeyPoint* cv_create_KeyPoint7(float x, float y, float _size, float _angle, float _response, int _octave, int _class_id)
(defcfun ("cv_create_KeyPoint7" keypoint7) (:pointer keypoint)
  "Keypoint constructor"
  (x :float)
  (y :float)
  (_size :float)
  (_angle :float)
  (_response :float)
  (_octave :int)
  (_class_id :int))

(defun keypoint (&optional x y _size (_angle -1) (_response 0) (_octave 0) (_class_id -1))
	   (cond ((eq x nil)
		  (keypoint0))
		 (x
		  (keypoint7 x y _size _angle _response _octave _class_id))
		 (t nil)))



;;; Common Interfaces of Descriptor Extractors


;; void DescriptorExtractor::compute(const Mat& image, vector<KeyPoint>& keypoints, Mat& descriptors) const
;; void cv_Feature2D_compute3(Feature2D* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) {
(defcfun ("cv_Feature2D_compute3" feat-2d-compute) (:pointer feature-2d)
  "Computes the descriptors for a set of keypoints detected in an image."
  (self (:pointer feature-2d))
  (image (:pointer mat))
  (keypoints (:pointer vector-keypoint))
  (descriptors (:pointer mat)))


;;; Common Interfaces of Descriptor Matchers


;; BFMatcher::BFMatcher(int normType=NORM_L2, bool crossCheck=false )
;; BFMatcher* cv_create_BFMatcher(int normType, bool crossCheck) 
(defcfun ("cv_create_BFMatcher" %bf-matcher) (:pointer bf-matcher)
  (norm-type :int)
  (cross-check :boolean))

(defun bf-matcher (&optional (norm-type +norm-l2+) (cross-check nil))
  "Brute-force matcher constructor."
   (%bf-matcher norm-type cross-check))

;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descrip-matcher-create) (:pointer descriptor-matcher)
  (self (:pointer descriptor-matcher))
  (descriptor-matcher-type (:pointer string*)))

(defun descrip-matcher-create (self descriptor-matcher-type)
  "Creates a descriptor matcher of a given type with the default parameters (using default constructor)."
   (%descrip-matcher-create self (foreign-alloc :string :initial-element descriptor-matcher-type)))


;; void DescriptorMatcher::match(const Mat& queryDescriptors, const Mat& trainDescriptors, 
;; vector<DMatch>& matches, const Mat& mask=Mat() ) const
;;void cv_DescriptorMatcher_match(DescriptorMatcher* self, Mat* queryDescriptors, Mat* trainDescriptors, vector_DMatch* matches, Mat* mask)
(defcfun ("cv_DescriptorMatcher_match" %descrip-matcher-match) :void
  (self (:pointer descriptor-matcher))
  (query-descriptors (:pointer mat))
  (train-descriptors (:pointer mat))
  (matches (:pointer vector-dmatch))
  (mask (:pointer mat)))

(defun descrip-matcher-match (self query-descriptors train-descriptors matches &optional (mask (mat)))
  "Finds the best match for each descriptor from a query set."
   (%descrip-matcher-match self query-descriptors train-descriptors matches mask))



;;; Drawing Function of Keypoints and Matches


;; void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, 
;; const vector<DMatch>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1),
;; const vector<char>& matchesMask=vector<char>(), int flags=DrawMatchesFlags::DEFAULT)

;; void cv_drawMatches(Mat* img1, vector_KeyPoint* keypoints1, Mat* img2, vector_KeyPoint* keypoints2, vector_DMatch* matches1to2, 
;; Mat* outImg, Scalar* matchColor, Scalar* singlePointColor, vector_char* matchesMask, int flags) 

(defcfun ("cv_drawMatches" %draw-matches) :void
  (img1 (:pointer mat))
  (keypoints1 (:pointer vector-keypoint))
  (img2 (:pointer mat))
  (keypoints2 (:pointer vector-keypoint))
  (matches1to2 (:pointer vector-dmatch))
  (outimg (:pointer mat))
  (match-color (:pointer scalar))
  (single-point-color (:pointer scalar))
  (matches-mask (:pointer vector-char))
  (flags :int))

(defun draw-matches (img1 keypoints1 img2 keypoints2 matches1to2 outimg &optional (match-color (scalar-all -1)) (single-point-color (scalar-all -1)) (matches-mask (vector-char)) (flags +default+))
  "Draws the found matches of keypoints from two images."
   (%draw-matches img1 keypoints1 img2 keypoints2 matches1to2 outimg match-color single-point-color matches-mask flags))















