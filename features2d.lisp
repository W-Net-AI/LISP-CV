;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; features2d.lisp
;;;; OpenCV bindings
;;;; 2D Features Framework

(in-package :lisp-cv)


;;; Feature Detection and Description



;; BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)
;; BRISK* cv_create_BRISK(int thresh, int octaves, float patternScale)
(defcfun ("cv_create_BRISK" %brisk) brisk
  (thresh :int)
  (octaves :int)
  (pattern-scale :float))


(defun brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
  "The BRISK constructor"
   (%brisk thresh octaves pattern-scale))


(defun make-brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
  "The BRISK constructor"
   (%brisk thresh octaves pattern-scale))


;; Feature2D* cv_Feature2D_create1_1(Feature2D* self, String* name) 
(defcfun ("cv_Feature2D_create1_1" %feature-2d-create-bf-matcher) bf-matcher
  (self bf-matcher)
  (name string*))


;; Feature2D* cv_Feature2D_create1_1(Feature2D* self, String* name) 
(defcfun ("cv_Feature2D_create1_1" %feature-2d-create-brisk) brisk
  (self brisk)
  (name string*))


;; Feature2D* cv_Feature2D_create1_1(Feature2D* self, String* name) 
(defcfun ("cv_Feature2D_create1_1" %feature-2d-create-surf) surf
  (self surf)
  (name string*))


;; void Feature2D::compute( InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors ) const
;; void cv_Feature2D_compute3(Feature2D* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) 
(defcfun ("cv_Feature2D_compute3" feature-2d-compute-bf-matcher) :void
  (self bf-matcher)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))


;; void Feature2D::compute( InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors ) const
;; void cv_Feature2D_compute3(Feature2D* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) 
(defcfun ("cv_Feature2D_compute3" feature-2d-compute-brisk) :void
  (self brisk)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))


;; void Feature2D::compute( InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors ) const
;; void cv_Feature2D_compute3(Feature2D* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors)
(defcfun ("cv_Feature2D_compute3" feature-2d-compute-surf) :void
  (self surf)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))



;;; Common Interfaces of Feature Detectors



;; Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)
;; FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType)
(defcfun ("cv_FeatureDetector_create2" %feature-detector-create-bf-matcher) bf-matcher 
  (self bf-matcher)
  (detector-type :string))


;; Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)
;; FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType)
(defcfun ("cv_FeatureDetector_create2" %feature-detector-create-brisk) brisk
  (self brisk)
  (detector-type :string))


;; Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)
;; FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType)
(defcfun ("cv_FeatureDetector_create2" %feature-detector-create-surf) surf 
  (self surf)
  (detector-type :string))


;; void FeatureDetector::detect(const Mat& image, vector<KeyPoint>& keypoints, const Mat& mask=Mat() ) const
;; void cv_FeatureDetector_detect3(FeatureDetector* self, Mat* image, vector_KeyPoint* keypoints, Mat* mask)
(defcfun ("cv_FeatureDetector_detect3" %feature-detector-detect-bf-matcher) :void
  (self bf-matcher)
  (image mat)
  (key-points vector-key-point)
  (mask mat))


;; void FeatureDetector::detect(const Mat& image, vector<KeyPoint>& keypoints, const Mat& mask=Mat() ) const
;; void cv_FeatureDetector_detect3(FeatureDetector* self, Mat* image, vector_KeyPoint* keypoints, Mat* mask)
(defcfun ("cv_FeatureDetector_detect3" %feature-detector-detect-brisk) :void
  (self brisk)
  (image mat)
  (key-points vector-key-point)
  (mask mat))


;; void FeatureDetector::detect(const Mat& image, vector<KeyPoint>& keypoints, const Mat& mask=Mat() ) const
;; void cv_FeatureDetector_detect3(FeatureDetector* self, Mat* image, vector_KeyPoint* keypoints, Mat* mask)
(defcfun ("cv_FeatureDetector_detect3" %feature-detector-detect-surf) :void
  (self surf)
  (image mat)
  (key-points vector-key-point)
  (mask mat))



;;; Common Interfaces of Descriptor Extractors



;; void DescriptorExtractor::compute(InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors) const
;; void cv_DescriptorExtractor_compute(DescriptorExtractor* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) 
(defcfun ("cv_DescriptorExtractor_compute" descriptor-extractor-compute-bf-matcher) :void
  (self bf-matcher)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))


;; void DescriptorExtractor::compute(InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors) const
;; void cv_DescriptorExtractor_compute(DescriptorExtractor* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) 
(defcfun ("cv_DescriptorExtractor_compute" descriptor-extractor-compute-brisk) :void
  (self brisk)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))


;; void DescriptorExtractor::compute(InputArray image, vector<KeyPoint>& keypoints, OutputArray descriptors) const
;; void cv_DescriptorExtractor_compute(DescriptorExtractor* self, Mat* image, vector_KeyPoint* keypoints, Mat* descriptors) 
(defcfun ("cv_DescriptorExtractor_compute" descriptor-extractor-compute-surf) :void
  (self surf)
  (image mat)
  (keypoints vector-key-point)
  (descriptors mat))


;; DescriptorExtractor* cv_DescriptorExtractor_create1_0(DescriptorExtractor* self, String* descriptorExtractorType) 
(defcfun ("cv_DescriptorExtractor_create1_0" %descriptor-extractor-create-bf-matcher) bf-matcher
  (self bf-matcher)
  (descriptor-extractor-type string*))


;; DescriptorExtractor* cv_DescriptorExtractor_create1_0(DescriptorExtractor* self, String* descriptorExtractorType) 
(defcfun ("cv_DescriptorExtractor_create1_0" %descriptor-extractor-create-brisk) brisk
  (self brisk)
  (descriptor-extractor-type string*))


;; DescriptorExtractor* cv_DescriptorExtractor_create1_0(DescriptorExtractor* self, String* descriptorExtractorType) 
(defcfun ("cv_DescriptorExtractor_create1_0" %descriptor-extractor-create-surf) surf
  (self surf)
  (descriptor-extractor-type string*))



;;; Common Interfaces of Descriptor Matchers



;; BFMatcher::BFMatcher(int normType=NORM_L2, bool crossCheck=false )
;; BFMatcher* cv_create_BFMatcher(int normType, bool crossCheck) 
(defcfun ("cv_create_BFMatcher" %bf-matcher) bf-matcher
  (norm-type :int)
  (cross-check :boolean))


(defun bf-matcher (&optional (norm-type +norm-l2+) (cross-check nil))
  "Brute-force matcher constructor."
   (%bf-matcher norm-type cross-check))


(defun make-bf-matcher (&optional (norm-type +norm-l2+) (cross-check nil))
  "Brute-force matcher constructor."
   (%bf-matcher norm-type cross-check))


;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descriptor-matcher-create-bf-matcher) bf-matcher
  (self bf-matcher)
  (descriptor-matcher-type string*))


;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descriptor-matcher-create-brisk) brisk
  (self brisk)
  (descriptor-matcher-type string*))


;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descriptor-matcher-create-surf) surf
  (self surf)
  (descriptor-matcher-type string*))


;; void DescriptorMatcher::match(const Mat& queryDescriptors, const Mat& trainDescriptors, vector<DMatch>& matches, 
;; const Mat& mask=Mat() ) const
;; void cv_DescriptorMatcher_match(DescriptorMatcher* self, Mat* queryDescriptors, Mat* trainDescriptors, vector_DMatch* matches, 
;; Mat* mask)
(defcfun ("cv_DescriptorMatcher_match" %descriptor-matcher-match-bf-matcher) :void
  (self bf-matcher)
  (query-descriptors mat)
  (train-descriptors mat)
  (matches vector-dmatch)
  (mask mat))


;; void DescriptorMatcher::match(const Mat& queryDescriptors, const Mat& trainDescriptors, vector<DMatch>& matches, 
;; const Mat& mask=Mat() ) const
;; void cv_DescriptorMatcher_match(DescriptorMatcher* self, Mat* queryDescriptors, Mat* trainDescriptors, vector_DMatch* matches, 
;; Mat* mask)
(defcfun ("cv_DescriptorMatcher_match" %descriptor-matcher-match-brisk) :void
  (self brisk)
  (query-descriptors mat)
  (train-descriptors mat)
  (matches vector-dmatch)
  (mask mat))


;; void DescriptorMatcher::match(const Mat& queryDescriptors, const Mat& trainDescriptors, vector<DMatch>& matches, 
;; const Mat& mask=Mat() ) const
;; void cv_DescriptorMatcher_match(DescriptorMatcher* self, Mat* queryDescriptors, Mat* trainDescriptors, vector_DMatch* matches, 
;; Mat* mask)
(defcfun ("cv_DescriptorMatcher_match" %descriptor-matcher-match-surf) :void
  (self surf)
  (query-descriptors mat)
  (train-descriptors mat)
  (matches vector-dmatch)
  (mask mat))



;;; Drawing Function of Keypoints and Matches



;; void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, 
;; const vector<DMatch>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1),
;; const vector<char>& matchesMask=vector<char>(), int flags=DrawMatchesFlags::DEFAULT)

;; void cv_drawMatches(Mat* img1, vector_KeyPoint* keypoints1, Mat* img2, vector_KeyPoint* keypoints2, vector_DMatch* matches1to2, 
;; Mat* outImg, Scalar* matchColor, Scalar* singlePointColor, vector_char* matchesMask, int flags) 

(defcfun ("cv_drawMatches" %draw-matches) :void
  (img1 mat)
  (keypoints1 vector-key-point)
  (img2 mat)
  (keypoints2 vector-key-point)
  (matches1to2 vector-dmatch)
  (out-img mat)
  (match-color scalar)
  (single-point-color scalar)
  (matches-mask vector-char)
  (flags :int))


(defun draw-matches (img1 keypoints1 img2 keypoints2 matches1to2 out-img &optional 
									  (match-color (scalar-all -1) given-match-color) 
									  (single-point-color (scalar-all -1) given-single-point-color) 
									  (matches-mask (make-vector-char) given-matches-mask) 
									  (flags +default+))
  "Draws the found matches of keypoints from two images."
  (%draw-matches img1 keypoints1 img2 keypoints2 matches1to2 out-img match-color single-point-color matches-mask flags)
  (if given-match-color nil (del-scalar match-color)) 
  (if given-single-point-color nil (del-scalar single-point-color)) 
  (if given-matches-mask nil (del-vector-char matches-mask)))



