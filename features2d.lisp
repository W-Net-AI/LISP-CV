;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; features2d.lisp
;;;; OpenCV bindings
;;;; 2D Features Framework

(in-package :lisp-cv)



;;;DEFGENERIC's


(defgeneric descriptor-extractor-compute (self image keypoints descriptors)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-COMPUTE methods."))


(defgeneric descriptor-extractor-create (self descriptor-extractor-type)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-CREATE methods."))


(defgeneric descriptor-matcher-create (self descriptor-matcher-type)
  (:documentation "Used for all DESCRIPTOR-MATCHER-CREATE methods."))


(defgeneric descriptor-matcher-match (self query-descriptors train-descriptors matches &optional mask)
  (:documentation "Used for all DESCRIPTOR-MATCHER-MATCH methods.")) 


(defgeneric feature-2d-compute (self image keypoints descriptors)
  (:documentation "Used for all FEATURE-2D-COMPUTE methods."))


(defgeneric feature-2d-create (self name)
  (:documentation "Used for all FEATURE-2D-CREATE methods."))


(defgeneric feature-detector-create (self detector-type)
  (:documentation "Used for all FEATURE-DETECTOR-CREATE methods."))


(defgeneric feature-detector-detect (self image keypoints &optional mask)
  (:documentation "Used for all FEATURE-DETECTOR-DETECT methods."))



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
  (name *string))


;; Feature2D* cv_Feature2D_create1_1(Feature2D* self, String* name) 
(defcfun ("cv_Feature2D_create1_1" %feature-2d-create-brisk) brisk
  (self brisk)
  (name *string))


;; Feature2D* cv_Feature2D_create1_1(Feature2D* self, String* name) 
(defcfun ("cv_Feature2D_create1_1" %feature-2d-create-surf) surf
  (self surf)
  (name *string))


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
  (descriptor-extractor-type *string))


;; DescriptorExtractor* cv_DescriptorExtractor_create1_0(DescriptorExtractor* self, String* descriptorExtractorType) 
(defcfun ("cv_DescriptorExtractor_create1_0" %descriptor-extractor-create-brisk) brisk
  (self brisk)
  (descriptor-extractor-type *string))


;; DescriptorExtractor* cv_DescriptorExtractor_create1_0(DescriptorExtractor* self, String* descriptorExtractorType) 
(defcfun ("cv_DescriptorExtractor_create1_0" %descriptor-extractor-create-surf) surf
  (self surf)
  (descriptor-extractor-type *string))



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
  (descriptor-matcher-type *string))


;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descriptor-matcher-create-brisk) brisk
  (self brisk)
  (descriptor-matcher-type *string))


;; Ptr<DescriptorMatcher> DescriptorMatcher::create(const string& descriptorMatcherType)
;; DescriptorMatcher* cv_DescriptorMatcher_create1_2(DescriptorMatcher* self, String* descriptorMatcherType) 
(defcfun ("cv_DescriptorMatcher_create1_2" %descriptor-matcher-create-surf) surf
  (self surf)
  (descriptor-matcher-type *string))


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



;;; DEFMETHOD'S


(defmethod compute ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-bf-matcher self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-bf-matcher self args))))


(defmethod compute ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-brisk self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-brisk self args))))


(defmethod compute ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-surf self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-surf self args))))


(defmethod create ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-bf-matcher self (%c-string-to-string (first args)
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-bf-matcher self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-bf-matcher self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-bf-matcher self (first args)))))


(defmethod create ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-brisk self (%c-string-to-string (first args) 
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-brisk self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-brisk self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-brisk self (first args)))))


(defmethod create ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-surf self (%c-string-to-string (first args) 
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-surf self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d (first args))
	 (%feature-2d-create-surf self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-surf self (first args)))))


(defmethod descriptor-extractor-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-bf-matcher self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-brisk self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-surf self image keypoints descriptors))


(defmethod descriptor-extractor-create ((self cv-bf-matcher) descriptor-extractor-type)
      (%descriptor-extractor-create-bf-matcher self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-brisk) descriptor-extractor-type)
      (%descriptor-extractor-create-brisk self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-surf) descriptor-extractor-type)
      (%descriptor-extractor-create-surf self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-matcher-create ((self cv-bf-matcher) descriptor-matcher-type)
      (%descriptor-matcher-create-bf-matcher self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-brisk) descriptor-matcher-type)
      (%descriptor-matcher-create-brisk self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-surf) descriptor-matcher-type)
      (%descriptor-matcher-create-surf self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-match ((self cv-bf-matcher) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod descriptor-matcher-match ((self cv-brisk) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod descriptor-matcher-match ((self cv-surf) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod detect ((self cv-bf-matcher) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-brisk) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-surf) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod feature-2d-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-bf-matcher self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-brisk self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-surf self image keypoints descriptors))


(defmethod feature-2d-create ((self cv-bf-matcher) name)
      (%feature-2d-create-bf-matcher self name))


(defmethod feature-2d-create ((self cv-brisk) name)
      (%feature-2d-create-brisk self name))


(defmethod feature-2d-create ((self cv-surf) name)
      (%feature-2d-create-surf self name))


(defmethod feature-detector-create ((self cv-bf-matcher) detector-type)
      (%feature-detector-create-bf-matcher self detector-type))


(defmethod feature-detector-create ((self cv-brisk) detector-type)
      (%feature-detector-create-brisk self detector-type))


(defmethod feature-detector-create ((self cv-surf) detector-type)
      (%feature-detector-create-surf self detector-type))


(defmethod feature-detector-detect ((self cv-bf-matcher) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-bf-matcher self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-brisk) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-brisk self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-surf) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-surf self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod match ((self cv-bf-matcher) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod match ((self cv-brisk) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod match ((self cv-surf) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))

