;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; ml.lisp
;;;; OpenCV bindings
;;;; Machine Learning

(in-package :lisp-cv)


;;; Normal Bayes Classifier



;; CvNormalBayesClassifier::CvNormalBayesClassifier()
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier()
(defcfun ("cv_create_CvNormalBayesClassifier" %normal-bayes-classifier) normal-bayes-classifier)

;; CvNormalBayesClassifier::CvNormalBayesClassifier(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), 
;;                                                  const Mat& sampleIdx=Mat() )
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier4(Mat* trainData, Mat* responses, Mat* varIdx, Mat* sampleIdx) 
(defcfun ("cv_create_CvNormalBayesClassifier4" normal-bayes-classifier4) normal-bayes-classifier
  (train-data mat)
  (responses mat)
  (var-idx mat)
  (sample-idx mat))

(defun normal-bayes-classifier (&optional train-data responses  (var-idx (mat) given-var-idx) (sample-idx (mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier4 train-data responses  var-idx sample-idx)
		    (%normal-bayes-classifier))))
    (if given-var-idx nil (del-mat var-idx))
    (if given-sample-idx nil (del-mat sample-idx)) 
    return))


;; float CvNormalBayesClassifier::predict(const Mat& samples, Mat* results=0, Mat* results_prob=0 ) const
;; float cv_CvNormalBayesClassifier_predict3(CvNormalBayesClassifier* self, Mat* samples, Mat* results, Mat* results_prob) 
(defcfun ("cv_CvNormalBayesClassifier_predict3" %normal-bayes-classifier-predict) :float
  (self normal-bayes-classifier)
  (samples mat)
  (results mat)
  (results-prob mat))

(defun normal-bayes-classifier-predict (self samples &optional (results (null-pointer)) (results-prob (null-pointer)))
  (%normal-bayes-classifier-predict self samples results results-prob))



;;; K-nearest Neighbors


;; CvKNearest::CvKNearest()
;; CvKNearest* cv_create_CvKNearest() 
(defcfun ("cv_create_CvKNearest" %k-nearest) k-nearest)

;; CvKNearest::CvKNearest(const CvMat* trainData, const CvMat* responses, const CvMat* sampleIdx=0, bool isRegression=false, 
;;                        int max_k=32 )
;; CvKNearest* cv_create_CvKNearest5(Mat* trainData, Mat* responses, Mat* sampleIdx, bool isRegression, int max_k) 
(defcfun ("cv_create_CvKNearest5" k-nearest5) k-nearest
  (train-data mat)
  (responses mat)
  (sample-idx mat)
  (is-regression :boolean)
  (max-k :int))

(defun k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest5 train-data responses sample-idx is-regression max-k)
      (%k-nearest)))


;; float CvKNearest::find_nearest(const Mat& samples, int k, Mat& results, Mat& neighborResponses, Mat& dists) const
;; float cv_CvKNearest_find_nearest(CvKNearest* self, Mat* samples, int k, Mat* results, Mat* neighborResponses, Mat* dists) 
(defcfun ("cv_CvKNearest_find_nearest" %k-nearest-find-nearest) :float
  (self k-nearest)
  (samples mat)
  (k :int)
  (results mat)
  (neighbor-responses mat)
  (dists mat))

(defun k-nearest-find-nearest (self samples k &optional (results (null-pointer)) (neighbor-responses (null-pointer)) 
                              (dists (null-pointer)))
  (%k-nearest-find-nearest self samples k results neighbor-responses dists))


;;; Decision Trees


;; CvDTree:CvDTree()
;; CvDTree* cv_create_CvDTree() 
(defcfun ("cv_create_CvDTree" d-tree) d-tree)


;; CvDTreeParams::CvDTreeParams()
;; CvDTreeParams* cv_create_CvDTreeParams()
(defcfun ("cv_create_CvDTreeParams" %d-tree-params) d-tree-params)

;; CvDTreeParams::CvDTreeParams(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                              int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors)
;; CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                                         int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, 
;;                                         const float* priors)
(defcfun ("cv_create_CvDTreeParams9" d-tree-params9) d-tree-params
  (max-depth :int)
  (min-sample-count :int)
  (regression-accuracy :float)
  (use-surrogates :boolean)
  (max-categories :int)
  (folds :int)
  (use-1se-rule :boolean)
  (truncate-pruned-tree :boolean)
  (priors :pointer))

(defun d-tree-params (&optional max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
			truncate-pruned-tree priors)
  (if max-depth
      (d-tree-params9 max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
                      truncate-pruned-tree priors)
      (%d-tree-params)))


;; CvDTreeNode* CvDTree::predict(const Mat& sample, const Mat& missingDataMask=Mat(), bool preprocessedInput=false ) const
;; CvDTreeNode* cv_CvDTree_predict3_0(CvDTree* self, Mat* sample, Mat* missingDataMask, bool preprocessedInput) 
(defcfun ("cv_CvDTree_predict3_0" %d-tree-predict) (:pointer (:struct d-tree-node))
  (self d-tree)
  (sample mat)
  (missing-data-mask mat)
  (preprocessed-input :boolean))

(defun d-tree-predict (self sample &optional (missing-data-mask (mat) given-missing-data-mask) (preprocessed-input nil))
  (let ((return 
	  (%d-tree-predict self sample missing-data-mask preprocessed-input)))
    (if given-missing-data-mask nil (del-mat missing-data-mask))
    return))


;; bool CvDTree::train(const Mat& trainData, int tflag, const Mat& responses, const Mat& varIdx=Mat(), const Mat& sampleIdx=Mat(), 
;;                     const Mat& varType=Mat(), const Mat& missingDataMask=Mat(), CvDTreeParams params=CvDTreeParams() )
;; bool cv_CvDTree_train8(CvDTree* self, Mat* trainData, int tflag, Mat* responses, Mat* varIdx, Mat* sampleIdx, Mat* varType,
;;                        Mat* missingDataMask, CvDTreeParams* params)
(defcfun ("cv_CvDTree_train8" %d-tree-train) :boolean
  (self d-tree)
  (train-data mat)
  (tflag :int)
  (responses mat)
  (var-idx mat)
  (sample-idx mat)
  (var-type mat)
  (missing-data-mask mat)
  (params d-tree-params))

(defun d-tree-train (self train-data tflag responses &optional (var-idx (mat) given-var-idx) (sample-idx (mat) given-sample-idx) 
						       (var-type (mat) given-var-type) (missing-data-mask (mat) given-missing-data-mask) 
						       (params (d-tree-params) given-params))
  (let ((return 
	  (%d-tree-train self train-data tflag responses var-idx sample-idx var-type missing-data-mask params)))
    (if given-var-idx nil (del-mat var-idx))
    (if given-sample-idx nil (del-mat sample-idx))
    (if given-var-type nil (del-mat var-type))
    (if given-missing-data-mask nil (del-mat missing-data-mask))
    (if given-params nil (del-d-tree-params params))
    return))


;;; Neural Networks


;; CvANN_MLP::CvANN_MLP()
;; CvANN_MLP* cv_create_CvANN_MLP()
(defcfun ("cv_create_CvANN_MLP" %ann-mlp) ann-mlp)

;; CvANN_MLP::CvANN_MLP(const CvMat* layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )
;; CvANN_MLP* cv_create_CvANN_MLP4(Mat* layerSizes, int activateFunc, double fparam1, double fparam2) 
(defcfun ("cv_create_CvANN_MLP4" ann-mlp4) ann-mlp
  (layer-sizes mat)
  (activate-func :int)
  (fparam1 :double)
  (fparam2 :double))

(defun ann-mlp (&optional layer-sizes (activate-func +ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp4 layer-sizes activate-func fparam1 fparam2)
      (%ann-mlp)))


;; void CvANN_MLP::create(const Mat& layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )
;; void cv_CvANN_MLP_create(CvANN_MLP* self, Mat* layerSizes, int activateFunc, double fparam1, double fparam2)
(defcfun ("cv_CvANN_MLP_create" %ann-mlp-create) ann-mlp
  (self ann-mlp)
  (layer-sizes mat)
  (activate-func :int)
  (fparam1 :double)
  (fparam2 :double))

(defun ann-mlp-create (self layer-sizes &optional (activate-func +ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  "Constructs MLP with the specified topology."
  (%ann-mlp-create self layer-sizes activate-func fparam1 fparam2))


;; float CvANN_MLP::predict(const Mat& inputs, Mat& outputs) const
;; float cv_CvANN_MLP_predict2_3(CvANN_MLP* self, Mat* inputs, Mat* outputs) 
(defcfun ("cv_CvANN_MLP_predict2_3" ann-mlp-predict) :float
  (self ann-mlp)
  (inputs mat)
  (outputs mat))


;; int CvANN_MLP::train(const Mat& inputs, const Mat& outputs, const Mat& sampleWeights, const Mat& sampleIdx=Mat(), 
;;                      CvANN_MLP_TrainParams params=CvANN_MLP_TrainParams(), int flags=0 )
;; int cv_CvANN_MLP_train6_0(CvANN_MLP* self, Mat* inputs, Mat* outputs, Mat* sampleWeights, Mat* sampleIdx, 
;;                           CvANN_MLP_TrainParams* params, int flags)
(defcfun ("cv_CvANN_MLP_train6_0" %ann-mlp-train) :int
  (self ann-mlp)
  (inputs mat)
  (ouputs mat)
  (sample-weights mat)
  (sample-idx mat)
  (params ann-mlp-train-params)
  (flags :int))

(defun ann-mlp-train (self inputs outputs sample-weights &optional (sample-idx (mat) given-sample-idx) 
							   (params (ann-mlp-train-params) given-params) (flags 0))
  "Trains/updates MLP."
  (let ((return (%ann-mlp-train self inputs outputs sample-weights sample-idx params flags)))
    (if given-sample-idx nil (del-mat sample-idx))
    (if given-params nil (del-ann-mlp-train-params params))
    return))


;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams()
(defcfun ("cv_create_CvANN_MLP_TrainParams" %ann-mlp-train-params) ann-mlp-train-params)

;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams(CvTermCriteria term_crit, int train_method, double param1, double param2=0 )
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2)
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params4) ann-mlp-train-params
  (term-crit term-criteria)
  (train-method :int)
  (param1 :double)
  (param2 :double))

(defun ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params4 term-crit train-method param1 param2)
	   (%ann-mlp-train-params)))
