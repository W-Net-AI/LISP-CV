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
       (%ann-mlp-train self inputs outputs sample-weights sample-idx params flags)
       (if given-sample-idx nil (del-mat sample-idx))
       (if given-params nil (del-ann-mlp-train-params params)))


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
