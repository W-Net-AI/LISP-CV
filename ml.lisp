;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; ml.lisp
;;;; OpenCV bindings
;;;; Machine Learning

(in-package :lisp-cv)


;;; LISP-CV specific


(defun make-training-data-matrix (&key directory dsize test)

  "Creates training data to give to Machine Learning functions.
   First, converts all of the images in the directory you have 
   specified to single float. Then, resizes them, to the size 
   you specified, with the DSIZE parameter. Finally, reshapes 
   the images to 1D and adds the now 1D images, one image per 
   row, to the TRAINING-DATA matrix.
  
   Note: All of the images in the directory you specify must 
   be square and width/height values of DSIZE must be equal."

  (let* ((window-name "TRAINING-DATA")
	 (file-list (uiop:directory-files directory))
	 (img-list (list))
         ;;Extra list for GC
         (end-list (list))
	 (pathname-list (list))
         (img-height (round (size-height dsize)))
         (img-width (round (size-width dsize)))
	 (img-area (* img-height img-width))
	 (num-of-files 1016)
         ;;Create matrix to hold the training data
	 (training-data (mat num-of-files img-area +32fc1+))
         (pass-fail 0)
	 (i 0))

    (with-named-window (window-name +window-autosize+)

      (if (not (= img-height img-width)) 
	  (return-from make-training-data-matrix 
	    (error ":DSIZE width and height must be equal."))) 

      (check-type test boolean)         

      ;;Create a list of pathnames to
      ;;read with the IMREAD function.
      (dotimes (i num-of-files)
	(push (full-pathname (nth i file-list)) pathname-list))

      ;;Make a list of all images in 
      ;;the directory you specified.
      (dotimes (i num-of-files)
	(let ((path (c-string-to-string (nth i pathname-list) (length (nth i pathname-list)))))
	  (push  (%imread path 0) img-list)
	  (del-std-string path)))

      ;;Convert all images in 
      ;;list to single float.
      (dotimes (i num-of-files)
	(%convert-to (nth i img-list) (nth i img-list) +32fc1+ 1.0d0 0.0d0))

      ;;Resize all of the images and 
      ;;convert them to 1D matrices.
      (dotimes (i num-of-files)
	(%resize (nth i img-list) (nth i img-list) dsize 0d0 0d0 +inter-linear+)
	(push (reshape-rows (nth i img-list) 0 1) end-list))

      ;;Fill TRAINING-DATA with all of the 
      ;;1D matrices. One matrix per row.
      (dotimes (i num-of-files)
	(dotimes (k img-area)
	  (setf (mem-aref (%ptr training-data i) :float k) (mem-aref (%ptr (nth i end-list) 0) :float k))))

      ;;Garbage collect IMG-LIST
      (dotimes (i num-of-files)
	(del-mat (nth i img-list)))

      (if test (progn

;;;Test to make sure all of the 
;;;images are in TRAINING-DATA.   

		 ;;Reset IMG-LIST so 
		 ;;it can be reused.
		 (setf img-list (list))    
		 
		 ;;Fill IMG-LIST with matrices.
		 (dotimes (i num-of-files)
		   (push (mat-typed 1 img-area 0) img-list))

		 ;;Convert all matrices in 
		 ;;IMG-LIST to single float.
		 (dotimes (i num-of-files)
		   (%convert-to (nth i img-list) (nth i img-list) +32fc1+ 1.0d0 0.0d0))

		 ;;Fill each matrix in IMG-LIST with 
		 ;;a separate row from TRAINING-DATA.
		 (dotimes (i num-of-files)
		   (dotimes (k img-area)
		     (setf (mem-aref (%ptr (nth i img-list) 0) :float k) (mem-aref (%ptr training-data i) :float k))))

		 ;;If test fails..Break!
		 (dotimes (i num-of-files)
		   (dotimes (k img-area)
		     (if (eq (mem-aref (%ptr (nth i end-list) 0) :float k) 
			     (mem-aref (%ptr training-data i) :float k))
			 (setf pass-fail 1)
			 (setf pass-fail 0))))

		 ;;Garbage collect END-LIST
		 (dotimes (i num-of-files)
		   (del-mat (nth i end-list)))

		 (if (eq pass-fail 0)	
		     (return-from make-training-data-matrix 
		       (format t "Test failed...Break!"))
		     (format t "Test passed...Proceed!"))

		 (loop
		    ;;Show the images in IMG-LIST in a window 
		    ;;for an additional visual verification.
		    (let ((reshaped-images (reshape (nth i img-list) 
						    0 img-height)))
		      (imshow window-name reshaped-images)
		      (del-mat reshaped-images))

		    (if (< i 1015) (incf i))
		    (sleep .001)
		    (let ((c (wait-key 1)))
		      (when (or (= c 27) (= i 1015))

			(return-from make-training-data-matrix 
			  training-data))))) 
	  training-data))))



;;; Normal Bayes Classifier



;; CvNormalBayesClassifier::CvNormalBayesClassifier()
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier()
(defcfun ("cv_create_CvNormalBayesClassifier" normal-bayes-classifier-0) normal-bayes-classifier)

;; CvNormalBayesClassifier::CvNormalBayesClassifier(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), 
;;                                                  const Mat& sampleIdx=Mat() )
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier4(Mat* trainData, Mat* responses, Mat* varIdx, Mat* sampleIdx) 
(defcfun ("cv_create_CvNormalBayesClassifier4" normal-bayes-classifier-4) normal-bayes-classifier
  (train-data mat)
  (responses mat)
  (var-idx mat)
  (sample-idx mat))

(defun normal-bayes-classifier (&optional train-data responses  (var-idx (%mat) given-var-idx) (sample-idx (%mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier-4 train-data responses  var-idx sample-idx)
		    (normal-bayes-classifier-0))))
    (if given-var-idx nil (del-mat var-idx))
    (if given-sample-idx nil (del-mat sample-idx)) 
    return))

(defun make-normal-bayes-classifier (&optional train-data responses  (var-idx (%mat) given-var-idx) (sample-idx (%mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier-4 train-data responses  var-idx sample-idx)
		    (normal-bayes-classifier-0))))
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
(defcfun ("cv_create_CvKNearest" k-nearest-0) k-nearest)

;; CvKNearest::CvKNearest(const CvMat* trainData, const CvMat* responses, const CvMat* sampleIdx=0, bool isRegression=false, 
;;                        int max_k=32 )
;; CvKNearest* cv_create_CvKNearest5(Mat* trainData, Mat* responses, Mat* sampleIdx, bool isRegression, int max_k) 
(defcfun ("cv_create_CvKNearest5" k-nearest-5) k-nearest
  (train-data mat)
  (responses mat)
  (sample-idx mat)
  (is-regression :boolean)
  (max-k :int))

(defun k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest-5 train-data responses sample-idx is-regression max-k)
      (k-nearest-0)))

(defun make-k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest-5 train-data responses sample-idx is-regression max-k)
      (k-nearest-0)))


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

;; CvDTree:CvDTree()
;; CvDTree* cv_create_CvDTree() 
(defcfun ("cv_create_CvDTree" make-d-tree) d-tree)


;; CvDTreeParams::CvDTreeParams()
;; CvDTreeParams* cv_create_CvDTreeParams()
(defcfun ("cv_create_CvDTreeParams" d-tree-params-0) d-tree-params)

;; CvDTreeParams::CvDTreeParams(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                              int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors)
;; CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                                         int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, 
;;                                         const float* priors)
(defcfun ("cv_create_CvDTreeParams9" d-tree-params-9) d-tree-params
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
      (d-tree-params-9 max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
                      truncate-pruned-tree priors)
      (d-tree-params-0)))

(defun make-d-tree-params (&optional max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
			truncate-pruned-tree priors)
  (if max-depth
      (d-tree-params-9 max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
                      truncate-pruned-tree priors)
      (d-tree-params-0)))


;; CvDTreeNode* CvDTree::predict(const Mat& sample, const Mat& missingDataMask=Mat(), bool preprocessedInput=false ) const
;; CvDTreeNode* cv_CvDTree_predict3_0(CvDTree* self, Mat* sample, Mat* missingDataMask, bool preprocessedInput) 
(defcfun ("cv_CvDTree_predict3_0" %d-tree-predict) (:pointer (:struct d-tree-node))
  (self d-tree)
  (sample mat)
  (missing-data-mask mat)
  (preprocessed-input :boolean))

(defun d-tree-predict (self sample &optional (missing-data-mask (%mat) given-missing-data-mask) (preprocessed-input nil))
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

(defun d-tree-train (self train-data tflag responses &optional (var-idx (%mat) given-var-idx) (sample-idx (%mat) given-sample-idx) 
						       (var-type (%mat) given-var-type) (missing-data-mask (%mat) given-missing-data-mask) 
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
(defcfun ("cv_create_CvANN_MLP" ann-mlp-0) ann-mlp)

;; CvANN_MLP::CvANN_MLP(const CvMat* layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )
;; CvANN_MLP* cv_create_CvANN_MLP4(Mat* layerSizes, int activateFunc, double fparam1, double fparam2) 
(defcfun ("cv_create_CvANN_MLP4" ann-mlp-4) ann-mlp
  (layer-sizes mat)
  (activate-func :int)
  (fparam1 :double)
  (fparam2 :double))

(defun ann-mlp (&optional layer-sizes (activate-func +ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp-4 layer-sizes activate-func fparam1 fparam2)
      (ann-mlp-0)))

(defun make-ann-mlp (&optional layer-sizes (activate-func +ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp-4 layer-sizes activate-func fparam1 fparam2)
      (ann-mlp-0)))

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

(defun ann-mlp-train (self inputs outputs sample-weights &optional (sample-idx (%mat) given-sample-idx) 
							   (params (ann-mlp-train-params) given-params) (flags 0))
  "Trains/updates MLP."
  (let ((return (%ann-mlp-train self inputs outputs sample-weights sample-idx params flags)))
    (if given-sample-idx nil (del-mat sample-idx))
    (if given-params nil (del-ann-mlp-train-params params))
    return))


;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams()
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params-0) ann-mlp-train-params)

;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams(CvTermCriteria term_crit, int train_method, double param1, double param2=0 )
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2)
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params-4) ann-mlp-train-params
  (term-crit term-criteria)
  (train-method :int)
  (param1 :double)
  (param2 :double))

(defun ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params-4 term-crit train-method param1 param2)
	   (ann-mlp-train-params-0)))

(defun make-ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params-4 term-crit train-method param1 param2)
	   (ann-mlp-train-params-0)))
