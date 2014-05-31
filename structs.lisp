;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; structs.lisp
;;;; OpenCV bindings
;;;; OpenCV structs


(in-package :lisp-cv)


;; D-TREE-NODE 


(cffi:defcstruct d-tree-node

	(class_idx :int)
	(Tn :int)
	(value :double)
	(parent :pointer)
	(left :pointer)
	(right :pointer)
	(split :pointer)
	(sample_count :int)
	(depth :int)
	(num_valid :pointer)
	(offset :int)
	(buf_idx :int)
	(maxlr :double)
	(complexity :int)
	(alpha :double)
	(node_risk :double)
	(tree_risk :double)
	(tree_error :double)
	(cv_Tn :pointer)
	(cv_node_risk :pointer)
	(cv_node_error :pointer)
	(get_num_valid :pointer)
	(set_num_valid :pointer))
