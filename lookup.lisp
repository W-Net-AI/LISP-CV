;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; lookup.lisp
;;;; OpenCV bindings for SBCL
;;;; Lookup functions 
(in-package :lisp-cv)
;; TODO - This file in process - not functional


(defparameter l  '((c .3) (inRange . (|in-range| . |cv_inRange|))))


(defun lookup (name language) 
  (if (eq language 'lisp)
      (setf name (cadr (assoc name l)))
      (if (eq language 'c) 
	  (setf name (cddr (assoc name l))))) 
name)





          


