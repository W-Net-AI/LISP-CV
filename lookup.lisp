;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; lookup.lisp
;;;; OpenCV bindings for SBCL
;;;; Lookup functions 
(in-package :lisp-cv)
;; TODO - This file in process - not functional
;; Creates hash-table used to lookup and print Common Lisp and C++
(defparameter lookup  '((Mat`::at . at) (c . 3) (in-range . inRange)))



(defun lookup (name language) 
  (if (eq language 'lisp)
      (setf name (car (assoc name lookup)))
      (if (eq language 'C++) 
	  (setf name (cdr (assoc name lookup))))) 
name)





          


