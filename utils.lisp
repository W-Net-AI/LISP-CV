;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; utils.lisp
;;;; Utility functions and macros


(in-package :lisp-cv)

;;;
;;; FILE:
;;; 

(defun get-fullpath (dir)
  (labels ((absolute-dir (dir)
	     (if (eql (car dir) :absolute) (if (find :home dir)
					       (append
						(pathname-directory (user-homedir-pathname))
						(cdr (member :home dir)))
					       dir)
		 (let* ((default-dir
			  (pathname-directory (truename ""))))
		   (when (find :up dir)
		     (setf dir (cdr dir))
		     (setf default-dir (butlast default-dir)))
		   (append default-dir (cdr dir))))))
    (make-pathname :directory (absolute-dir (pathname-directory dir)) :name (pathname-name dir) :type (pathname-type dir))))


(defun full-pathname (path)
  "returning absoulte full-pathname of path"
  #+ccl (namestring (ccl:full-pathname path))
  #-ccl (namestring (get-fullpath path)))


(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))


(define-symbol-macro lisp-cv-src-dir  (full-pathname (asdf::system-source-directory :lisp-cv)))

