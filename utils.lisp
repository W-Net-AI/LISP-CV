;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; utils.lisp
;;;; Utility functions and macros(Most borrowed from the)


(in-package :lisp-cv)


;;; -----------------------------------------------------------------------------------------
;;; package
;;; -----------------------------------------------------------------------------------------

(defun rename-package-nicknames (package &rest nicknames)
  "for Alias short package name from too long package name."
  (let ((pkg (package-name (find-package package))))
    #-sbcl(rename-package pkg pkg (append nicknames (package-nicknames pkg)))
    #+sbcl
    (let ((lock-p (sb-ext:package-locked-p pkg)))
      (when lock-p (sb-ext:unlock-package pkg))
      (prog1
	  (rename-package pkg pkg (append nicknames (package-nicknames pkg)))
	(when lock-p (sb-ext:lock-package pkg))))))

;;; -----------------------------------------------------------------------------------------
;;; file
;;; -----------------------------------------------------------------------------------------

(defun full-pathname (path)
  "returning absoulte full-pathname of path"
  #+ccl (namestring (ccl:full-pathname path))
  #-ccl (namestring (get-fullpath path)))

#-ccl
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


(define-symbol-macro *lisp-cv-src-dir*  (full-pathname (asdf::system-source-directory :lisp-cv))) 


;;; -----------------------------------------------------------------------------------------
;;; external-process
;;; -----------------------------------------------------------------------------------------

(defun run-program (command &key output wait)
  #+ccl (ccl:run-program "/bin/sh" (list "-c" command) :output output :wait wait)
  #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" command) :output output :wait wait)
  #+clisp (ext:run-program "/bin/sh" :arguments (list "-c" command) :output (if (eql output t) :terminal output) :wait wait)
  #+abcl (progn
	   wait
	   (ext:run-shell-command (format nil "/bin/sh -c \"~a\"" command) :output (if (eql output t) *standard-output* output)))
  #+ecl (let ((*standard-output* ext:+process-standard-output+)
	      (*standard-input* ext:+process-standard-input+)
	      (*error-output* ext:+process-error-output+))
	  output wait
	  (ext:system (format nil "/bin/sh -c \"~a\"" command))))

;;; -----------------------------------------------------------------------------------------
;;; printing
;;; -----------------------------------------------------------------------------------------

(defun println (thing &rest things)
  (format t "~&~{~a ~}~%" (cons thing things)))

;;; -----------------------------------------------------------------------------------------
;;; sequence
;;; -----------------------------------------------------------------------------------------

(defmacro -> (&optional arg &body body)
  "(-> (+ 1 2) (* 3) (/ 3))  expand to => (/ (* (+ 1 2) 3) 3)"
  (when arg
    (if (not body) arg
	(let* ((form (mklist (car body)))
	       (form (append (list (car form) arg) (cdr form))))
	  (reduce (lambda (x y)
		    (let ((y (mklist y)))
		      (append (list (car y) x) (cdr y)))) (append (list form) (cdr body)))))))

(defun mklist (val)
  "If val is lisp, then return. but if val is atom, make list from val."
  (if (listp val) val (list val)))

(defun partition (lst x)
  (labels ((partition-helper (lst acc x)
	     (cond ((not lst) acc)
		   ((< (length lst) x) (cons lst acc))
		   (t (partition-helper (subseq lst x) (cons (subseq lst 0 x) acc) x))))) 
    (reverse (partition-helper lst '() x))))

(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))


(defun dup (object &optional (n 2))
  (duplicate object n))

(defmethod duplicate (self (n integer))
  (if (not (listp self)) (make-list n :initial-element self)
      (loop for i from 0 below n
	 collect (copy-list self))))

(defmethod duplicate ((self function) (n integer))
  (loop for i from 0 below n collect (funcall self i)))

(defmethod duplicate ((self function) (n list))
  (loop for i in n collect (funcall self i)))


(defmacro do-while ((var form) &body body)
  `(do ((,var ,form ,form))
       ((not ,var))
     ,@body))


;;; -----------------------------------------------------------------------------------------
;;; read macro
;;; -----------------------------------------------------------------------------------------

;;; #! read-macro - clojure style of lambda functions.

(defvar *name-db*)
(defmethod name-parse ((symbol symbol))
  (let ((name-string (string-upcase symbol)))
    (if (char= #\% (elt name-string 0))
	(let ((number (progn (when (= 1 (length name-string))
			       (setq name-string (format nil "~a1" name-string)))
			     (parse-integer (subseq name-string 1)))))
	  (when (zerop number) (error "in-args number must not 0"))
	  (let ((sym (assoc number *name-db*)))
	    (if sym (cdr sym)
		(let ((new-symb (cons number (gensym))))
		  (setf *name-db* (append *name-db* (list new-symb)))
		  (cdr new-symb)))))
	symbol)))

(defmethod name-parse ((self t))
  self)

(defun do-parse-from-form (form)
  (cond ((null form) nil)
	((atom form) (name-parse form))
	(t (cons (do-parse-from-form (car form))
		 (do-parse-from-form (cdr form))))))

(defun fill-in-name-db (db)
  (when db
    (let ((max-number (reduce  #'max db :key #'car)))
      (loop for i from 1 to max-number
	 collect (let ((val (find i db :key #'car)))
		   (if val val (cons i (gensym))))))))

(set-dispatch-macro-character #\# #\!
		     (lambda (stream char1 char2)
		       (declare (ignore char1 char2))
		       (let ((first-char (read-char stream nil nil)))
			 (if (char= first-char #\space) (error "bad syntax..by #\\space")
			     (unread-char first-char stream)))
		       (let ((*name-db* nil))
			 (let ((body-form (do-parse-from-form (read stream nil nil)))
			       (args (mapcar #'cdr (fill-in-name-db *name-db*))))
			   `(lambda ,(cat args '(&rest rest))
			      (declare (ignorable ,@args rest))
			      ,body-form)))))
