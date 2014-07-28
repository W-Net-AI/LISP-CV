;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; macros.lisp
;;;; OpenCV bindings for SBCL
;;;; Macros for CFFI functions
(in-package :lisp-cv)



;; DEFPARAMETER macro (to make testing easier)

(defmacro d (var val)
  `(defparameter ,var ,val))


;; FORMAT macro (to make debugging easier)

(defmacro f (&optional (val "test"))
  `(if ,val
       (format t "~%~a~%~%" ,val) nil))


;; LET macro (to make REPL testing easier)

(defmacro l (var val &body body)
  `(let ((,var ,val))
     ,@body))


;;SIZEOF macro

(defmacro size-of (val)
  `(foreign-type-size ,val))


;; TIME Macro used to time your functions - see examples.lisp for usage

(defmacro $ (&rest arguments)
  (let ((last (last arguments))
        (count-form 1000000))
    (when (integerp (car last))
      (setq count-form (car last))
      ;; Exclude COUNT-FORM from the list.
      (setq arguments (ldiff arguments last)))
    `(time (dotimes (_ ,count-form) ,@arguments))))


;; FOREIGN-ALLOC macro

(defmacro alloc (&optional type value)
       (cond ((listp value)
	      `(foreign-alloc ,type ,:initial-contents ,value))
	     (t `(foreign-alloc ,type ,:initial-element ,value))))


;; FOREIGN-FREE macro


(defmacro free (ptr)
  `(foreign-free ,ptr))


;; RUN

;; Macro to build and run executable(not implemented all the way yet)

;; To use place the ~/quicklisp/dists/quicklisp/software/lisp-cv-master/extras/lisp-executable-example.asd 
;; file in the ~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example directory.  
;; This directory was installed when you built LISP-CV. Then take open the main.lisp file in that directory 
;; and edit it with your LISP-CV code. The main.lisp file must stay in that directory. An example main.lisp
;; file that shows the camera feed in a window is in the <lisp-cv-source-dir>/extras folder. Edit it to your 
;; liking and run: (RUN) at the REPL. The macro (RUN) will build the executable. It will be placed it in the
;; ~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example directory. Then it will run it.
;; Building an executable from your LISP-CV code will will knock about 33% off the run time.  

(defmacro run ()
(asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")
(run-program "~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example/example-program"))

