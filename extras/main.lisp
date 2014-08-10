(defpackage "LISP-EXECUTABLE.EXAMPLE"
  (:use "COMMON-LISP" "LISP-EXECUTABLE"))
(in-package "LISP-EXECUTABLE.EXAMPLE")

;;See <lisp-cv-src-dir>/macros.lisp for instructions


(define-program example-program (&options help)

  (cv:with-captured-camera (cap  0 :width 640 :height 480)
    help
    (let* ((window-name "EXAMPLE-PROGRAM"))
      (cv:with-named-window (window-name cv:+window-normal+)
	(cv:move-window window-name 759 175)
  	(loop
	   (cv:with-mat ((frame (cv:mat)))
	     (cv:read cap frame)
	     (cv:imshow window-name frame)
	     (let ((c (cv:wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


