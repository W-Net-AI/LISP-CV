(defpackage "LISP-EXECUTABLE.EXAMPLE"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE" "LISP-CV"))
(in-package "LISP-EXECUTABLE.EXAMPLE")

;;See <lisp-cv-src-dir>/macros.lisp for instructions


(define-program example-program (&options help)

  (with-capture (cap (video-capture 0))
    (let* ((window-name "EXAMPLE-PROGRAM"))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
  	(loop
	   (with-mat (frame (mat))
	     (cap-read cap frame)
	     (imshow window-name frame)
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


(define-program control-c-tester (&options help (sleep sleep-time))
  (declare (conversion-function (real 0) sleep)
	   (type (or null (real 0)) sleep-time))

  (unless sleep
    (setf sleep-time 20.0))

  (cond
    (help
     (format t "Usage: [options]

Sleep for an amount of time.

Options:
  --help                      This helpful message.
  --sleep <positive real>     How long to sleep for. Default is 20 seconds.
")
     0)
    (t
     (sleep sleep-time)
     0)))
