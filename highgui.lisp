;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; highgui.lisp
;;;; OpenCV bindings for SBCL
;;;; High-level GUI and Media I/O
(in-package :lisp-cv)




;;; Types and structures

;;; User Interface

;; int createTrackbar(const string& trackbarname, const string& winname, int* value, int count, TrackbarCallback onChange=0, void* userdata=0)
;; int cv_createTrackbar(String* trackbarname, String* winname, int* value, int count, TrackbarCallback* onChange, void* userdata)
(defcfun ("cv_createTrackbar" %create-trackbar) :int
  (trackbarname :string)
  (winname :string)
  (value :pointer)
  (count :int)
  (on-change (:pointer trackbar-callback))
  (userdata :pointer))

(defun create-trackbar (trackbarname winname value count &optional (on-change (null-pointer)) (userdata (null-pointer)))
  "Creates a trackbar and attaches it to the specified window."
  (%create-trackbar trackbarname winname value count on-change userdata))

;; void destroyWindow(const string& winname)
;; void cv_destroyWindow((:pointer string*) winname)
(defcfun ("cv_destroyWindow" %destroy-window) :void
  "Destroys a window."
  (winname (:pointer string*)))

(defun destroy-window (winname)
  "Destroys a window."
  (%destroy-window (foreign-alloc :string :initial-element winname)))

;; void imshow(const string& winname, InputArray mat)
;; void cv_imshow(String* winname, Mat* mat)
(defcfun ("cv_imshow" %imshow) :void
  (winname (:pointer string*))
  (mat (:pointer mat)))

(defun imshow (winname mat)
  "Displays an image in the specified window."
  (%imshow (foreign-alloc :string :initial-element winname) mat))

;; void moveWindow(const string& winname, int x, int y)
;; void cv_moveWindow((:pointer string*) winname, int x, int y)
(defcfun ("cv_moveWindow" %move-window) :void
  (winname (:pointer string*))
  (x :int)
  (y :int))

(defun move-window (winname x y)
  "Moves window to the specified position"
  (%move-window (foreign-alloc :string :initial-element winname) x y))

;; void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE)
;; void cv_namedWindow((:pointer string*) winname, int flags)
(cffi:defcfun ("cv_namedWindow" %named-window) :void
  (winname (:pointer string*))
  (flags :int))

(defun named-window (winname flags)
  "Creates a window."
  (%named-window (foreign-alloc :string :initial-element winname) flags))

;; void setMouseCallback(const string& winname, MouseCallback onMouse, void* userdata=0)
;;void cv_setMouseCallback(const char* winname, MouseCallback onMouse, void* userdata)
(defcfun ("cv_setMouseCallback" %set-mouse-callback) :void
  (winname :string)
  (on-mouse (:pointer mouse-callback))
  (userdata :pointer))

(defun set-mouse-callback (winname on-mouse &optional (userdata (null-pointer)))
  "Sets mouse handler for the specified window."
  (%set-mouse-callback winname on-mouse userdata))

;; int waitKey(int delay=0)
;; int cv_waitKey(int delay)
(defcfun ("cv_waitKey" %wait-key) :int
  (delay :int))
 
(defun wait-key (&optional (delay 0))
  "Waits for a pressed key."
  (%wait-key delay))


;;; Reading and Writing Images and Video


;; VideoCapture::VideoCapture(int device)
;; VideoCapture* cv_create_VideoCapture1_0(int device)
 (cffi:defcfun ("cv_create_VideoCapture1_0" cap-cam) (:pointer video-capture)
  "Open video file or a capturing device for video capturing"
  (device :int))

;; VideoCapture::VideoCapture(const string& filename)
;; VideoCapture* cv_create_VideoCapture1(String* filename) {
(defcfun ("cv_create_VideoCapture1" %cap-file) (:pointer video-capture)
  (filename (:pointer string*)))

(defun cap-file (filename)
  "VideoCapture constructor."
  (%cap-file (foreign-alloc :string :initial-element filename)))

;; double VideoCapture::get(int propId)
;; double cv_VideoCapture_get(VideoCapture* self, int propId)
 (cffi:defcfun ("cv_VideoCapture_get" cap-get) :double
  "Returns the specified VideoCapture property."
  (self (:pointer video-capture))
  (prop-id :int))

;; bool VideoCapture::isOpened()
;; cv_VideoCapture_isOpened0
(cffi:defcfun ("cv_VideoCapture_isOpened0" cap-is-open) :boolean
  "Returns true if video capturing has been initialized already."
  (self (:pointer video-capture)))

;; bool VideoCapture::read(Mat& image)
;; bool cv_VideoCapture_read(VideoCapture* self, Mat* image)
 (cffi:defcfun ("cv_VideoCapture_read" cap-read) :boolean
  "Grabs, decodes and returns the next video frame."
  (self (:pointer video-capture))
  (image (:pointer mat)))

(defcfun ("cv_VideoCapture_release0_0" cap-release) :void
  (self (:pointer video-capture)))

;; bool VideoCapture::set(int propId, double value
;; bool cv_VideoCapture_set(VideoCapture* self, int propId, double value)
(cffi:defcfun ("cv_VideoCapture_set" %cap-set) :boolean
  (self (:pointer video-capture))
  (prop-id :int)
  (value :double))

(defun cap-set (self prop-id value)
  "Sets a property in the VideoCapture."
   (%cap-set self prop-id (coerce value 'double-float)))

;; Mat imread(const string& filename, int flags=1)
;; mat cv_imread2 (const char* filename, int flags)
(defcfun ("cv_imread2" imread) (:pointer mat)
  (filename :string)
  (flags :int))


;; VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor) 
;; VideoWriter* cv_create_VideoWriter5(String* filename, int fourcc, double fps, Size* frameSize, bool isColor)
(defcfun ("cv_create_VideoWriter5" %video-writer) (:pointer video-writer)
  (filename (:pointer string*))
  (fourcc :int)
  (fps :double)
  (frame-size (:pointer size))
  (is-color :boolean))

(defun video-writer (filename fourcc fps frame-size &optional (is-color t))
  "VIDEO-WRITER constructor"
  (%video-writer (foreign-alloc :string :initial-element filename) fourcc fps frame-size is-color))

;; VideoWriter* cv_create_VideoWriter() 
(defcfun ("cv_create_VideoWriter" video-writer-init) (:pointer video-writer)
  "VIDEO-WRITER constructor")

;; bool VideoWriter::isOpened()
;; bool cv_VideoWriter_isOpened0_0(VideoWriter* self) 
  "Returns true if video writer has been successfully initialized."
(defcfun ("cv_VideoWriter_isOpened0_0" video-writer-is-open) :boolean
  (self (:pointer video-writer)))

;; void VideoWriter::write(const Mat& image)
;; void cv_VideoWriter_write(VideoWriter* self, Mat* image)
(defcfun ("cv_VideoWriter_write" video-writer-write) :void
  "Writes the next video frame"
  (self (:pointer mat))
  (image (:pointer mat)))

(defmacro with-capture ((capture-var capture) &body body)
  "Ensures CAP-RELEASE gets called on captures."
  `(let ((,capture-var ,capture))
     (unwind-protect
	  (progn ,@body)
       (cap-release ,capture-var))))


;;; Qt New Functions
