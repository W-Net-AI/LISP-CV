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
  (trackbarname (:pointer string*))
  (winname (:pointer string*))
  (value :pointer)
  (count :int)
  (on-change (:pointer trackbar-callback))
  (userdata :pointer))


(defun create-trackbar (trackbarname winname value count &optional (on-change (null-pointer)) (userdata (null-pointer)))
  "Creates a trackbar and attaches it to the specified window."
  (%create-trackbar (foreign-alloc :string :initial-element trackbarname) (foreign-alloc :string :initial-element winname) value count on-change userdata))


;; void destroyAllWindows()
;; void cv_destroyAllWindows() 
(defcfun ("cv_destroyAllWindows" destroy-all-windows) :void
  "Destroys all of the HighGUI windows.")


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


(defun named-window (winname &optional (flags +window-autosize+))
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
  (%set-mouse-callback (foreign-alloc :string :initial-element winname) on-mouse userdata))


;; int waitKey(int delay=0)
;; int cv_waitKey(int delay)
(defcfun ("cv_waitKey" %wait-key) :int
  (delay :int))


(defun wait-key (&optional (delay 0))
  "Waits for a pressed key."
  (%wait-key delay))


;;; Reading and Writing Images and Video


;; VideoCapture::VideoCapture()
;; VideoCapture* cv_create_VideoCapture() 
(defcfun ("cv_create_VideoCapture" video-capture0) (:pointer video-capture) 
	 "VideoCapture constructor")


;; VideoCapture::VideoCapture(int device)
;; VideoCapture* cv_create_VideoCapture1_0(int device)
(cffi:defcfun ("cv_create_VideoCapture1_0" video-capture-dev) (:pointer video-capture)
  "VideoCapture constructor"
  (device :int))


;; VideoCapture::VideoCapture(const string& filename)
;; VideoCapture* cv_create_VideoCapture1(String* filename) {
(defcfun ("cv_create_VideoCapture1" video-capture-fn) (:pointer video-capture)
  "VideoCapture constructor"
  (filename (:pointer string*)))


(defun video-capture (&optional src)
  (cond ((eq src nil)
	 (video-capture0))
	((numberp src)
	 (video-capture-dev src))
	((stringp src) 
	 (video-capture-fn (foreign-alloc :string :initial-element src)))
	(t nil)))


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
;; mat cv_imread (const char* filename, int flags)
(defcfun ("cv_imread" %imread) (:pointer mat)
  (filename (:pointer string*))
  (flags :int))

(defun imread (filename &optional (flags 1))
  (%imread (c-string-to-string* filename (length filename)) flags))


;; bool imwrite(const string& filename, InputArray img, const vector<int>& params=vector<int>() )
;; bool cv_imwrite(String* filename, Mat* img, vector_int* params) 
(defcfun ("cv_imwrite2" %imwrite) :boolean
  (filename :string)
  (img (:pointer mat))
  (params (:pointer vector-int)))

(defun imwrite (filename img &optional (params (vector-int)))
  "Saves an image to a specified file."
  (%imwrite filename img params))


;; VideoWriter* cv_create_VideoWriter() 
(defcfun ("cv_create_VideoWriter" video-writer0) (:pointer video-writer)
  "VIDEO-WRITER constructor")


;; VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor) 
;; VideoWriter* cv_create_VideoWriter5(String* filename, int fourcc, double fps, Size* frameSize, bool isColor)
(defcfun ("cv_create_VideoWriter5" video-writer5) (:pointer video-writer)
  (filename (:pointer string*))
  (fourcc :int)
  (fps :double)
  (frame-size (:pointer size))
  (is-color :boolean))

(defun video-writer (&optional filename fourcc fps frame-size (is-color t))
  "VIDEO-WRITER constructor"  
	   (cond ((eq filename nil)
		  (video-writer0))
		 (filename
		  (video-writer5 (foreign-alloc :string :initial-element filename) fourcc fps frame-size is-color))
		 (t nil)))


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

;; double cv_getWindowProperty(String* winname, int prop_id) 
(defcfun ("cv_getWindowProperty" %get-window-property) :double
  (winname (:pointer string*))
  (prop-id :int))


(defun get-window-property (winname prop-id)
  "Provides parameters of a window."
  (%get-window-property (foreign-alloc :string :initial-element winname) prop-id))


;; void setWindowProperty(const string& winname, int prop_id, double prop_value)
;; void cv_setWindowProperty(String* winname, int prop_id, double prop_value)
(defcfun ("cv_setWindowProperty" %set-window-property) :void
  (winname (:pointer string*))
  (prop-id :int)
  (prop-value :double))


(defun set-window-property (winname prop-id prop-value)
  "Changes parameters of a window dynamically."
  (%set-window-property  (foreign-alloc :string :initial-element winname) prop-id (coerce prop-value 'double-float)))

