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
  (trackbarname *string)
  (winname *string)
  (value :pointer)
  (count :int)
  (on-change trackbar-callback)
  (userdata :pointer))


(defun create-trackbar (trackbarname winname value count &optional (on-change (null-pointer)) (userdata (null-pointer)))
       "Creates a trackbar and attaches it to the specified window."
       (%create-trackbar (c-string-to-string trackbarname (length trackbarname)) (c-string-to-string winname (length winname)) 
			 value count on-change userdata))


;; void destroyAllWindows()
;; void cv_destroyAllWindows() 
(defcfun ("cv_destroyAllWindows" destroy-all-windows) :void
  "Destroys all of the HighGUI windows.")


;; void destroyWindow(const string& winname)
;; void cv_destroyWindow(String* winname)
(defcfun ("cv_destroyWindow" %destroy-window) :void
  "Destroys a window."
  (winname *string))

(defun destroy-window (winname)
       "Destroys a window."
       (%destroy-window (c-string-to-string winname (length winname))))
       
       
;; int getTrackbarPos(const String& trackbarname, const String& winname)
;; int cv_getTrackbarPos(String* trackbarname, String* winname)
(defcfun ("cv_getTrackbarPos" %get-trackbar-pos) :int
  (trackbarname *string)
  (winname *string))

(defun get-trackbar-pos (trackbarname winname)
  "Returns the trackbar position."
  (%get-trackbar-pos (c-string-to-string trackbarname (length trackbarname)) (c-string-to-string winname (length winname))))


;; void imshow(const string& winname, InputArray mat)
;; void cv_imshow(String* winname, Mat* mat)
(defcfun ("cv_imshow" %imshow) :void
  (winname *string)
  (mat mat))


(defun imshow (winname mat)
       "Displays an image in the specified window."
       (%imshow (c-string-to-string winname (length winname)) mat))


;; void moveWindow(const string& winname, int x, int y)
;; void cv_moveWindow(String* winname, int x, int y)
(defcfun ("cv_moveWindow" %move-window) :void
  (winname *string)
  (x :int)
  (y :int))


(defun move-window (winname x y)
       "Moves window to the specified position"
       (%move-window (c-string-to-string winname (length winname)) x y))


;; void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE)
;; void cv_namedWindow(String* winname, int flags)
(cffi:defcfun ("cv_namedWindow" %named-window) :void
  (winname *string)
  (flags :int))

(defun named-window (winname &optional (flags +window-autosize+))
       "Creates a window."
       (%named-window (c-string-to-string winname (length winname)) flags))

(defmacro with-named-window ((winname &optional (flags +window-autosize+)) &body body)
	  `(unwind-protect (progn (named-window ,winname ,flags)
				  ,@body)
				  (destroy-window ,winname)))



;; void setMouseCallback(const string& winname, MouseCallback onMouse, void* userdata=0)
;;void cv_setMouseCallback(const char* winname, MouseCallback onMouse, void* userdata)
(defcfun ("cv_setMouseCallback" %set-mouse-callback) :void
  (winname *string)
  (on-mouse mouse-callback)
  (userdata :pointer))


(defun set-mouse-callback (winname on-mouse &optional (userdata (null-pointer)))
       "Sets mouse handler for the specified window."
       (%set-mouse-callback (c-string-to-string winname (length winname)) on-mouse userdata))
       
       
;; void setTrackbarPos(const String& trackbarname, const String& winname, int pos)
;; void cv_setTrackbarPos(String* trackbarname, String* winname, int pos)
(defcfun ("cv_setTrackbarPos" %set-trackbar-pos) :void
  (trackbarname *string)
  (winname *string)
  (pos :int))

(defun set-trackbar-pos (trackbarname winname pos)
  "Sets the trackbar position."
  (%set-trackbar-pos (c-string-to-string trackbarname (length trackbarname)) (c-string-to-string winname (length winname))  pos))


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
(defcfun ("cv_create_VideoCapture" video-capture0) video-capture 
	 "VideoCapture constructor")


;; VideoCapture::VideoCapture(int device)
;; VideoCapture* cv_create_VideoCapture1_0(int device)
(cffi:defcfun ("cv_create_VideoCapture1_0" video-capture-dev) video-capture
	      "VideoCapture constructor"
	      (device :int))


;; VideoCapture::VideoCapture(const string& filename)
;; VideoCapture* cv_create_VideoCapture1(String* filename) {
(defcfun ("cv_create_VideoCapture1" video-capture-fn) video-capture
	 "VideoCapture constructor"
	 (filename *string))


(defun video-capture (&optional src)
       (cond ((eq src nil)
	      (video-capture0))
	      ((numberp src)
	       (video-capture-dev src))
	       ((stringp src) 
		(video-capture-fn (c-string-to-string src (length src))))
		(t nil)))


;; double VideoCapture::get(int propId)
;; double cv_VideoCapture_get(VideoCapture* self, int propId)
(cffi:defcfun ("cv_VideoCapture_get" cap-get) :double
  "Returns the specified VideoCapture property."
  (self video-capture)
  (prop-id :int))


;; bool VideoCapture::isOpened()
;; cv_VideoCapture_isOpened0
(cffi:defcfun ("cv_VideoCapture_isOpened0" cap-is-open) :boolean
  "Returns true if video capturing has been initialized already."
  (self video-capture))


;; bool VideoCapture::read(Mat& image)
;; bool cv_VideoCapture_read(VideoCapture* self, Mat* image)
(cffi:defcfun ("cv_VideoCapture_read" cap-read) :boolean
  "Grabs, decodes and returns the next video frame."
  (self video-capture)
  (image mat))


(defcfun ("cv_VideoCapture_release0_0" cap-release) :void
  (self video-capture))


;; bool VideoCapture::set(int propId, double value
;; bool cv_VideoCapture_set(VideoCapture* self, int propId, double value)
(cffi:defcfun ("cv_VideoCapture_set" %cap-set) :boolean
  (self video-capture)
  (prop-id :int)
  (value :double))


(defun cap-set (self prop-id value)
       "Sets a property in the VideoCapture."
       (%cap-set self prop-id (coerce value 'double-float)))


;; Mat imread(const string& filename, int flags=1)
;; mat cv_imread (const char* filename, int flags)
(defcfun ("cv_imread" %imread) mat
  (filename *string)
  (flags :int))

(defun imread (filename &optional (flags 1))
       (%imread (c-string-to-string filename (length filename)) flags))


;; bool imwrite(const string& filename, InputArray img, const vector<int>& params=vector<int>() )
;; bool cv_imwrite(String* filename, Mat* img, vector_int* params) 
(defcfun ("cv_imwrite2" %imwrite) :boolean
  (filename :string)
  (img mat)
  (params vector-int))

(defun imwrite (filename img &optional (params (vec-int)))
       "Saves an image to a specified file."
       (%imwrite filename img params))


;; VideoWriter* cv_create_VideoWriter() 
(defcfun ("cv_create_VideoWriter" video-writer0) video-writer
	 "VIDEO-WRITER constructor")


;; VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor) 
;; VideoWriter* cv_create_VideoWriter5(String* filename, int fourcc, double fps, Size* frameSize, bool isColor)
(defcfun ("cv_create_VideoWriter5" video-writer5) video-writer
	 (filename *string)
	 (fourcc :int)
	 (fps :double)
	 (frame-size size)
	 (is-color :boolean))

(defun video-writer (&optional filename fourcc fps frame-size (is-color t))
       "VIDEO-WRITER constructor"  
       (cond ((eq filename nil)
	      (video-writer0))
	      (filename
	       (video-writer5 (c-string-to-string filename (length filename)) fourcc fps frame-size is-color))
	       (t nil)))


;; bool VideoWriter::isOpened()
;; bool cv_VideoWriter_isOpened0_0(VideoWriter* self) 
"Returns true if video writer has been successfully initialized."
(defcfun ("cv_VideoWriter_isOpened0_0" video-writer-is-open) :boolean
  (self video-writer))


;; void VideoWriter::write(const Mat& image)
;; void cv_VideoWriter_write(VideoWriter* self, Mat* image)
(defcfun ("cv_VideoWriter_write" video-writer-write) :void
  "Writes the next video frame"
  (self video-writer)
  (image mat))


(defmacro with-capture ((capture-var capture) &body body)
  "Ensures CAP-RELEASE gets called on captures."
  `(let ((,capture-var ,capture))
     (unwind-protect
	  (progn ,@body)
       (cap-release ,capture-var)
       (del-vid-cap ,capture-var))))


(defmacro with-captured-camera ((capture-var dev-index &key width height) &body body)
  "Ensures CAP-RELEASE gets called on captures 
   and sets capture width/height in function"
  `(let ((,capture-var (video-capture ,dev-index)))
     (when ,width
       (cap-set ,capture-var +cap-prop-frame-width+ ,width))
     (when ,height
       (cap-set ,capture-var +cap-prop-frame-height+ ,height))
     (unwind-protect (progn ,@body)
       (cap-release ,capture-var)
       (del-vid-cap ,capture-var))))


(defmacro with-captured-file ((capture-var file-path &key width height) &body body)
  "Ensures CAP-RELEASE gets called on captures 
   and sets capture width/height in function"
  `(let ((,capture-var (video-capture ,file-path)))
     (when ,width
       (cap-set ,capture-var +cap-prop-frame-width+ ,width))
     (when ,height
       (cap-set ,capture-var +cap-prop-frame-height+ ,height))
     (unwind-protect (progn ,@body)
       (cap-release ,capture-var)
       (del-vid-cap ,capture-var))))


;;; Qt New Functions


;; void displayOverlay(const String& winname, const String& text, int delayms=0 )
;; void cv_displayOverlay(String* winname, String* text, int delayms)
(cffi:defcfun ("cv_displayOverlay" %display-overlay) :void
  (winname *string)
  (text *string)
  (flags :int))

(defun display-overlay (winname text &optional (delayms 0))
  "Displays a text on a window image as an overlay for a specified duration."
  (%display-overlay (c-string-to-string winname (length winname)) (c-string-to-string text (length text)) delayms))



;; double cv_getWindowProperty(String* winname, int prop_id) 
(defcfun ("cv_getWindowProperty" %get-window-property) :double
  (winname *string)
  (prop-id :int))

(defun get-window-property (winname prop-id)
       "Provides parameters of a window."
       (%get-window-property (c-string-to-string winname (length winname)) prop-id))


;; void setWindowProperty(const string& winname, int prop_id, double prop_value)
;; void cv_setWindowProperty(String* winname, int prop_id, double prop_value)
(defcfun ("cv_setWindowProperty" %set-window-property) :void
  (winname *string)
  (prop-id :int)
  (prop-value :double))

(defun set-window-property (winname prop-id prop-value)
       "Changes parameters of a window dynamically."
       (%set-window-property  (c-string-to-string winname (length winname)) prop-id (coerce prop-value 'double-float)))

