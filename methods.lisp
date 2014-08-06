;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; methods.lisp
;;;; Methods for the OpenCV bindings


(in-package :lisp-cv)


;;; DEFGENERIC

(defgeneric get* (self value)
  (:documentation "Used for all class bindings with a GET member function."))

(defgeneric abs (self)
  (:documentation "Used to call the binding for the OpenCV function 'abs' and CL:ABS."))

(defgeneric angle (self)
  (:documentation "Used for all class bindings with an ANGLE member."))

(defgeneric (setf angle) (val self)
  (:documentation "Used to setf the ANGLE value of class bindings with an ANGLE member."))

(defgeneric assign (self other)
  (:documentation "Used for bindings of the OpenCV = operator."))

(defgeneric apply* (self arg1 arg2 &optional arg3)
  (:documentation "Used for all class bindings with an APPLY member function."))

(defgeneric bounding-rect (self)
  (:documentation "Used for the BOUNDING-RECT member of ROTATED-RECT."))

(defgeneric center (self)
  (:documentation "Used for all class bindings with an CENTER member."))

(defgeneric (setf center) (val self)
  (:documentation "Used to setf the CENTER value of class bindings with an CENTER member."))

(defgeneric clone (self)
  (:documentation "Used for all class bindings with a CLONE member function."))

(defgeneric compute (type self &rest args)
  (:documentation "Used for all class bindings with a COMPUTE member function."))

(defgeneric create (type self &rest args)
  (:documentation "Used for all class bindings with a CREATE member function."))

(defgeneric data (self)
  (:documentation "Used for all class bindings with a DATA member function."))

(defgeneric detect (self &rest args)
  (:documentation "Used for all class bindings with a DETECT member function."))

(defgeneric descriptor-extractor-compute (self image keypoints descriptors)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-COMPUTE methods."))

(defgeneric descriptor-extractor-create (self descriptor-extractor-type)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-CREATE methods."))

(defgeneric descriptor-matcher-create (self descriptor-matcher-type)
  (:documentation "Used for all DESCRIPTOR-MATCHER-CREATE methods."))

(defgeneric descriptor-matcher-match (self query-descriptors train-descriptors matches &optional mask)
  (:documentation "Used for all DESCRIPTOR-MATCHER-MATCH methods.")) 

(defgeneric dot (self other)
  (:documentation "Used for all class bindings with a DOT member function."))

(defgeneric feature-2d-compute (self image keypoints descriptors)
  (:documentation "Used for all FEATURE-2D-COMPUTE methods."))

(defgeneric feature-2d-create (self name)
  (:documentation "Used for all FEATURE-2D-CREATE methods."))

(defgeneric feature-detector-create (self detector-type)
  (:documentation "Used for all FEATURE-DETECTOR-CREATE methods."))

(defgeneric feature-detector-detect (self image keypoints &optional mask)
  (:documentation "Used for all FEATURE-DETECTOR-DETECT methods."))

(defgeneric height (self)
  (:documentation "Used for all class bindings with an HEIGHT member."))

(defgeneric (setf height) (val self)
  (:documentation "Used to setf the HEIGHT value of class bindings with an HEIGHT member."))

(defgeneric is-opened (self)
  (:documentation "Used for all class bindings with an IS-OPENED member function."))

(defgeneric file-storage-write (fs name value)
  (:documentation "Used for all FILE-STORAGE-WRITE methods."))

(defgeneric match (self &rest args)
  (:documentation "Used for all class bindings with a MATCH member function."))

(defgeneric open (self &rest args)
  (:documentation "Used for all class bindings with a OPEN member function."))

(defgeneric predict (self &rest args)
  (:documentation "Used for all class bindings with a PREDICT member function."))

(defgeneric *read (self arg)
  (:documentation "Used for all class bindings with an READ member function."))

(defgeneric release (self)
  (:documentation "Used for all class bindings with a RELEASE member function."))

(defgeneric save (self &rest args)
  (:documentation "Used for all class bindings with a SAVE member function."))

(defgeneric size (arg &rest args)
  (:documentation "Used for all class bindings with a SIZE member."))

(defgeneric (setf size) (val self)
  (:documentation "Used to setf the SIZE value of class bindings with an SIZE member."))

(defgeneric train (self &rest args)
  (:documentation "Used for all class bindings with a TRAIN member."))

(defgeneric width (self)
  (:documentation "Used for all class bindings with an WIDTH member."))

(defgeneric (setf width) (val self)
  (:documentation "Used to setf the WIDTH value of class bindings with an WIDTH member."))

(defgeneric x (self)
  (:documentation "Used for all class bindings with an X member."))

(defgeneric (setf x) (val self)
  (:documentation "Used to setf the x coordinate of class bindings with an X member."))

(defgeneric (setf y) (val self)
  (:documentation "Used to setf the y coordinate of class bindings with an Y member."))

(defgeneric y (self)
  (:documentation "Used for all class bindings with an Y member."))

(defgeneric z (self)
  (:documentation "Used for all class bindings with an Z member."))

(defgeneric (setf z) (val self)
  (:documentation "Used to setf the z coordinate of class bindings with an Z member."))



;;;DEFMETHODS


(defmethod get* ((self cv-video-capture) (value integer))
	   (video-capture-get self value))


(defmethod angle ((self cv-rotated-rect))
  "Gets the rotation angle of a ROTATED-RECT object."
  (rotated-rect-angle self))


(defmethod (setf angle) ((val single-float) (self cv-rotated-rect))
  "Sets the rotation angle of a ROTATED-RECT object."
  (rotated-rect-set-angle self val))


(defmethod angle ((self cv-key-point))
  (key-point-angle self))


(defmethod (setf angle) ((val single-float) (self cv-key-point))
  (key-point-set-angle self val))


(defmethod assign ((self cv-mat) (other cv-mat))
  "Assign matrix data to another matrix."
  (mat-assign self other))


(defmethod assign ((self cv-mat) (other cv-scalar))
  "Assigns a scalar value to a matrix."
  (mat-assign-val self other))


(defmethod assign ((self cv-size) (other cv-size))
  "Assign data from one SIZE object to another,
   OTHER to SELF."
  (size-assign-to self other))


(defmethod bounding-rect ((self cv-rotated-rect))
  "Returns the minimal up-right rectangle 
   containing the rotated rectangle."
  (rotated-rect-bounding-rect self))


(defmethod center ((self cv-rotated-rect))
  "Gets the center of a ROTATED-RECT object."
  (rotated-rect-center self))

;; Using mem-aref to access the member may not be safe if OpenCv changes something.
(defmethod (setf center) ((val cv-point) (self cv-rotated-rect))
  (setf (mem-aref (c-pointer self) :float) (coerce (mem-aref (c-pointer val) :int) 'single-float))
  (setf (mem-aref (c-pointer self) :float 1) (coerce (mem-aref (c-pointer val) :int 1) 'single-float))
  val)


(defmethod clone ((self cv-mat))
  "Creates a full copy of the array and the underlying data."
  (mat-clone self))


(defmethod clone ((self cv-rect))
  "Creates a full copy of a RECT object"
  (rect-clone self))


(defmethod compute ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-bf-matcher self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-bf-matcher self args))))


(defmethod compute ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-brisk self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-brisk self args))))


(defmethod compute ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-surf self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-surf self args))))


(defmethod create ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-bf-matcher self (%c-string-to-string (first args)
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-bf-matcher self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-bf-matcher self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-bf-matcher self (first args)))))


(defmethod create ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-brisk self (%c-string-to-string (first args) 
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-brisk self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-brisk self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-brisk self (first args)))))


(defmethod create ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-surf self (%c-string-to-string (first args) 
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-surf self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d (first args))
	 (%feature-2d-create-surf self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-surf self (first args)))))


(defmethod data ((self cv-mat))
  (mat-data self))


(defmethod data ((self std-vector-char))
  (vec-char-to-c-arr self))


(defmethod data ((self std-vector-dmatch))
  (vec-dmatch-to-c-arr self))


(defmethod data ((self std-vector-double))
  (vec-double-to-c-arr self))


(defmethod data ((self std-vector-float))
  (vec-float-to-c-arr self))


(defmethod data ((self std-vector-int))
  (vec-int-to-c-arr self))


(defmethod data ((self std-vector-key-point))
  (vec-key-point-to-c-arr self))


(defmethod data ((self std-vector-mat))
  (vec-mat-to-c-arr self))


(defmethod data ((self std-vector-point))
  (vec-point-to-c-arr self))


(defmethod data ((self std-vector-point-2f))
  (vec-point-2f-to-c-arr self))


(defmethod data ((self std-vector-rect))
  (vec-rect-to-c-arr self))


(defmethod data ((self std-vector-uchar))
  (vec-uchar-to-c-arr self))


(defmethod data ((self std-vector-vec-2d))
  (vec-vec-2d-to-c-arr self))


(defmethod data ((self std-vector-vec-3d))
  (vec-vec-3d-to-c-arr self))


(defmethod data ((self std-vector-vec-4d))
  (vec-vec-4d-to-c-arr self))


(defmethod data ((self std-vector-vec-6d))
  (vec-vec-6d-to-c-arr self))


(defmethod data ((self std-vector-vec-2f))
  (vec-vec-2f-to-c-arr self))


(defmethod data ((self std-vector-vec-3f))
  (vec-vec-3f-to-c-arr self))


(defmethod data ((self std-vector-vec-4f))
  (vec-vec-4f-to-c-arr self))


(defmethod data ((self std-vector-vec-6f))
  (vec-vec-6f-to-c-arr self))


(defmethod data ((self std-vector-vec-2i))
  (vec-vec-2i-to-c-arr self))


(defmethod data ((self std-vector-vec-3i))
  (vec-vec-3i-to-c-arr self))


(defmethod data ((self std-vector-vec-4i))
  (vec-vec-4i-to-c-arr self))


(defmethod data ((self std-vector-vec-6i))
  (vec-vec-6i-to-c-arr self))


(defmethod data ((self std-vector-vec-8i))
  (vec-vec-8i-to-c-arr self))


(defmethod descriptor-extractor-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-bf-matcher self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-brisk self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-surf self image keypoints descriptors))


(defmethod descriptor-extractor-create ((self cv-bf-matcher) descriptor-extractor-type)
      (%descriptor-extractor-create-bf-matcher self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-brisk) descriptor-extractor-type)
      (%descriptor-extractor-create-brisk self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-surf) descriptor-extractor-type)
      (%descriptor-extractor-create-surf self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-matcher-create ((self cv-bf-matcher) descriptor-matcher-type)
      (%descriptor-matcher-create-bf-matcher self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-brisk) descriptor-matcher-type)
      (%descriptor-matcher-create-brisk self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-surf) descriptor-matcher-type)
      (%descriptor-matcher-create-surf self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-match ((self cv-bf-matcher) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod descriptor-matcher-match ((self cv-brisk) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod descriptor-matcher-match ((self cv-surf) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod detect ((self cv-bf-matcher) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-brisk) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-surf) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod dot ((self cv-mat) (other cv-mat))
  (mat-dot self other))


(defmethod dot ((self cv-point) (other cv-point))
  (dot-2i self other))


(defmethod dot ((self cv-point-2d) (other cv-point-2d))
  (dot-2d self other))


(defmethod dot ((self cv-point-2f) (other cv-point-2f))
  (dot-2f self other))


(defmethod dot ((self cv-point-3d) (other cv-point-3d))
  (dot-3d self other))


(defmethod dot ((self cv-point-3f) (other cv-point-3f))
  (dot-3f self other))


(defmethod dot ((self cv-point-3i) (other cv-point-3i))
  (dot-3i self other))


(defmethod feature-2d-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-bf-matcher self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-brisk self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-surf self image keypoints descriptors))


(defmethod feature-2d-create ((self cv-bf-matcher) name)
      (%feature-2d-create-bf-matcher self name))


(defmethod feature-2d-create ((self cv-brisk) name)
      (%feature-2d-create-brisk self name))


(defmethod feature-2d-create ((self cv-surf) name)
      (%feature-2d-create-surf self name))


(defmethod feature-detector-create ((self cv-bf-matcher) detector-type)
      (%feature-detector-create-bf-matcher self detector-type))


(defmethod feature-detector-create ((self cv-brisk) detector-type)
      (%feature-detector-create-brisk self detector-type))


(defmethod feature-detector-create ((self cv-surf) detector-type)
      (%feature-detector-create-surf self detector-type))


(defmethod feature-detector-detect ((self cv-bf-matcher) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-bf-matcher self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-brisk) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-brisk self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-surf) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-surf self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value float))
  (if (typep value 'double-float)
      (file-storage-write-double fs (%c-string-to-string name (length name)) value)
      (file-storage-write-float fs (%c-string-to-string name (length name)) value)))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value integer))
  (file-storage-write-int fs (%c-string-to-string name (length name)) value))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value cv-mat))
  (file-storage-write-mat fs (%c-string-to-string name (length name)) value))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value string))
  (file-storage-write-string fs (%c-string-to-string name (length name)) 
		       (%c-string-to-string value (length value))))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value std-vector-key-point))
  (file-storage-write-vector-key-point fs (%c-string-to-string name (length name)) value))


(defmethod height ((self cv-rect))
  "Retrieves HEIGHT value of a RECT object."
  (mem-aref (c-pointer self) :int 3))


(defmethod (setf height) ((val integer) (self cv-rect))
  "Sets HEIGHT value of a RECT object."
  (rect-set-height self val))


(defmethod height ((self cv-size))
  "Retrieves HEIGHT value of a SIZE object."
  (size-height self))


(defmethod (setf height) ((val double-float) (self cv-size))
  "Sets HEIGHT value of a SIZE object."
  (size-set-height self val))


(defmethod is-opened ((self cv-video-capture))
	   (video-capture-is-opened self))


(defmethod is-opened ((self cv-video-writer))
	   (video-writer-is-opened self))


(defmethod load ((self cv-svm) &rest args)
  (apply #'stat-model-load self args))


(defmethod length ((self std-vector-char))
  (vec-char-length self))


(defmethod length ((self std-vector-dmatch))
  (vec-dmatch-length self))


(defmethod length ((self std-vector-double))
  (vec-double-length self))


(defmethod length ((self std-vector-float))
  (vec-float-length self))


(defmethod length ((self std-vector-int))
  (vec-int-length self))


(defmethod length ((self std-vector-key-point))
  (vec-key-point-length self))


(defmethod length ((self std-vector-mat))
  (vec-mat-length self))


(defmethod length ((self std-vector-point))
  (vec-point-length self))


(defmethod length ((self std-vector-point-2f))
  (vec-point-2f-length self))


(defmethod length ((self std-vector-rect))
  (vec-rect-length self))


(defmethod length ((self std-vector-uchar))
  (vec-uchar-length self))


(defmethod length ((self std-vector-vec-2d))
  (vec-vec-2d-length self))


(defmethod length ((self std-vector-vec-3d))
  (vec-vec-3d-length self))


(defmethod length ((self std-vector-vec-4d))
  (vec-vec-4d-length self))


(defmethod length ((self std-vector-vec-6d))
  (vec-vec-6d-length self))


(defmethod length ((self std-vector-vec-2f))
  (vec-vec-2f-length self))


(defmethod length ((self std-vector-vec-3f))
  (vec-vec-3f-length self))


(defmethod length ((self std-vector-vec-4f)) 
  (vec-vec-4f-length self))


(defmethod length ((self std-vector-vec-6f))
  (vec-vec-6f-length self))


(defmethod length ((self std-vector-vec-2i))
  (vec-vec-2i-length self))


(defmethod length ((self std-vector-vec-3i))
  (vec-vec-3i-length self))


(defmethod length ((self std-vector-vec-4i))
  (vec-vec-4i-length self))


(defmethod length ((self std-vector-vec-6i))
  (vec-vec-6i-length self))


(defmethod length ((self std-vector-vec-8i))
  (vec-vec-8i-length self))


(defmethod match ((self cv-bf-matcher) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod match ((self cv-brisk) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod match ((self cv-surf) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod open ((self cv-file-storage) &rest args)
  (apply #'file-storage-open self args))


(defmethod open (self &rest args)
  (apply #'cl::open self args))


(defmethod predict ((self cv-svm) &rest args)
  (apply #'svm-predict self args))


(defmethod release ((self cv-file-storage))
  (file-storage-release self))


(defmethod release ((self cv-video-capture))
  (video-capture-release self))


(defmethod save ((self cv-svm) &rest args)
  (apply #'stat-model-save self args))


(defmethod size ((arg cv-key-point) &rest args)
  args
  (key-point-size arg))


(defmethod (setf size) ((val single-float) (self cv-key-point))
  (key-point-set-size self val))


(defmethod size ((arg cv-mat) &rest args)
  args
  (mat-size arg))


(defmethod size ((arg cv-range) &rest args)
  args
  (range-size arg))


(defmethod size ((arg cv-rect) &rest args)
  args
  (rect-size arg))


(defmethod size ((arg cv-rotated-rect) &rest args)
  "Gets the size of a ROTATED-RECT object."
  args
  (rotated-rect-size arg))

;; Using mem-aref to access the member may not be safe if OpenCv changes something.
(defmethod (setf size) ((val cv-size) (self cv-rotated-rect))
  (setf (mem-aref (c-pointer self) :float 2) (coerce (mem-aref (c-pointer val) :int) 'single-float))
  (setf (mem-aref (c-pointer self) :float 3) (coerce (mem-aref (c-pointer val) :int 1) 'single-float))
  val)


(defmethod size ((arg null) &rest args)
  args
  (if (eq arg nil) (size-0) nil))


(defmethod size ((arg real) &rest args)
  (size-2 (coerce arg 'double-float) (coerce (or (first args) 0) 'double-float)))


(defmethod train ((self cv-svm) &rest args)
  (apply #'svm-train self args))


(defmethod width ((self cv-rect))
  "Retrieves WIDTH value of a RECT object."
  (rect-width self))


(defmethod (setf width) ((val integer) (self cv-rect))
  "Sets WIDTH value of a RECT object."
  (rect-set-width self val))


(defmethod width ((self cv-size))
  "Retrieves WIDTH value of a SIZE object."
  (size-width self))


(defmethod (setf width) ((val double-float) (self cv-size))
  "Sets WIDTH value of a SIZE object."
  (size-set-width self val))


(defmethod x ((self cv-key-point))
  "Retrieves X coordinate of a KEY-POINT object."
  (point-2f-x (key-point-pt self)))


(defmethod (setf x) ((val single-float) (self cv-key-point))
  "Sets X coordinate of a KEY-POINT object."
  (point-2f-set-x (key-point-pt self) val))


(defmethod x ((self cv-point))
  "Retrieves X coordinate of a POINT object."
  (point-x self))


(defmethod (setf x) ((val integer) (self cv-point))
  "Sets X coordinate of a POINT object."
  (point-set-x self val))


(defmethod x ((self cv-point-2d))
  "Retrieves X coordinate of a POINT-2D object."
  (point-2d-x self))


(defmethod (setf x) ((val double-float) (self cv-point-2d))
  "Sets X coordinate of a POINT-2D object."
  (point-2d-set-x self val))


(defmethod x ((self cv-point-2f))
  "Retrieves X coordinate of a POINT-2F object."
  (point-2f-x self))


(defmethod (setf x) ((val single-float) (self cv-point-2f))
  "Sets X coordinate of a POINT-2F object."
  (point-2f-set-x self val))


(defmethod x ((self cv-point-3d))
  "Retrieves X coordinate of a POINT-3D object."
  (point-3d-x self))


(defmethod (setf x) ((val double-float) (self cv-point-3d))
  "Sets X coordinate of a POINT-3D object."
  (point-3d-set-x self val))


(defmethod x ((self cv-point-3f))
  "Retrieves X coordinate of a POINT-3F object."
  (point-3f-x self))


(defmethod (setf x) ((val single-float) (self cv-point-3f))
  "Sets X coordinate of a POINT-3F object."
  (point-3f-set-x self val))


(defmethod x ((self cv-point-3i))
  "Retrieves X coordinate of a POINT-3I object."
  (point-3i-x self))


(defmethod (setf x) ((val integer) (self cv-point-3i))
  "Sets X coordinate of a POINT-3I object."
  (point-3i-set-x self val))


(defmethod x ((self cv-rect))
  (rect-x self))


(defmethod (setf x) ((val integer) (self cv-rect))
  "Sets x coordinate of a RECT object."
  (rect-set-x self val))


(defmethod y ((self cv-key-point))
  "Retrieves Y coordinate of a KEY-POINT object."
  (point-2f-y (key-point-pt self)))


(defmethod (setf y) ((val single-float) (self cv-key-point))
  "Sets Y coordinate of a KEY-POINT object."
  (point-2f-set-y (key-point-pt self) val))


(defmethod y ((self cv-point))
  "Retrieves Y coordinate of a POINT object."
  (point-y self))


(defmethod (setf y) ((val integer) (self cv-point))
  "Sets Y coordinate of a POINT object."
  (point-set-y self val))


(defmethod y ((self cv-point-2d))
  "Retrieves Y coordinate of a POINT-2D object."
  (point-2d-y self))


(defmethod (setf y) ((val double-float) (self cv-point-2d))
  "Sets Y coordinate of a POINT-2D object."
  (point-2d-set-y self val))


(defmethod y ((self cv-point-2f))
  "Retrieves Y coordinate of a POINT-2F object."
  (point-2f-y self))


(defmethod (setf y) ((val single-float) (self cv-point-2f))
  "Sets Y coordinate of a POINT-2F object."
  (point-2f-set-y self val))


(defmethod y ((self cv-point-3d))
  "Retrieves Y coordinate of a POINT-3D object."
  (point-3d-y self))


(defmethod (setf y) ((val double-float) (self cv-point-3d))
  "Sets Y coordinate of a POINT-3D object."
  (point-3d-set-y self val))


(defmethod y ((self cv-point-3f))
  "Retrieves Y coordinate of a POINT-3F object."
  (point-3f-y self))


(defmethod (setf y) ((val single-float) (self cv-point-3f))
  "Sets Y coordinate of a POINT-3F object."
  (point-3f-set-y self val))


(defmethod y ((self cv-point-3i))
  "Retrieves Y coordinate of a POINT-3I object."
  (point-3i-y self))


(defmethod (setf y) ((val integer) (self cv-point-3i))
  "Sets Y coordinate of a POINT-3I object."
  (point-3i-set-y self val))


(defmethod y ((self cv-rect))
  "Retrieves Y coordinate of a RECT object."
  (rect-y self))


(defmethod (setf y) ((val integer) (self cv-rect))
  "Sets y coordinate of a RECT object."
  (rect-set-y self val))


(defmethod z ((self cv-point-3d))
  "Retrieves Z coordinate of a POINT-3D object."
  (point-3d-z self))


(defmethod (setf z) ((val double-float) (self cv-point-3d))
  "Sets Z coordinate of a POINT-3D object."
  (point-3d-set-z self val))


(defmethod z ((self cv-point-3f))
  "Retrieves Z coordinate of a POINT-3F object."
  (point-3f-z self))


(defmethod (setf z) ((val single-float) (self cv-point-3f))
  "Sets Z coordinate of a POINT-3F object."
  (point-3f-set-z self val))


(defmethod z ((self cv-point-3i))
  "Retrieves Z coordinate of a POINT-3I object."
  (point-3i-z self))


(defmethod (setf z) ((val integer) (self cv-point-3i))
  "Sets Z coordinate of a POINT-3I object."
  (point-3i-set-z self val))


;;; Functions and methods used to 
;;; re-import shadowed symbols.


(defmethod abs ((self cv-mat))
  "Calculates an absolute value of each matrix element."
  (%abs self))


(defmethod abs ((self number))
  (cl:abs self))


(defun exp (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%exp args))
	(t (apply #'cl:exp args))))


(defun fill (arg &rest args)
  (cond ((or (listp arg) (vectorp arg))
	 (apply #'cl:fill arg args))
	((typep arg 'cv-rng)
	 (apply #'rng-fill arg args))))


(defun log (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%log args))
	(t (apply #'cl:log args))))


(defun max (&rest args)
  (cond ((realp (first args)) (apply #'cl:max args))
	((typep (first args) 'cv-mat) (apply #'%max args))))


(defun min (&rest args)
  (cond ((realp (first args)) (apply #'cl:min args))
	((typep (first args) 'cv-mat) (apply #'%min args))))


(defun read (&rest args)
  (cond ((typep (first args) 'cv-video-capture)
	 (apply #'video-capture-read args))
	(t (apply #'cl:read args))))


(defun set (&rest args)
  (cond ((symbolp (first args))
	 (apply #'cl:set args))
	((typep (first args) 'cv-video-capture) 
	 (video-capture-set (first args) (second args) (coerce (or (third args) 0d0) 'double-float)))))


(defun write (&rest args)

  (let ((val (second args)))

    (cond ((keywordp val)
	   (apply #'cl:write args))

	  ((null (cdr args))
	   (apply #'cl:write args))

	  ((typep val 'string) (apply #'file-storage-write args))

          ((typep val 'cv-mat) (apply #'video-writer-write args)))))


