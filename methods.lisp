;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; methods.lisp
;;;; Methods for the OpenCV bindings


(in-package :lisp-cv)


;;; DEFGENERIC

(defgeneric abs (self)
  (:documentation "Used to call the binding for the OpenCV function 'abs' and CL:ABS."))

(defgeneric angle (self)
  (:documentation "Used for all class bindings with an ANGLE member."))

(defgeneric bounding-rect (self)
  (:documentation "Used for all class bindings with an ANGLE member."))

(defgeneric center (self)
  (:documentation "Used for all class bindings with an CENTER member."))

(defgeneric clone (self)
  (:documentation "Used for all class bindings with a CLONE member."))

(defgeneric compute (type self &rest args)
  (:documentation "Used for all class bindings with a COMPUTE member."))

(defgeneric create (type self &rest args)
  (:documentation "Used for all class bindings with a CREATE member."))

(defgeneric detect (self &rest args)
  (:documentation "Used for all class bindings with a DETECT member."))

(defgeneric descriptor-extractor-compute (self image keypoints descriptors)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-COMPUTE methods."))

(defgeneric descriptor-extractor-create (self descriptor-extractor-type)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-CREATE methods."))

(defgeneric descriptor-matcher-create (self descriptor-matcher-type)
  (:documentation "Used for all DESCRIPTOR-MATCHER-CREATE methods."))

(defgeneric descriptor-matcher-match (self query-descriptors train-descriptors matches &optional mask)
  (:documentation "Used for all DESCRIPTOR-MATCHER-MATCH methods.")) 

(defgeneric dot (self other)
  (:documentation "Used for all class bindings with a DOT member"))

(defgeneric feature-2d-compute (self image keypoints descriptors)
  (:documentation "Used for all FEATURE-2D-COMPUTE methods."))

(defgeneric feature-2d-create (self name)
  (:documentation "Used for all FEATURE-2D-CREATE methods."))

(defgeneric feature-detector-create (self detector-type)
  (:documentation "Used for all FEATURE-DETECTOR-CREATE methods."))

(defgeneric feature-detector-detect (self image keypoints &optional mask)
  (:documentation "Used for all FEATURE-DETECTOR-DETECT methods."))

(defgeneric *get (self value)
  (:documentation "Used for all class bindings with a GET member"))

(defgeneric height (self)
  (:documentation "Used for all class bindings with an HEIGHT member."))

(defgeneric is-opened (self)
  (:documentation "Used for all class bindings with an IS-OPENED member."))

(defgeneric file-storage-write (fs name value)
  (:documentation "Used for all FILE-STORAGE-WRITE methods."))

(defgeneric match (self &rest args)
  (:documentation "Used for all class bindings with a MATCH member."))

(defgeneric open (self &rest args)
  (:documentation "Used for all class bindings with a OPEN member."))

(defgeneric *read (self arg)
  (:documentation "Used for all class bindings with an READ member."))

(defgeneric release (self)
  (:documentation "Used for all class bindings with a RELEASE member."))

(defgeneric sqrt (arg &rest args)
  (:documentation "Used for all class bindings with a SIZE member."))

(defgeneric size (arg &rest args)
  (:documentation "Used for all class bindings with a SIZE member."))

(defgeneric width (self)
  (:documentation "Used for all class bindings with an WIDTH member."))

(defgeneric x (self)
  (:documentation "Used for all class bindings with an X member."))

(defgeneric y (self)
  (:documentation "Used for all class bindings with an Y member."))

(defgeneric z (self)
  (:documentation "Used for all class bindings with an Z member."))



;;;DEFMETHOD'S


(defmethod angle ((self cv-rotated-rect))
  "The rotation angle. When the angle is 0, 90, 180, 270 
    etc., the rectangle becomes an up-right rectangle."
  (rotated-rect-angle self))


(defmethod bounding-rect ((self cv-rotated-rect))
  "Returns the minimal up-right rectangle 
   containing the rotated rectangle."
  (rotated-rect-bounding-rect self))


(defmethod center ((self cv-rotated-rect))
  "The rectangle mass center."
  (rotated-rect-center self))


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


(defmethod height ((self cv-rect))
  (mem-aref (c-pointer self) :int 2))


(defmethod height ((self cv-size))
  (mem-aref (c-pointer self) :int 1))


(defmethod height ((self cv-size-2f))
  (mem-aref (c-pointer self) :float 1))


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


(defmethod is-opened ((self cv-video-capture))
	   (video-capture-is-opened self))

(defmethod is-opened ((self cv-video-writer))
	   (video-writer-is-opened self))


(defmethod *get ((self cv-video-capture) (value integer))
	   (video-capture-get self value))


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


(defmethod release ((self cv-file-storage))
  (file-storage-release self))


(defmethod release ((self cv-video-capture))
  (video-capture-release self))


(defmethod sqrt ((arg cv-mat) &rest args)
	   (%sqrt arg (first args)))


(defmethod sqrt ((arg number) &rest args)
  args
  (cl:sqrt arg))


(defmethod size ((arg cv-key-point) &rest args)
  args
  (mem-aref (c-pointer arg) :float 2))


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
  "Width and height of the rectangle."
  args
  (rotated-rect-size arg))


(defmethod size ((arg null) &rest args)
  args
  (if (eq arg nil) (size-0) nil))


(defmethod size ((arg real) &rest args)
  (size-2 (coerce arg 'double-float) (coerce (or (first args) 0) 'double-float)))


(defmethod width ((self cv-rect))
  (mem-aref (c-pointer self) :int 3))


(defmethod width ((self cv-size))
  (mem-aref (c-pointer self) :int))


(defmethod width ((self cv-size-2f))
  (mem-aref (c-pointer self) :float))


(defmethod x ((self cv-key-point))
  (mem-aref (c-pointer self) :float))


(defmethod x ((self cv-point))
 "Retrieves X coordinate of a POINT object."
  (mem-aref (c-pointer self) :int))


(defmethod x ((self cv-point-2d))
  "Retrieves X coordinate of a POINT-2D object."
  (mem-aref (c-pointer self) :double))


(defmethod x ((self cv-point-2f))
  "Retrieves X coordinate of a POINT-2F object."
  (mem-aref (c-pointer self) :float))


(defmethod x ((self cv-point-3d))
  "Retrieves X coordinate of a POINT-3D object."
  (mem-aref (c-pointer self) :double))


(defmethod x ((self cv-point-3f))
  "Retrieves X coordinate of a POINT-3F object."
  (mem-aref (c-pointer self) :float))


(defmethod x ((self cv-point-3i))
  "Retrieves X coordinate of a POINT-3I object."
  (mem-aref (c-pointer self) :int))


(defmethod x ((self cv-rect))
  (mem-aref (c-pointer self) :int))


(defmethod y ((self cv-key-point))
  (mem-aref (c-pointer self) :float 1))


(defmethod y ((self cv-point))
  "Retrieves Y coordinate of a POINT object."
  (mem-aref (c-pointer self) :int 1))


(defmethod y ((self cv-point-2d))
  "Retrieves Y coordinate of a POINT-2D object."
  (mem-aref (c-pointer self) :double 1))


(defmethod y ((self cv-point-2f))
  "Retrieves Y coordinate of a POINT-2F object."
  (mem-aref (c-pointer self) :float 1))


(defmethod y ((self cv-point-3d))
  "Retrieves Y coordinate of a POINT-3D object."
  (mem-aref (c-pointer self) :double 1))


(defmethod y ((self cv-point-3f))
  "Retrieves Y coordinate of a POINT-3F object."
  (mem-aref (c-pointer self) :float 1))


(defmethod y ((self cv-point-3i))
  "Retrieves Y coordinate of a POINT-3I object."
  (mem-aref (c-pointer self) :int 1))


(defmethod y ((self cv-rect))
  "Retrieves Y coordinate of a RECT object."
  (mem-aref (c-pointer self) :int 1))


(defmethod z ((self cv-point-3d))
  "Retrieves Z coordinate of a POINT-3D object."
  (mem-aref (c-pointer self) :double 2))


(defmethod z ((self cv-point-3f))
  "Retrieves Z coordinate of a POINT-3F object."
  (mem-aref (c-pointer self) :float 2))


(defmethod z ((self cv-point-3i))
  "Retrieves Z coordinate of a POINT-3I object."
  (mem-aref (c-pointer self) :int 2))



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


(defun log (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%log args))
	(t (apply #'cl:log args))))


(defun max (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%max args))
	(t (apply #'cl:max args))))


(defun min (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%min args))
	(t (apply #'cl:min args))))


(defun read (&rest args)
  (cond ((typep (first args) 'cv-video-capture)
	 (apply #'video-capture-read args))
	(t (apply #'cl:read args))))


(defun set (&rest args)
  (cond ((symbolp (first args))
	 (apply #'cl:set args))
	((typep (first args) 'cv-video-capture) 
	 (video-capture-set (first args) (second args) (coerce (or (third args) 0d0) 'double-float)))))
;

(defun write (&rest args)

  (let ((val (second args)))

    (cond ((keywordp val)
	   (apply #'cl:write args))

	  ((null (cdr args))
	   (apply #'cl:write args))

	  ((typep val 'string) (apply #'file-storage-write args))

          ((typep val 'cv-mat) (apply #'video-writer-write args)))))


