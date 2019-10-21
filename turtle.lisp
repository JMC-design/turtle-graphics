(defpackage :turtle-graphics
  (:use :cl)
  (:nicknames :turtle))
(in-package :turtle)

(defconstant +half-pi+ (/ pi 2))
(defconstant +2pi+ (* pi 2))

(defparameter *composite-p* t "When True composites lines onto final drawing.")
(defparameter *always-flush* nil "When False only flushes the clx display buffer in TO functions.")
(defparameter *stroke-width* 1.0)
(defparameter *turtle* nil)
(defparameter *surface* nil)
(defparameter *cursor* (make-array 117 :element-type '(unsigned-byte 8)
				       :initial-contents '(00 00 00 00 #xFF 00 00 00 00
							   00 00 00 00 #xFF 00 00 00 00
							   00 00 00 #xAF #xFF #xAF 00 00 00
							   00 00 00 #xFF 00 #xFF 00 00 00
							   00 00 00 #xFF 00 #xFF 00 00 00
							   00 00 #xAF #xFF 00 #xFF #xAF 00 00
							   00 00 #xFF 00 00 00 #xFF 00 00
							   00 00 #xFF 00 00 00 #xFF 00 00
							   00 #xAF #xFF 00 #xFF 00 #xFF #xAF 00
							   00 #xFF 00 00 00 00 00 #xFF 00
							   00 #xFF 00 00 00 00 00 #xFF 00
							   #xAF #xFF 00 00 #xAF 00 00 #xFF #xAF
							   #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))

(defun init (&optional (width 600) (height 600) (colour #(#xFFFF #xFFFF #xFFFF #xFFFF)))
  (unless xwindows:*display* (xwindows::init-default-display)) ;FIXME i'm a hack.
  (setf *surface* (surface:get-surface :width width :height height :depth 32)
	*turtle* (make-turtle :location (cons (floor width 2) (floor height 2)) :angle 0 :pen (make-pen colour) :drawp t))
  (surface:prepare-surface *surface*)
  (surface:map-surface *surface*))

(defun stop ()
  (surface:destroy-surface *surface*)
  (xlib:render-free-picture (turtle-pen *turtle*))
  (setf *turtle* nil *surface* nil))

(defmacro with-turtle (turtle &rest body)
  `(with-slots (location angle pen drawp representation)
       ,turtle ,@body))

(defstruct turtle
  location
  angle
  pen
  drawp
  representation)

;;;; Utils
(declaim (inline calc-destination deg->rad rad->deg))
(defun calc-destination (location angle units)
  (declare (optimize speed))
  (let ((rads (deg->rad angle)))
    (destructuring-bind (x . y) location
      (cons (the double-float (+ x (* units (sin rads)))) (the double-float (+ y (* units (cos rads))))))))
(defun deg->rad (degrees)
  (* pi (/ degrees 180.0)))
(defun rad->deg (rads)
  (* (/ rads 2 pi) 360))

;;;; The workhorse, no butts.
(defun draw-line (surface pen x1 y1 x2 y2 &optional (thickness *stroke-width*))
  (declare (optimize speed)
	   (type (or integer double-float) thickness x1 x2 y1 y2))
  (let ((delta (/ thickness 2))
	(angle (- (turtle-angle *turtle*) 90))
	p1x p1y p2x p2y p3x p3y p4x p4y)
    (cond
      ;;Vertical line
      ((= x1 x2) (setf p1x (- x1 delta)
		       p1y y1
		       p2x (+ x1 delta)
		       p2y y1
		       p3x (- x2 delta)
		       p3y y2
		       p4x (+ x2 delta)
		       p4y y2))
      ;;Horizontal line
      ((= y1 y2)(setf p1x x1
		      p1y (- y1 delta)
		      p2x x1
		      p2y (+ y1 delta)
		      p3x x2
		      p3y (- y2 delta)
		      p4x x2
		      p4y (+ y2 delta)))
      (t (let ((p1 (calc-destination (cons x1 y1) angle delta))
	       (p2 (calc-destination (cons x1 y1) angle (- delta)))
	       (p3 (calc-destination (cons x2 y2) angle delta))
	       (p4 (calc-destination (cons x2 y2) angle (- delta))))
	   (setf p1x (car p1)
		 p1y (cdr p1)
		 p2x (car p2)
		 p2y (cdr p2)
		 p3x (car p3)
		 p3y (cdr p3)
		 p4x (car p4)
		 p4y (cdr p4)))))
    (xlib:render-triangle-strip (xlib:render-create-picture surface) :over pen 0 0 :none (mapcar (lambda (x) (coerce (realpart x) 'double-float)) (list p1x p1y p2x p2y p3x p3y p4x p4y))))
  (when *always-flush* (surface:update *surface*)))

;;;; Basic Relative Turtle commands
(defun forward (units)
  (with-turtle *turtle*
    (let ((destination (calc-destination location angle units)))
      (when drawp
	(draw-line *surface* pen (car location) (cdr location) (car destination) (cdr destination)))
      (setf location destination))))
(defun back (units)
  (forward (- units)))
(defun right (degrees)
  (with-turtle *turtle*
    (setf angle (mod (- angle degrees) 360))))
(defun left (degrees)
  (right (- degrees)))

;;;; Pen Basics
(defun pen-up ()
  (setf (turtle-drawp *turtle*) nil))
(defun pen-down ()
  (setf (turtle-drawp *turtle*) t))
(defun pen-colour (r g b a)
  (set-pen (turtle-pen *turtle*) (make-array 4 :initial-contents (mapcar (lambda (x) (* x a)) (list r g b a) ))))
(defun pen-width (width)
  (setf *stroke-width* width))

;;;; Absolute basics
(defun center ()
  (destructuring-bind (w . h) (surface:size *surface*)
    (setf (turtle-location *turtle*) (cons (floor w 2) (floor h 2))
	  (turtle-angle *turtle*) 0)))
(defun move-to (x y)
  (setf (turtle-location *turtle*) (cons x y)))
(defun location ()
  (turtle-location *turtle*))
(defun heading ()
  (turtle-angle *turtle*))
(defun clear ()
  (xlib:clear-area *surface* )
  (xlib:display-finish-output xwindows:*display*))
(defun reset ()
  (center)
  (clear))

;;;; Procedure macros
(defmacro repeat (times &rest rest)
  (typecase times
    (keyword (case :forever `(loop :for repeat-counter :from 0 :do (progn ,@rest))))
    (t `(dotimes (repeat-counter ,times) ,@rest))))

(defmacro to (name vars &rest body)
  `(defun ,name ,vars ,@body (unless *always-flush* (xlib:display-finish-output xwindows:*display*) )))

;;;;RGBA Colour helpers
(defun make-pen (colour)
  (let ((pic (xlib:render-create-picture (surface:get-surface :width 1 :height 1 :type :x-pixmap :depth 32) :format (xlib:find-window-picture-format *surface*) :repeat :on)))
    (xlib:render-fill-rectangle pic :over colour 0 0 1 1)
    pic))
(defun set-pen (pen colour)
  (xlib:render-fill-rectangle pen :src #(0 0 0 0) 0 0 1 1)
  (xlib:render-fill-rectangle pen :over colour 0 0 1 1))
(defun random-colour (&optional (min 128))
  (colour (+ min (random (- 255 min))) (+ min (random (- 255 min))) (+ min (random (- 255 min))) 255))
(defun background-colour (r g b a)
  ())

#-LIFE
(progn
  (defun swap-rb-flat (array)
    (let ((size (car (array-dimensions array))))    
      (loop :for r fixnum :from 0 :upto size :by 4
	    :for b fixnum :from 2 :upto size :by 4 :do
	      (rotatef (aref array r) (aref array b)))))
  (defun save (filename)
    (let ((data (surface:get-data *surface*)))
      (swap-rb-flat data)
      (destructuring-bind (width . height) (surface:size *surface*)
	(zpng:write-png (make-instance 'zpng:png
				       :color-type :truecolor-alpha
				       :width width
				       :height height
				       :image-data data)
			filename))))
  (defun load-documentation (system lang-code)
    "Loads the appropriate language documentation according to two char language code."
    (let* ((dir (asdf:system-source-directory system))
	   (file (probe-file (merge-pathnames (concatenate 'string "documentation-" (symbol-name lang-code) ".lisp") dir))))
      (when file (load file)))))
(load-documentation :turtle-graphics :en)


;;;; examples

(defun ldragon (size level)
  (random-colour 20)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))(if (< size 1) 10 (random size)) 
      (left 90)
      (rdragon size (- level 1)))))

(defun rdragon (size level)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))
      (right 90)
      (rdragon size (- level 1)))))

