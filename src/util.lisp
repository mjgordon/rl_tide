(in-package :rl)

;;; GRID MATH

(defparameter *dx* '(0 1 1 1 0 -1 -1 -1))
(defparameter *dy* '(-1 -1 0 1 1 1 0 -1))

(defun cart-to-loc (x y)
  (+ x (* y rl-graphics::*terminal-width*)))

(defun rect-bounds-p (x y w h)
  (and (>= x 0)
       (>= y 0)
       (< x w)
       (< y h)))

;;; DEBUG

(defun trace-dummy ())

(defun get-stack-depth()
  (trace trace-dummy)
  (trace-dummy)
  (untrace trace-dummy))

(defun print-hash-table (ht)
  (maphash (lambda (k v)
	     (format t "~a : ~a~%" k v))
	   ht))

;;; LISTS

(defun get-bounds (input &optional (ignore nil))
  (let ((lower nil)
	(upper nil))
    (mapc (lambda (n)
	    (when (and n
		       (or (equal lower nil)
			   (< n lower)))
	      (setf lower n))
	    (when (and n
		       (or (equal upper nil)
			   (> n upper)))
	      (setf upper n)))
	  input)
    (values lower upper)))
	    

;;; SETS

(defun array-set-counts (input)
  (let ((counts (make-hash-table)))
    (loop for i from 0 below (array-total-size input) do
	 (let ((n (row-major-aref input i)))
	   (if (gethash n counts)
	       (incf (gethash n counts))
	       (setf (gethash n counts) 1))))
    counts))

;;; SYMBOLS

(defun concatenate-symbols (a b)
  "Combines the string representation of a and b with a hyphen and returns a new symbol"
  (intern (concatenate 'string (symbol-name a) "-" (symbol-name b))))



