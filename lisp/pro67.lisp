(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(defpackage :pro67
  (:use :cl :iterate :cl-ppcre))

(in-package :pro67)

(defparameter *aux*
  (make-array '(100 100) :initial-element nil))

(defun read-array ()
  (let ((ary (make-array '(100 100) :initial-element 0))
	(i 0))
    (iterate (for row in-file "triangle.txt" using #'read-line)
	     (let* ((num-list (split " " row))
		    (list-len (length num-list)))
	       (dotimes (j list-len)
		 (setf (aref ary i j)
		       (parse-integer (nth j num-list))))
	       (incf i)))
    ary))

(defvar *tbl* (read-array))

(proclaim '(optimize speed))

(defun max-route-sum/dynpro (table)
  (let* ((d (array-dimension table 0))
	 (aux (make-array `(,d ,d) :initial-element nil)))
    (labels ((rec (i j)
	       (if (= (1- d) i)
		   (aref table i j)
		   (let ((cache (aref aux i j)))
		     (or cache
			 (let* ((tmp (max (rec (1+ i) j)
					  (rec (1+ i) (1+ j))))
				(val (+ (aref table i j) tmp)))
			   (setf (aref aux i j) val)
			   val))))))
      (rec 0 0))))

(defun pro67 ()
  (max-route-sum/dynpro *tbl*))