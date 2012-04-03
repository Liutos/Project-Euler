(defpackage :pro11
  (:use :cl :iterate :cl-ppcre))

(in-package :pro11)

(declaim (optimize (debug 3)))

(defun read-array (filespec)
  (let ((lst '()))
    (iterate (for line in-file filespec using #'read-line)
	     (push (mapcar #'parse-integer
			   (split " " line)) lst))
    (make-array '(20 20) :initial-contents (nreverse lst))))

(defun down-value (table i j)
  (if (> i 16) 0
      (* (aref table i j) (aref table (+ i 1) j)
	 (aref table (+ i 2) j) (aref table (+ i 3) j))))
(defun right-value (table i j)
  (if (> j 16) 0
      (* (aref table i j) (aref table i (+ j 1))
	 (aref table i (+ j 2)) (aref table i (+ j 3)))))
(defun rb-value (table i j)
  (if (or (> j 16) (> i 16)) 0
      (* (aref table i j) (aref table (+ i 1) (+ j 1))
	 (aref table (+ i 2) (+ j 2)) (aref table (+ i 3) (+ j 3)))))
(defun lb-value (table i j)
  (if (or (< j 4) (> i 16)) 0
      (* (aref table i j) (aref table (+ i 1) (- j 1))
	 (aref table (+ i 2) (- j 2)) (aref table (+ i 3) (- j 3)))))

(defun pro11 (table)
  (let ((d (array-dimension table 0))
	(max 0))
    (dotimes (i d)
      (dotimes (j d)
	(let ((new-max (max (down-value table i j)
			    (right-value table i j)
			    (rb-value table i j)
			    (lb-value table i j))))
	  (if (> new-max max) (setf max new-max)))))
    max))