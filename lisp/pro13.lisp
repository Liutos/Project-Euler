(defpackage :pro13
  (:use :cl :iterate))

(in-package :pro13)

(defun pro13 ()
  (let ((sum 0))
    (iterate (for line in-file "pro13.txt" using #'read-line)
	     (incf sum (parse-integer line)))
    (subseq (format nil "~D" sum) 0 10)))