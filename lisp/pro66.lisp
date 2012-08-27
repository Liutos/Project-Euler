(defparameter *sq*
  (make-hash-table))

(defun sqnum-p (n)
  (multiple-value-bind (val fnd)
      (gethash n *sq*)
    (if fnd val
	(let ((rt (sqrt n)))
	  (setf (gethash n *sq*)
		(= n (expt (truncate rt) 2)))))))

(defun minx (d)
  (let ((start (ceiling (sqrt (1+ d)))))
    (loop :for x :from start :to (expt 10 8)
       :when (let ((tmp (1- (expt x 2))))
	       (and (zerop (rem tmp d))
		    (sqnum-p (/ tmp d))))
       :do (return x)
       :finally (error "~&Solution not found~%"))))

(defun pro66 (&optional (bnd 1000))
  (let ((cd 0)
	(dx 0))
    (loop :for d :from 2 :to bnd
       :when (not (sqnum-p d))
       :do (let ((tmpx (minx d)))
	     (if (> tmpx dx)
		 (setf cd d dx tmpx)))
       :finally (return (cons cd dx)))))