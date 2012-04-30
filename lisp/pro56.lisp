(defun digit-sum (n)
  (labels ((rec (sum num)
	     (if (= 0 num)
		 sum
		 (rec (+ sum (rem num 10))
		      (truncate (/ num 10))))))
    (rec 0 n)))

(defun pro56 ()
  (let ((max 0) (win 0))
    (dotimes (a 100)
      (dotimes (b 100)
	(let* ((n (if (= 0 (rem (1+ a) 10))
		      (expt (/ (1+ a) 10) (1+ b))
		      (expt (1+ a) (1+ b))))
	       (score (digit-sum n)))
	  (if (> score win)
	      (setf max n
		    win score)))))
    (values max win)))