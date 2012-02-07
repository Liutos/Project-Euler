(defun pro16 (n)
  (labels ((rec (num sum)
	     (if (zerop num)
		 sum
		 (rec (truncate (/ num 10))
		      (+ sum (rem num 10))))))
    (rec n 0)))