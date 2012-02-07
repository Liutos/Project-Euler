(defun fact (n)
  (labels ((rec (m prod)
	     (if (= 1 m)
		 prod
		 (rec (- m 1) (* prod m)))))
    (rec n 1)))

(defun pro20 (n)
  (labels ((rec (num sum)
	     (if (zerop num)
		 sum
		 (rec (truncate (/ num 10))
		      (+ sum (rem num 10))))))
    (rec n 0)))