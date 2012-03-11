;;; Iterative version
(defun pro1 (num)
  (let ((sum 0))
    (loop for i from 1 to (1- num)
       do (if (or (zerop (mod i 3))
		  (zerop (mod i 5)))
	      (incf sum i)))
    sum))
;;; Recursive version
(defun pro1 (num)
  (labels ((rec (n sum)
	     (if (>= n num)
		 sum
		 (if (or (zerop (mod n 3))
			 (zerop (mod n 5)))
		     (rec (1+ n) (+ sum n))
		     (rec (1+ n) sum)))))
    (rec 1 0)))