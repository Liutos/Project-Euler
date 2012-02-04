(defun primep (n)
  (labels ((rec (test-num)
	     (if (> (* test-num test-num) n)
		 t
		 (if (zerop (mod n test-num))
		     nil
		     (rec (1+ test-num))))))
    (rec 2)))

(defun largest-prime-factor (n)
  (labels ((rec (num acc)
	     (cond ((= 1 num) acc)
		   ((<= num 2) num)
		   (t (let ((fac (do ((i 2 (1+ i)))
				     ((or (>= i num)
					  (zerop (mod num i))) i))))
			(rec (/ num fac) fac))))))
    (rec n 1)))

(defun largest-prime-factor (n)
  (declare (number n))
  (let ((max 2))
    (declare (fixnum max))
    (loop for i from 3 to (- n 2) by 2
       do (if (zerop (mod n i))
	      (if (primep i)
		  (setq max i)))
       finally (return max))))