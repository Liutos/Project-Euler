(defun pentagonal-p (n)
  (let ((rt (/ (1+ (sqrt (1+ (* 24 n)))) 6)))
    (= (truncate rt) rt)))

(defun ptg-num (i)
  (/ (* i (- (* 3 i) 1)) 2))

(defun maybe-d (idx)
  (let ((n (ptg-num idx)))
    (loop :for i :from (- idx 1) :downto 1
       :do (let ((m (ptg-num i)))
	     (if (and (pentagonal-p (- n m))
		      (pentagonal-p (+ n m)))
		 (return (- n m))))
       :finally (return nil))))

(defun pro44 ()
  (labels ((aux (i)
	     (let ((fnd-val (maybe-d i)))
	       (or fnd-val
		   (aux (1+ i))))))
    (aux 1)))