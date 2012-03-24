(defun divisor-count (number)
  (let ((cnt 0))
    (dotimes (i (truncate (sqrt number)))
      (if (zerop (mod number (1+ i)))
	  (incf cnt 2)))
    cnt))

(defun triangle-number-p (number)
  (let ((root (truncate (/ (1- (sqrt (1+ (* 8 number)))) 2))))
    (= number
       (/ (* root (1+ root)) 2))))

(defun pro12 ()
  (labels ((rec (n)
	     (if (and (triangle-number-p n)
		      (> (divisor-count n) 500))
		 n
		 (rec (1+ n)))))
    (rec 28)))

(defun pro12 ()
  (labels ((rec (test inc)
	     (let ((cnt (divisor-count test)))
	       (if (> cnt 500)
		   test
		   (rec (+ test inc) (1+ inc))))))
    (rec 1 2)))