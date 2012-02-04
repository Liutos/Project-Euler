(defun difference (n)
  (- (expt (/ (* n (1+ n)) 2) 2)
     (loop for i from 1 upto n summing (expt i 2))))