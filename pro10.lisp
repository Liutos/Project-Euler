(defun primep (number)
  (labels ((rec (n)
	     (if (> (* n n) number)
		 t
		 (if (zerop (rem number n))
		     nil
		     (rec (1+ n))))))
    (and (/= 1 number)
	 (rec 2))))

(defun pro10 ()
  (let ((sum 0))
    (loop
       :for n :from 1 :upto 2000000
       :do (if (primep n)
	       (incf sum n)))
    sum))