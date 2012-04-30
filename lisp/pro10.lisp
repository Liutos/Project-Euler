(declaim (optimize (speed 3)))

(defun primep (number)
  (labels ((rec (n)
	     (if (> (* n n) number)
		 t
		 (if (zerop (rem number n))
		     nil
		     (rec (1+ n))))))
    (rec 2)))

(defun pro10 ()
  (let ((sum 2))
    (loop
       :for n :from 3 :upto 2000000 :by 2
       :do (if (primep n)
	       (incf sum
		     (the fixnum n))))
    sum))