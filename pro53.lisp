(defun fac (n)
  (let ((p 1))
    (dotimes (i n p)
      (setf p (* p (1+ i))))))

(defun selects (n r)
  (/ (fac n)
     (* (fac r) (fac (- n r)))))

(defun pro53 ()
  (let ((cnt 0))
    (loop
       :for i from 1 upto 100
       :do (dotimes (j i)
	     (if (> (selects i (1+ j)) 1000000)
		 (incf cnt))))
    cnt))