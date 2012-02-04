(defun collatz-length (n)
  (labels ((rec (m times)
	     (if (= 1 m)
		 times
		 (if (evenp m)
		     (rec (/ m 2) (1+ times))
		     (rec (1+ (* 3 m)) (1+ times))))))
    (rec n 1)))

(defun pro14 (n)
  (let ((max nil)
	(win 0))
    (loop
       :for i from 2 upto n
       :do (let ((len (collatz-length i)))
	     (if (> len win)
		 (setf max i
		       win len))))
    (values max win)))