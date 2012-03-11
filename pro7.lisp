(defun primep (number)
  (labels ((rec (n)
	     (if (> (* n n) number)
		 t
		 (if (zerop (rem number n))
		     nil
		     (rec (1+ n))))))
    (and (/= 1 number)
	 (rec 2))))

(defun pro7 (lim)
  (labels ((rec (test cnt)
	     (if (= lim cnt)
		 (1- test)
		 (if (primep test)
		     (rec (1+ test) (1+ cnt))
		     (rec (1+ test) cnt)))))
    (rec 2 0)))