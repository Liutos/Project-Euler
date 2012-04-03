(defun smallest-factor (n)
  (labels ((rec (test)
	     (if (or (>= test n) (zerop (rem n test)))
		 test
		 (rec (1+ test)))))
    (rec 2)))

(defun largest-prime-factor (n)
  (labels ((rec (num acc)
	     (cond ((= 1 num) acc)
		   ((<= num 2) num)
		   (t (let ((fac (smallest-factor num)))
			(rec (/ num fac) fac))))))
    (rec n 1)))