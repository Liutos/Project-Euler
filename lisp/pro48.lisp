(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (rem (expt (expmod base (/ exp 2) m) 2)
	      m))
	(t
	 (rem (* base (expmod base (- exp 1) m))
	      m))))

(defun pro48 (n)
  (let ((sum 0)
	(m (expt 10 10)))
    (loop
       :for i from 1 upto n
       :do (incf sum (expmod i i m)))
    sum))