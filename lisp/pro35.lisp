(proclaim '(optimize speed))

(defun primep (number)
  (labels ((rec (test)
	     (if (> (* test test) number)
		 t
		 (if (zerop (rem number test))
		     nil
		     (rec (1+ test))))))
    (and (/= 1 number)
	 (rec 2))))

(defun num-len (num)
  (labels ((rec (acc rest)
	     (if (zerop rest)
		 acc
		 (rec (1+ acc) (truncate (/ rest 10))))))
    (if (zerop num) 1 (rec 0 num))))

(defun next-circular (number)
  (let* ((len (num-len number))
	 (lsb (rem number 10))
	 (prt (/ (- number lsb) 10)))
    (+ (* (expt 10 (1- len)) lsb) prt)))

(defun circular-prime-p (number)
  (labels ((rec (test)
	     (if (= number test)
		 t
		 (if (primep test)
		     (rec (next-circular test))
		     nil))))
    (and (primep number)
	 (rec (next-circular number)))))

(defun all-digit-odd-p (number)
  (labels ((rec (test)
	     (if (zerop test)
		 t
		 (let ((r (rem test 10)))
		   (if (evenp r)
		       nil
		       (rec (/ (- test r) 10)))))))
    (and (/= 0 number)
	 (rec number))))

(defun pro35 (n)
  (cond ((<= n 2) 1)
	(t (let ((cnt 0))
	     (dotimes (i n)
	       (if (and (all-digit-odd-p (1+ i))
			(circular-prime-p (1+ i)))
		   (incf cnt)))
	     (1+ cnt)))))