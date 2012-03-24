(defun divisor-sum (n)
  (labels ((rec (test acc)
	     (if (> (* test test) n)
		 acc
		 (if (zerop (mod n test))
		     (rec (1+ test) (+ acc test (/ n test)))
		     (rec (1+ test) acc)))))
    (rec 2 1)))

(defun amicable-number-p (num)
  (let ((tmp (divisor-sum num)))
    (and (/= tmp num)
	 (= num (divisor-sum tmp)))))

(defun pro21 ()
  (let ((sum 0))
    (loop :for i :from 1 :upto 10000
       :do (if (amicable-number-p i)
	       (incf sum i)))
    sum))