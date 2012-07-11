(defun nth-component (n)
  (cond ((= 1 n) 2)
	((= 0 (mod n 3)) (* 2 (/ n 3)))
	(t 1)))

(defun nth-convergent (n)
  (labels ((rec (acc m)			;acc表示从这一次往后的所有组成部分经过运算后得到的结果，只要直接和目前的组成部分相加就可以了。
	     (cond ((= 1 m) (+ 2 acc))
		   ((= 2 m) (rec (/ 1 (+ 1 acc)) (- m 1)))
		   (t (rec (/ 1 (+ (nth-component m) acc)) ;acc的直接可用性由这里的倒数操作保证
			   (- m 1))))))
    (rec 0 n)))

(defun digit-sum (number)
  (labels ((rec (acc n)
	     (if (= 0 n)
		 acc
		 (let ((rem (rem n 10)))
		   (rec (+ acc rem)
			(/ (- n rem) 10))))))
    (rec 0 number)))

(defun pro65 ()
  (digit-sum (numerator (nth-convergent 100))))









