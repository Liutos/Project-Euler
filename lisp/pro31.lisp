(defun coins-value (n)
  (nth (1- n) '(1 2 5 10 20 50 100 200)))

(defun cons-coins (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(t (+ (cons-coins amount
			  (1- kinds-of-coins))
	      (cons-coins (- amount
			     (coins-value kinds-of-coins))
			  kinds-of-coins)))))

(defun pro31 ()
  (cons-coins 200 8))