(proclaim '(optimize speed))

(defun primep (number)
  (labels ((rec (test)
	     (cond ((> (* test test) number) t)
		   ((= 0 (rem number test)) nil)
		   (t (rec (1+ test))))))
    (cond ((= 1 number) nil)
	  ((= 2 number) t)
	  (t (rec 2)))))

(defun cons-fn (a b)
  (lambda (n)
    (+ (* n n) (* a n) b)))