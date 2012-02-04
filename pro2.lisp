(defun even-fib-sum (limit)
  (labels ((rec (st nd sum)
	     (if (> nd limit)
		 sum
		 (rec nd (+ st nd)
		      (if (evenp nd)
			  (+ sum nd)
			  sum)))))
    (rec 1 2 0)))