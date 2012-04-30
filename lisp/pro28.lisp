;;; The recursive version
(defun helix-sum (n)
  (if (= 1 n)
      (values 1 1)
      (multiple-value-bind (sum prev) (helix-sum (1- n))
	(let ((len (1- (* 2 n)))
	      (rb-value (+ prev (* 2 (1- n)))))
	  (values (+ sum (* 4 rb-value) (* 6 (1- len)))
		  (+ rb-value (* 3 (1- len))))))))

;;; The iterative version
(defun helix-sum (n)
  (labels ((rec (acc layer prev)
	     (if (> layer n)
		 acc
		 (let ((len (1- (* 2 layer)))
		       (rb-value (+ prev (* 2 (1- layer)))))
		   (rec (+ acc (* 4 rb-value) (* 6 (1- len)))
			(1+ layer)
			(+ rb-value (* 3 (1- len))))))))
    (if (= 1 n) 1 (rec 1 2 1))))

(defun pro28 (n)
  (helix-sum (/ (1+ n) 2)))