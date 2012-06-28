(proclaim '(optimize speed))

(defparameter *primes*
  (make-hash-table))

(defun primep (n)
  (declare (type integer n))
  (the boolean
    (multiple-value-bind (value found)
	(gethash n *primes*)
      (if found value
	  (let ((value (cond ((>= 1 n) nil)
			     ((= 2 n) t)
			     (t (let ((limit (truncate (sqrt n))))
				  (labels ((aux (test)
					     (cond ((> test limit) t)
						   ((zerop (rem n test)) nil)
						   (t (aux (1+ test))))))
				    (aux 2)))))))
	    (when value
	      (setf (gethash n *primes*) t))
	    value)))))

(defun make-fn (a b)
  (declare (type (integer -999 999) a b))
  (the function
    (lambda (n)
      (+ (* n (+ n a)) b))))

(defun prime-count (fn)
  (declare (type function fn))
  (the fixnum
    (labels ((aux (acc n)
	       (if (primep (funcall fn n))
		   (aux (1+ acc) (1+ n))
		   acc)))
      (aux 0 0))))

(defun pro27 ()
  (let (max
	(win 0))
    (dotimes (a 1999)
      (dotimes (b 1000)
	(let ((a (- a 999)))
	  (let ((count (prime-count (make-fn a b))))
	    (when (> count win)
	      (setf max (* a b)
		    win count))))))
    (values max win)))