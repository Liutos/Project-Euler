(proclaim '(optimize speed))

(defparameter *primes*
  '(2 3 5 7 11 13 17 19 23 29 31 41))

(defparameter *primes-list*
  (cons *primes* (last *primes*)))

(defun concat-prime (n)
  (nconc (cdr *primes-list*) `(,n))
  (setf (cdr *primes-list*)
	(cddr *primes-list*)))

(defun primep/list (n)
  (labels ((rec (test-list)
	     (let ((test (car test-list)))
	       (cond ((> (* test test) n)
		      (if (> n (cadr *primes-list*))
			  (concat-prime n))
		      t)
		     ((= 0 (rem n test)) nil)
		     (t (rec (cdr test-list)))))))
    (rec *primes*)))

(defun primep/+ (n)
  (labels ((rec (test)
	     (cond ((> (* test test) n) t)
		   ((= 0 (rem n test)) nil)
		   (t (rec (1+ test))))))
    (rec 2)))

(defun primep (n)
  (cond ((= 1 n) nil)
	((let ((test (cadr *primes-list*)))
	   (< n (* test test)))
	 (primep/list n))
	(t (primep/+ n))))

(defun squarep (n)
  (let ((sqrt (sqrt n)))
    (= sqrt (truncate sqrt))))

(defun helper (n p)
  (let ((r (- n p)))
    (and (evenp r) (squarep (/ r 2)) p)))

(defun pro46-test (n)
  (labels ((rec (primes test)
	     (cond (primes
		    (let ((p (car primes)))
		      (if (> p n)
			  nil
			  (or (helper n p)
			      (rec (cdr primes) p)))))
		   (t
		    (cond ((primep test)
			   (concat-prime test)
			   (if (helper n test)
			       test
			       (rec nil (+ test 2))))
			  (t
			   (rec nil (+ test 2))))))))
    (rec *primes* 2)))

(defun pro46 ()
  (labels ((rec (test)
	     (if (pro46-test test)
		 (rec (+ test 2))
		 test)))
    (rec 5)))