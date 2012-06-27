(defun make-queue ()
  (cons nil nil))

(defun empty-queue-p (queue)
  (null (car queue)))

(defun enqueue (value queue)
  (cond ((empty-queue-p queue)
	 (setf (car queue) (list value))
	 (setf (cdr queue) (car queue)))
	(t
	 (nconc (cdr queue) (list value))
	 (setf (cdr queue) (cddr queue)))))

(defun primep (n)
  (cond ((< n 2) nil)
	((= 2 n) t)
	(t
	 (let ((bnd (truncate (sqrt n))))
	   (labels ((rec (test)
		      (cond ((> test bnd) t)
			    ((= 0 (rem n test)) nil)
			    (t (rec (1+ test))))))
	     (rec 2))))))

(defparameter *primes*
  (let ((queue (make-queue)))
    (setf (car queue) '(2 3 5 7 11 13 17 19)
	  (cdr queue) (last (car queue)))
    queue))

;;; 简单地判断是否可以表示成质数之和
(defun primes-sum-p (n prime-list)
  (cond ((< n 0) nil)
	((= 0 n) t)
	((null prime-list) nil)
	(t (primes-sum-p (- n (car prime-list))
			 (cdr prime-list)))))

;;; 对于能够表示成质数之和的数打印出对应的质数序列
(defun primes-sum-p/show (n prime-list)
  (cond ((< n 0) nil)
	((= 0 n) t)
	((null prime-list) nil)
	((primes-sum-p/show (- n (car prime-list))
			    (cdr prime-list))
	 (format t "~D + " (car prime-list))
	 t)))

;;; 对于能够表示成质数之和的数同时返回这些质数的个数
(defun primes-sum-p/acc (n prime-list)
  (labels ((rec (acc n list)
	     (cond ((< n 0) nil)
		   ((= 0 n) (values t acc))
		   ((null list) nil)
		   (t (rec (1+ acc)
			   (- n (car list))
			   (cdr list))))))
    (rec 0 n prime-list)))

(defun sum-test (n)
  (labels ((rec (test-list)
	     (if (null test-list) nil
		 (multiple-value-bind (val cnt)
		     (primes-sum-p/acc n test-list)
		   (if val cnt
		       (rec (cdr test-list)))))))
    (rec (car *primes*))))

(defun pro50 (limit)
  (let ((max)
	(win 0))
    (dotimes (i limit)
      (let ((i (1+ i)))
	(when (primep i)
	  (when (> i (car (last (cdr *primes*))))
	    (enqueue i *primes*))
	  (let ((result (sum-test i)))
	    (when (and result (> result win))
	      (setf max i
		    win result))))))
    (values max win)))