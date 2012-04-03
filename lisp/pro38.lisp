(proclaim '(optimize speed))

(declaim (optimize (debug 3)))

(defun mul-pandigital-p (number digits)
  (let ((aux (make-array '(9) :initial-element 0)))
    (labels ((fill-aux (number)
	       (if (/= 0 number)
		   (let ((lsb (rem number 10)))
		     (if (= 0 lsb) (return-from mul-pandigital-p nil))
		     (incf (aref aux (- lsb 1)))
		     (fill-aux (/ (- number lsb) 10)))))
	     (rec (lst)
	       (cond ((null lst)
		      (every #'(lambda (x) (= 1 x)) aux))
		     (t
		      (fill-aux (* number (car lst)))
		      (rec (cdr lst))))))
      (rec digits))))

(defun number-len (number)
  (labels ((rec (acc num)
	     (if (= 0 num)
		 acc
		 (rec (1+ acc) (truncate (/ num 10))))))
    (rec 0 number)))

(defun bnd-for-n (n)
  (labels ((rec (test)
	     (if (= 1 test) nil
		 (let ((mul-len (number-len (* n test))))
		   (if (> mul-len 9)
		       (rec (1- test))
		       (ceiling (/ 9 mul-len)))))))
    (rec 9)))

(defun gen1->n (n)
  (let ((seq (list)))
    (loop :for i :from 1 :upto n
       :do (push i seq))
    (nreverse seq)))

(defun mul-sum (n digits)
  (reduce #'(lambda (sum x)
	      (let ((len (number-len (* n x))))
		(+ (* (expt 10 len) sum) (* n x)))) digits :initial-value 0))

(defun mul-pandigital-p (number)
  (let ((aux (make-array '(9) :initial-element 0)))
    (labels ((rec (cur)
	       (if (= 0 cur)
		   (every #'(lambda (x) (= 1 x)) aux)
		   (let ((lsb (rem cur 10)))
		     (when (/= 0 lsb)
		       (incf (aref aux (1- lsb)))
		       (rec (/ (- cur lsb) 10)))))))
      (rec number))))

(defun pro38 ()
  (let ((max 0))
    (loop :for i :from 1 :upto 9999
       :do (let ((bnd (bnd-for-n i)))
	     (if bnd
		 (loop :for j :from 2 :upto bnd
		    :do (let* ((digits (gen1->n j))
			       (val (mul-sum i digits)))
			  (if (and (mul-pandigital-p val)
				   (> val max))
			      (setf max val)))))))
    max))