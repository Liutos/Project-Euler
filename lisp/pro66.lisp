(defun make-cf (n)
  (let ((a 0) (b 1) (cf-list nil))
    (flet ((update ()
	     (let ((m (truncate (/ (+ (sqrt n) a) b))))
	       (push m cf-list)
	       (psetf a (- (* m b) a)
		      b (/ (- n (expt (- a (* m b)) 2)) b))
	       (values a b m)))
	   (to-cf ()
	     (reduce #'(lambda (acc x)
			 (+ x (/ 1 acc)))
		     (rest cf-list) :initial-value (first cf-list)))
	   (cf-list ()
	     cf-list))
      (lambda (msg)
	(ecase msg
	  (update (update))
	  (to-cf (to-cf))
	  (cf-list (cf-list)))))))

(defun update-cf (cfc)
  (funcall cfc 'update))

(defun to-cf (cfc)
  (funcall cfc 'to-cf))

(defun get-cf-list (cfc)
  (funcall cfc 'cf-list))

(defun minx (d)
  (let ((cfc (make-cf d)))
    (labels ((aux (cfc)
	       (let* ((n (to-cf cfc))
		      (num (numerator n))
		      (den (denominator n)))
		 (cond ((= 1 (- (expt num 2)
				(* d (expt den 2)))) num)
		       (t
			(update-cf cfc)
			(aux cfc))))))
      (update-cf cfc)
      (aux cfc))))

(defun square-p (n)
  (let ((rt (sqrt n)))
    (= rt (truncate rt))))

(defun pro66 (&optional (bnd 1000))
  (let ((target-d)
	(score 0))
    (loop :for i :from 2 :to bnd
       :when (not (square-p i))
       :do (let ((tmps (minx i)))
	     (if (> tmps score)
		 (setf target-d i
		       score tmps))))
    (values target-d score)))