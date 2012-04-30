;; (defun nth-sqrt2 (n)
;;   (labels ((rec (n)
;; 	     (if (= 1 n)
;; 		 2
;; 		 (+ 2 (/ 1 (rec (- n 1)))))))
;;     (1+ (/ 1 (rec n)))))
(proclaim '(optimize speed))

(defun nth-sqrt2 (n)
  (labels ((rec (acc n)
	     (if (= 1 n)
		 acc
		 (rec (+ 2 (/ 1 acc)) (- n 1)))))
    (1+ (/ 1 (rec 2 n)))))

(defun number-length (number)
  (labels ((rec (acc n)
	     (if (= 0 n)
		 acc
		 (rec (1+ acc) (truncate (/ n 10))))))
    (rec 0 number)))

(defun object-ratio-p (ratio)
  (let ((n (numerator ratio))
	(d (denominator ratio)))
    (> (number-length n)
       (number-length d))))

(defun pro57 ()
  (labels ((rec (acc n)
	     (cond ((> n 1000) acc)
		   ((object-ratio-p (nth-sqrt2 n))
		    (rec (1+ acc) (1+ n)))
		   (t (rec acc (1+ n))))))
    (rec 0 1)))