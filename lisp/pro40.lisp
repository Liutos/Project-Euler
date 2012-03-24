(defun num-length (num)
  (labels ((rec (n len)
	     (if (<= n 0)
		 len
		 (rec (truncate (/ n 10)) (1+ len)))))
    (rec num 0)))

(defun helper (n)
  (labels ((rec (index test-num)
	     (let ((len (num-length test-num)))
	       (if (<= index len)
		   (- (char-code (char (format nil "~D" test-num) (1- index)))
		      (char-code #\0))
		   (rec (- index len) (1+ test-num))))))
    (rec n 1)))

(defun pro40 ()
  (apply #'* (mapcar #'helper (mapcar #'(lambda (e)
					  (expt 10 e))
				      '(0 1 2 3 4 5 6)))))