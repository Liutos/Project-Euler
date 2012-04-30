(defun help-test (n)
  (let ((pr (/ (1+ (sqrt (1+ (* 24 n)))) 6)))
    (and (= (truncate pr) pr)
	 (let ((hr (/ (1+ (sqrt (1+ (* 8 n)))) 4)))
	   (= (truncate hr) hr)))))

(defun pro45 ()
  (labels ((rec (index test)
	     (if (help-test test)
		 test
		 (rec (1+ index)
		      (+ test (1+ index))))))
    (rec 286 (+ 40755 285 1))))