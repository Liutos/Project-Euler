(defun fact (n)
  (labels ((rec (m prod)
	     (if (= 1 m)
		 prod
		 (rec (- m 1) (* prod m)))))
    (rec n 1)))

(defun pro20 (n)
  (reduce #'(lambda (init c)
	      (+ init
		 (- (char-code c) (char-code #\0))))
	  (format nil "~A" n)
	  :initial-value 0))