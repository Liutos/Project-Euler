(defun pro17 (n)
  (let ((cnt 0))
    (loop
       :for i from 1 upto n
       :do (incf cnt
		 (+ (count-if #'(lambda (c)
				  (and (char/= #\Space c)
				       (char/= #\- c)))
			      (format nil "~R" i))
		    (if (and (> i 100) (/= 0 (rem i 100))) 3 0))))
    cnt))