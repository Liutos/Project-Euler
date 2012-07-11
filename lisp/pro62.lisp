(defun cubed-number-p (number)
  (let ((n (expt number (/ 1 3))))
    (= (expt (truncate n) 3) number)))

()