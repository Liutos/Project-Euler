(defun palindromicp (n)
  (let ((rev (parse-integer (reverse (format nil "~D" n)))))
    (= rev n)))

(defun n->2base (n)
  (labels ((rec (num)
	     (if (= 0 num)
		 0
		 (+ (* 10 (rec (truncate (/ num 2))))
		    (rem num 2)))))
    (rec n)))

(defun both-palindromic-p (n)
  (and (palindromicp n)
       (palindromicp (n->2base n))))

(defun pro36 ()
  (loop :for i :from 1 :upto 1000000 :by 2
     :when (both-palindromic-p i) :summing i))