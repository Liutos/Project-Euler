(proclaim '(optimize speed))

(defun 0->9-pandigital-p (number)
  (let ((aux (make-array '(10) :initial-element 0)))
    (labels ((rec (test)
	       (cond ((= 0 test)
		      (every #'(lambda (x) (= 1 x)) aux))
		     (t
		      (incf (aref aux (rem test 10)))
		      (rec (truncate (/ test 10)))))))
      (rec number))))

(defun 3-subnum (number start)
  (parse-integer (format nil "~D" number)
		 :start start
		 :end (+ start 3)))

(defun help-test (number)
  (and (0->9-pandigital-p number)
       (every #'(lambda (x prime)
		  (= 0 (rem (3-subnum number x) prime)))
	      '(7 6 5 4 3 2 1)
	      '(17 13 11 7 5 3 2))))

(defun pro43 ()
  (let ((sum 0))
    (do ((i 1245789306 (+ i 17)))
	((> i 9876543210) sum)
      (if (help-test i)
	  (incf sum i)))))