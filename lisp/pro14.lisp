(defun collatz-length (n)
  (labels ((rec (m times)
	     (if (= 1 m)
		 times
		 (if (evenp m)
		     (rec (/ m 2) (1+ times))
		     (rec (1+ (* 3 m)) (1+ times))))))
    (rec n 1)))

(defun pro14 (n)
  (let ((max nil)
	(win 0))
    (loop
       :for i from 2 upto n
       :do (let ((len (collatz-length i)))
	     (if (> len win)
		 (setf max i
		       win len))))
    (values max win)))
;;; The version used the hash table
(defparameter *collatz*
  (make-hash-table :test #'eql))

(setf (gethash 1 *collatz*) 1)

(defun collatz-length/hash (n)
  (multiple-value-bind (value found)
      (gethash n *collatz*)
    (if found
	value
	(let ((m (if (evenp n) (/ n 2) (1+ (* 3 n)))))
	  (let ((v (collatz-length/hash m)))
	    (setf (gethash m *collatz*) v)
	    (1+ v))))))

(defun pro14/hash (n)
  (let ((max nil)
	(win 0))
    (loop
       :for i from 2 upto n
       :do (let ((len (collatz-length/hash i)))
	     (if (> len win)
		 (setf max i
		       win len))))
    (values max win)))