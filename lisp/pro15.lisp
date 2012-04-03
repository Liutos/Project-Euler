(defun square-routes (size)
  (let ((aux (make-array `(,(1+ size) ,(1+ size)) :initial-element nil)))
    (labels ((rec (i j)
	       (cond ((aref aux i j) (aref aux i j))
		     ((or (= size i) (= size j)) 1)
		     (t (let ((right-ptr (rec (1+ i) j))
			      (down-ptr (rec i (1+ j))))
			  (setf (aref aux i j) (+ right-ptr down-ptr))
			  (+ right-ptr down-ptr))))))
      (rec 0 0))))