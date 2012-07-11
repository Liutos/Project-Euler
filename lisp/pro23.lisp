(proclaim '(optimize speed))

(defun divisor-sum (n)
  (let ((limit (truncate (sqrt n))))
    (labels ((aux (acc test)
	       (cond ((> test limit) acc)
		     ((zerop (rem n test))
		      (aux (+ acc test (/ n test))
			   (1+ test)))
		     (t (aux acc (1+ test))))))
      (aux 1 2))))

(defun abundant-number-p (n)
  (the boolean
    (> (divisor-sum n) n)))

(defparameter *abundants* '())
(defparameter *ahs*
  (make-hash-table))

(defun get-abundants ()
  (loop
     :for i :from 12 :to 28123
     :when (abundant-number-p i)
     :collect i))

(defun fill-abundant-hash-table ()
  (dolist (n *abundants*)
    (setf (gethash n *ahs*) t)))

(defun in-abundants-p (n)
  ;; (labels ((aux (abundants)
  ;; 	     (cond ((null abundants) nil)
  ;; 		   ((= (car abundants) n) t)
  ;; 		   ((> (car abundants) n) nil)
  ;; 		   (t (aux (cdr abundants))))))
  ;;   (the boolean
  ;;     (aux *abundants*)))
  (the boolean
    (gethash n *ahs*)))

(defun abundant-sum-p (n)
  (labels ((aux (abundants)
	     (cond ((null abundants) nil)
		   ((< n (car abundants)) nil)
		   ((in-abundants-p (- n (car abundants))))
		   (t (aux (cdr abundants))))))
    (aux *abundants*)))

(defun pro23 ()
  (loop
     :for i :from 24 :to 28123
     :when (abundant-sum-p i)
     :sum i))