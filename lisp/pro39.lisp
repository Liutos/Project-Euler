;; 较小的直角边(a)必须小于n/3
;; 斜边必须不小于n/3，可以推导出sqrt(n^2 / 9 - a^2) <= b <= n/2
(defun triangle-numbers (n)
  (let ((acc 0))
    (dotimes (a (truncate n 3))
      (let ((low (truncate (sqrt (- (truncate (* n n) 9) (* a a))))))
        (dotimes (_b (- (truncate n 2) low))
          (let* ((b (+ _b low))
                 (c (- n a b)))
            (when (= (* c c) (+ (* a a) (* b b)))
              (incf acc))))))
    acc))

(defun pro39 ()
  (let ((max 0) (win 0))
    (loop :for i :from 1 :upto 1000
       :do (let ((score (triangle-numbers i)))
	     (if (> score win)
		 (setf max i
		       win score))))
    (values max win)))
