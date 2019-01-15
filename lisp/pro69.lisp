(in-package :cl-user)

;;; 预先生成一百万范围内的素数
(defun primep (n)
  (cond ((< n 2) nil)
	((= 2 n) t)
	(t
	 (let ((bnd (truncate (sqrt n))))
	   (labels ((rec (test)
		      (cond ((> test bnd) t)
			    ((= 0 (rem n test)) nil)
			    (t (rec (1+ test))))))
	     (rec 2))))))

(defvar *primes-in-1000000* nil)
(defun generate-primes-in-1000000 ()
  (dotimes (i 1000000)
    (when (primep (1+ i))
      (push (1+ i) *primes-in-1000000*)))
  (setf *primes-in-1000000* (nreverse *primes-in-1000000*)))

;;; 舍弃的版本
(defun phi (n)
  "通过将不互质的比特设置为1并计算为0的比特的个数来计算phi函数"
  (let ((bits (make-array n :element-type 'bit :initial-element 0))
        (count 0))
    (dotimes (i n)
      (cond ((= (bit bits i) 1)
             ;; 该比特已经为1，说明已经在比它小的倍数被处理时一并被标记了
             ;; (format t "忽视第~D位~%" i)
             )
            ((= i (1- n))
             ;; 只处理比上界要小的数字
             )
            ((/= (gcd (1+ i) n) 1)
             ;; 除了当前这个不互质的数字之外，还需要将这个数字的倍数也一并处理
             (dotimes (j (floor n (1+ i)))
               (let* ((j (1+ j))
                      (m (* (1+ i) j)))
                 ;; (format t "将数字~D所在的位置标记为1~%" m)
                 (setf (bit bits (1- m)) 1)))
             ;; (format t "内循环处理结束~%")
             )
            (t (incf count))))
    count)
  ;; (let ((count 0))
  ;;   (dotimes (i n count)
  ;;     (when (= (gcd (1+ i) n) 1)
  ;;       (incf count))))
  )

(defun factoring (n)
  "质因数分解，以列表形式返回所有的质因数"
  (labels ((aux (test)
             (cond ((> test n)
                    '())
                   ((zerop (mod n test))
                    (let ((tail (factoring (/ n test))))
                      (if (equal (car tail) test)
                          tail
                          (cons test tail))))
                   (t (aux (1+ test))))))
    (aux 2)))

;;; 舍弃的版本
(defun phi2 (n)
  "先对n进行质因数分解，然后把每一个因数的倍数对应的比特置为1，最后计算比特为0的数量即可"
  (let ((bits (make-array n :element-type 'bit :initial-element 0))
        (factors (factoring n)))
    (dolist (factor factors)
      (let ((index (1- factor)))
        (when (zerop (bit bits index))
          (dotimes (j (floor n factor))
            (setf (bit bits (1- (* factor (1+ j)))) 1)))))
    (count-if (lambda (bit)
                (zerop bit))
              bits)))

(defun phi3 (n)
  "直接用素数表来做筛法"
  (prog
      ((bits (make-array n :element-type 'bit :initial-element 0)))
     (dolist (num *primes-in-1000000*)
       (cond ((> num n)
              (go :return))
             ((zerop (mod n num))
              (labels ((aux (i)
                         (when (< i n)
                           (setf (bit bits i) 1)
                           (aux (+ i num)))))
                (aux (1- num))))))
     :return
     (return-from phi3
       (count-if (lambda (bit) (zerop bit)) bits))))

(defun totient-maximum (n)
  (let ((max)
        (max-n))
    (dotimes (i n max-n)
      (when (and (> i 0)
                 (evenp (1+ i)))
        (let* ((n (1+ i))
               (phi-n (phi3 n)))
          (when (or (null max-n)
                    (> (/ n phi-n) max))
            (setf max (/ n phi-n)
                  max-n n)))))))
