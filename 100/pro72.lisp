(in-package #:cl-user)

(defpackage #:com.liutos.pe.72
  (:use #:cl))

(in-package #:com.liutos.pe.72)

;;; 用筛法来找出所有与参数互质的数字
(defun find-coprime-numbers (d)
  "找出小于D并且与D互质的正整数。"
  (let ((bitmap (make-array d :initial-element 1))
        (result '()))
    (dotimes (i (1- d) (nreverse result))
      (let ((ele (aref bitmap i)))
        (unless (zerop ele)
          ;; 如果为0，意味着这个下标对应的数字是某个不互质的数字的倍数，那么肯定也不互质，所以不用处理。
          (let ((n (1+ i)))
            ;; (format t "待检测的数字为~D~%" n)
            (cond ((= (gcd n d) 1)
                   ;; (format t "~D和~D互质哦~%" n d)
                   (push n result))
                  (t
                   ;; 把n的倍数对应的下标（即-1后的值）的元素都赋值为0
                   (do* ((times 2 (1+ times))
                         (j (1- (* times n)) (1- (* times n))))
                        ((> j (1- d)))
                     (setf (aref bitmap j) 0))))))))))

(defun compute-reduced-proper-fractions (d)
  "返回D的所有reduced proper fraction。"
  (let ((ns (find-coprime-numbers d)))
    (mapcar #'(lambda (n)
                (/ n d))
            ns)))

(defun count-total-reduced-proper-fractions (d)
  "计算所有不大于D的正整数的reduced proper fraction的数量。"
  (let ((count 0))
    (dotimes (i d count)
      (let* ((nd (1+ i))
             ;; 并不需要真的计算出这个reduced proper fraction，因为既然n和d是互质的，也就是没有约分的过程，那么就不可能由不同的d产生出相同的n/d。
             (fractions (progn
                          (when (zerop (mod nd 1000))
                            (format t "已处理了前~D个数字~%" nd))
                          (find-coprime-numbers nd))))
        (incf count (length fractions))))))

;;; 换个思路？
;;; 先算出一百万以下的所有素数
;;; 基于这个素数表，对每一个数字d都进行快速的因数分解
;;; 得到数字d的质因数后，用筛法计算出互质的数字的个数
(defun generate-prime-numbers (limit)
  "计算出所有不大于LIMIT的素数。"
  (let ((bitmap (make-array limit :initial-element 1))
        (result '()))
    (dotimes (i limit (nreverse result))
      (let ((ele (aref bitmap i)))
        ;; 如果ele已经是0了，便不是素数，也就不用做进一步判断了。
        (when (= ele 1)
          (let ((n (1+ i)))
            ;; 跳过1，因为1是素数并且是所有数字的因数
            (when (> n 1)
              (push n result)
              ;; 把属于当前数字的倍数对应的下标的元素赋值为0
              (do* ((times 2 (1+ times))
                    (j (1- (* times n)) (1- (* times n))))
                   ((> j (1- limit)))
                (setf (aref bitmap j) 0)))))))))

(defvar *prime-numbers* (generate-prime-numbers 1000000)
  "不大于一百万的素数。")

(defun factoring (n)
  "返回N的质因数。"
  (labels ((aux (n ps result)
             (let ((f (first ps)))
               (cond ((= n 1)
                      result)
                     ((zerop (mod n f))
                      (aux (/ n f) ps (if (equal f (car result)) result (cons f result))))
                     (t
                      (aux n (rest ps) result))))))
    (aux n *prime-numbers* '())))

;;; 用因数分解加筛法来寻找互质的数字
(defun count-coprime-numbers (d)
  "数出小于D并且与D互质的正整数。"
  (let ((bitmap (make-array d :initial-element 1))
        (factors (factoring d)))
    (dolist (f factors)
      (do ((i (1- f) (+ i f)))
          ((> i (1- d)))
        (setf (aref bitmap i) 0)))
    (count-if #'(lambda (ele)
                  (= ele 1))
              bitmap)))

(defun count-coprime-numbers-by-formula (d)
  "用欧拉函数计算与D互质的数的个数。"
  (* d
     (reduce #'(lambda (acc n)
                 (* acc (- 1 (/ 1 n))))
             (factoring d)
             :initial-value 1)))

(defun count-total-reduced-proper-fractions (d)
  "计算所有不大于D的正整数的reduced proper fraction的数量。"
  (let ((count 0))
    (dotimes (i d (1- count))
      (let ((nd (1+ i)))
        (when (zerop (mod nd 1000))
          (format t "已处理~D个数字~%" nd))
        (incf count (count-coprime-numbers-by-formula nd))))))
