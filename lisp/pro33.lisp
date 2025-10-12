;;; 参考资料：点击链接查看和 Kimi 的对话 https://www.kimi.com/share/d3ls4642sn141k4249o0
(defpackage #:com.liutos.pro33
  (:use #:cl))

(in-package #:com.liutos.pro33)

(defun check-is-curious-fraction (numerator denominator)
  "检查以 numerator 为分子、 denominator 为分母的分数是不是一个好奇分数。"
  ;; 分子和分母都是 2 位数。
  (assert (<= 10 numerator 99))
  (assert (<= 10 denominator 99))

  ;; 只有小于 1 才能算是好奇分数。
  (when (>= numerator denominator)
    (return-from check-is-curious-fraction nil))
  
  ;; 找出分子分母中一个相同的数字删掉，可以直接分为 4 种情况：
  ;; 1. 分子个位与分母个位相同；
  ;; 2. 分子个位与分母十位相同；
  ;; 3. 分子十位与分母个位相同；
  ;; 4. 分子十位与分母十位相同；
  (let ((target (/ numerator denominator)))
    (multiple-value-bind (numerator-l numerator-r)
        (truncate numerator 10)
      (multiple-value-bind (denominator-l denominator-r)
          (truncate denominator 10)
        (when (and (= numerator-r denominator-r)
                   (/= numerator-r 0)) 
          (when (= (/ numerator-l denominator-l) target)
            (return-from check-is-curious-fraction 't1)))

        (when (and (= numerator-r denominator-l)
                   (/= denominator-r 0))
          (when (= (/ numerator-l denominator-r) target)
            (return-from check-is-curious-fraction 't2)))

        (when (= numerator-l denominator-l)
          (when (= (/ numerator-r denominator-l) target)
            (return-from check-is-curious-fraction 't3)))

        (when (and (= numerator-l denominator-l)
                   (/= denominator-r 0))
          (when (= (/ numerator-r denominator-r) target)
            (return-from check-is-curious-fraction 't3)))))))

(defun pro33 ()
  (let ((results '()))
    (dotimes (i (1+ (- 99 10)))
      (let ((i (+ i 10)))
        (dotimes (j (1+ (- 99 10)))
          (let ((j (+ j 10)))
            (when (check-is-curious-fraction i j)
              (format t "i = ~D and j = ~D~%" i j)
              (push (/ i j) results))))))

    (let ((product (apply #'* results)))
      (denominator product))))
