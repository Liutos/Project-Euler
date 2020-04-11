(in-package #:cl-user)

(defpackage #:com.liutos.pe.100
  (:use #:cl))

(in-package #:com.liutos.pe.100)

(defun compute-max-reduced-proper-fraction (d)
  "计算出以D为分母的、小于3/7的、最大的reduced proper fraction。"
  ;; (let ((max 0))
  ;;   (dotimes (i d)
  ;;     (let ((i (1+ i)))
  ;;       (when (>= i (/ (* 3 d) 7))
  ;;         (return-from compute-max-reduced-proper-fraction max))
  ;;       (when (and (= (gcd i d) 1)
  ;;                  (> (/ i d) max))
  ;;         (setf max (/ i d))))))
  (do ((n (truncate (* 3 d) 7) (- n 1)))
      ((= (gcd n d) 1) (/ n d))))

(defun compute-max-reduced-proper-fraction-under (d)
  "计算出不大于D的所有整数中最大的reduced proper fraction。"
  (let ((max 0))
    (dotimes (i d max)
      (let ((i (1+ i)))
        (when (zerop (rem i 1000))
          (format t "处理了~D个数字~%" i))
        (when (/= i 7)
          (setf max (max max (compute-max-reduced-proper-fraction i))))))))
