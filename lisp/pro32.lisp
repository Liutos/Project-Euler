(in-package #:com.liutos.project-euler)

(defun check-have-dup-digit (a)
  "检查 A 中是否有重复的数字。"
  ;; 创建一个长度为 9 的位图。每一位为 0 表示数字 A 中没有该数字出现、为 1 表示已经出现过。
  (let ((flags (make-list 10 :initial-element 0)))
    (do ((tmp a (truncate tmp 10)))
        ((zerop tmp))
      (let ((digit (nth-value 1 (truncate tmp 10))))
        (when (= (nth digit flags) 1)
          ;; 曾经有数字将这里设置成了 T，则表示现在遇到了重复的数字。
          (return-from check-have-dup-digit t))

        (setf (nth digit flags) 1))))

  ;; 能走到这里说明没有重复的数字。
  nil)

(defun check-is-from-1-to-9 (a b c)
  "检查 A、B，以及 C 三个数字是否正好完全了 1 到 9 这些数字各一次。"
  ;; 将 A、B，以及 C 的每一位相加，看看结果是否为 45（即从 1 加到 9 的和）；
  ;; 如果是 50，再看看位数是否刚好为 9 ；
  ;; 如果是，则说明参数 A、B，以及 C 符合条件。
  (let ((flags (make-list 9 :initial-element 0))
        (ns (list a b c)))
    (dolist (n ns)
      ;; 变量 tmp 从 n 开始迭代，每次都除以 10。
      (do ((tmp n (truncate tmp 10)))
          ((zerop tmp))
        (let ((digit (nth-value 1 (truncate tmp 10))))
          (when (zerop digit)
            ;; 遇到了 0 肯定是不对的。
            (return-from check-is-from-1-to-9 nil))

          (when (= (nth (1- digit) flags) 1)
            (return-from check-is-from-1-to-9 nil))
          
          (setf (nth (1- digit) flags) 1))))

    (every #'(lambda (x) (= x 1)) flags)))

(defun calculate-max-j (i)
  ;; 假设数字 i 有 X 位，那么【乘数】的位数不能多于 ceiling((9 - X)/2) 位。
  (let ((count 0))
    (do ((tmp i (truncate tmp 10)))
        ((zerop tmp))
      (incf count))

    (expt 10 (round (- 9 count) 2))))

(defun pro32 ()
  ;; 从 1 开始递增被乘数；
  ;; 在不遇到被乘数的情况下，从 1 开始递增乘数；
  ;; 检查它们与积一起是否涵盖了从 1 到 9 的所有数字。
  (let ((seen (make-hash-table))
        (sum 0))
    (loop
      :for i :from 1 :to 999           ; i 不能超过 3 位数，所以最大是 999。
      :do (cond ((check-have-dup-digit i))
                (t
                 (loop
                   :for j :from (1+ i) :to (calculate-max-j i)
                   :do (cond ((check-have-dup-digit j))
                             (t
                              (let ((product (* i j)))
                                (unless (check-have-dup-digit product)
                                  (when (check-is-from-1-to-9 i j product)
                                    (format t "i = ~D~Cj = ~D~Cproduct = ~D~%" i #\Tab j #\Tab product)
                                    (multiple-value-bind (v found)
                                        (gethash product seen)
                                      (declare (ignorable v))
                                      (unless found
                                        (incf sum product)
                                        (setf (gethash product seen) t))))))))))) )

    sum))
