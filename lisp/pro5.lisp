;;; helper函数取一个整数n和一个由整数组成的列表lst，然后用reduce函数从列表的左端开始向右运行同样
;;; 的操作。整数n为二元函数的初始值，如果作为第二个参数分派给二元函数的参数d的整数可以整除n，那么就
;;; 取n除以d的商，否则还是取n。这样经过一系列的运算，helper函数最后返回的结果就是一个整数对于一系
;;; 列整数来说``多出来''的那个数，只要将这个数与列表lst拼接在一起并计算所有元素的乘积，就可以计算出
;;; 整数n和列表中原来的所有数的最小公倍数。
(defun helper (n lst)
  (reduce #'(lambda (n d)
	      (if (zerop (rem n d))
		  (/ n d)
		  n))
	  lst :initial-value n))

(defun pro5 (n)
  (labels ((rec (m lst)
	     (if (>= m n)
		 (apply #'* lst)
		 (rec (1+ m) (push (helper m lst) lst)))))
    (rec 2 '(1))))