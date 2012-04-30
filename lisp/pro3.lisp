;;; 这道题目就是要计算一个数的最大质因数，所以只要从最小的质数开始计算就可以了。不过也没必要没每到一
;;; 个因数就判断是否是因数，只要对于当前求最大质因数的数，假设为n，找出它的最小因数d（它必然是质数）
;;; ，除以这个最小因数，得到的商为m=n/d，这个m的最大因数必然也是n的最大质因数，所以接下来就计算m的
;;; 最大质因数就好了。很显然，这里可以使用递归来优雅地处理。smallest-factor函数用于计算最小的质因
;;; 数，然后largest-prime-factor内部的rec就以(/ num fac)来作为新的等待求最大质因数的参数。
(defun smallest-factor (n)
  (labels ((rec (test)
	     (if (or (>= test n) (zerop (rem n test)))
		 test
		 (rec (1+ test)))))
    (rec 2)))

;;; 由labels定义的函数rec的第一个参数是用于继续求最大质因数的数字，而acc则是存储着上一个数的最大质
;;; 因数的参数。在每一次内部迭代中，我都要判断num是否为1。如果num为1，则表示原来的参数n已经被整除到
;;; 极限了，acc就是上一个数的最大质因数，也是n的最大质因数。否则，就计算num的最小因数fac，用fac除
;;; 参数num，得到的数的最大质因数必然还是num的最大质因数，同时也是n的最大质因数。至于代码中的第二个
;;; 测试条件，则一般不会遇到了。
(defun largest-prime-factor (n)
  (labels ((rec (num acc)
	     (cond ((= 1 num) acc)
		   ((<= num 2) num)
		   (t (let ((fac (smallest-factor num)))
			(rec (/ num fac) fac))))))
    (rec n 1)))