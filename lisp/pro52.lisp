;;; This is really a ugly program
(defun digit-chars (number)
  (if (< number 10)
      (list number)
      (cons (rem number 10)
	    (digit-chars (/ (- number (rem number 10)) 10)))))

(defun same-elts (lst1 lst2)
  (and (every (lambda (elt)
		(member elt lst2 :test #'=))
	      lst1)
       (every (lambda (elt)
		(member elt lst1 :test #'=))
	      lst2)))

(defun digit-test (number)
  (let ((org (digit-chars number))
	(2org (digit-chars (* 2 number)))
	(3org (digit-chars (* 3 number)))
	(4org (digit-chars (* 4 number)))
	(5org (digit-chars (* 5 number)))
	(6org (digit-chars (* 6 number))))
    (every (lambda (lst)
	     (same-elts org lst))
	   (list 2org 3org 4org 5org 6org))))

(defun pro52 ()
  (labels ((rec (test-number)
	     (if (> test-number 100000000)
		 nil
		 (if (digit-test test-number)
		     test-number
		     (rec (1+ test-number))))))
    (rec 1)))