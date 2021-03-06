;; Required the 3rd party library CL-PPCRE
;; Loaded by quicklisp: (ql:quickload "cl-ppcre")
(defun triangle-word-p (word)
  (let ((sum 0))
    (dotimes (i (length word))
      (let ((c (char word i)))
        (incf sum (1+ (- (char-code c) (char-code #\A))))))
    (let ((n (truncate (1- (truncate (sqrt (1+ (* 8 sum))))) 2)))
      (= (truncate (* n (1+ n)) 2) sum))))

(defun pro42 ()
  (with-open-file (s "p042_words.txt")
    (let* ((line (read-line s))
           (words (cl-ppcre:split "," line))
           (cnt 0))
      (dolist (_word words cnt)
        (let ((word (subseq _word 1 (1- (length _word)))))
          (when (triangle-word-p word)
            (incf cnt)))))))
