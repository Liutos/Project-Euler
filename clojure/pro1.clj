(defn rec [n sum num]
  (if (>= n num)
    sum
    (if (or (= 0 (mod n 3))
            (= 0 (mod n 5)))
      (rec (+ n 1) (+ sum n) num)
      (rec (+ n 1) sum num))))

(defn pro1 [num]
  (rec 1 0 num))