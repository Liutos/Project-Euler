(defn p1 [n]
  (apply + (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))) (range 1 n))))
