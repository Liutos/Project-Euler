(defn rec [st nd sum limit]
  (if (> nd limit)
    sum
    (rec nd (+ st nd)
         (if (= 0 (mod nd 2))
           (+ sum nd)
           sum)
         limit)))

(defn pro2 [limit]
  (rec 1 2 0 limit))