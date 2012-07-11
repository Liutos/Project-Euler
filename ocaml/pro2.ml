let even_fib_sum limit =
  let rec aux st nd sum =
    if nd > limit then sum
    else aux nd (st+nd) (if 0= nd mod 2 then (sum+nd) else sum)
  in aux 1 2 0
