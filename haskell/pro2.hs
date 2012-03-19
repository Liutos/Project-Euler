pro2 limit =
  let rec st nd sum =
        if nd > limit
        then sum
        else rec nd (st + nd) (if even nd then sum + nd else sum)
  in rec 1 2 0