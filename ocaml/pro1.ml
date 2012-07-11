let pro1 num =
  let rec aux n sum =
    if n >= num then sum
    else if 0 = n mod 3 || 0 = n mod 5
    then aux (1+n) (sum+n)
    else aux (1+n) sum
  in aux 1 0
