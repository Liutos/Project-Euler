let smallest_factor n =
  let rec aux test =
    if test >= n || 0= n mod test then test
    else aux (1+test)
  in aux 2

let largest_prime_factor n =
  let rec aux num acc =
    if 1=num then acc
    else if num <= 2 then num
    else let fac = smallest_factor num
    in aux (num / fac) fac
  in aux n 1
