import Data.Char

fact n =
  let rec m prod =
        if 1 == m
        then prod
        else rec (m - 1) (prod * m)
  in rec n 1

pro20 n =
  if n < 10
  then n
  else (n `mod` 10) + (pro20 $ n `div` 10)