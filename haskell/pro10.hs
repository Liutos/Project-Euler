isPrime :: Int -> Bool
isPrime number =
  number /= 1 && rec 2
  where rec n
          | n * n > number = True
          | 0 == number `mod` n = False
          | otherwise = rec (n + 1)

pro10 =
  sum [ n | n <- [1..200000], isPrime n]