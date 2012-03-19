isPrime :: Int -> Bool
isPrime number =
  number /= 1 && rec 2
  where rec n
          | n * n > number = True
          | 0 == number `mod` n = False
          | otherwise = rec (n + 1)

pro7 :: Int -> Int
pro7 lim =
  rec 2 0
  where rec test cnt
          | lim == cnt = test - 1
          | isPrime test = rec (test + 1) (cnt + 1)
          | otherwise = rec (test + 1) cnt