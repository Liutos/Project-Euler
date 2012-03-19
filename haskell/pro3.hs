pro3Helper :: Integer -> Integer
pro3Helper n =
  let rec testNum
        | testNum >= n || 0 == n `mod` testNum = testNum
        | otherwise = rec (testNum + 1)
  in rec 2

pro3 :: Integer -> Integer
pro3 n =
  let rec num acc
        | 1 == num = acc
        | num <= 2 = num
        | otherwise = rec (num `div` fac) fac
        where fac = pro3Helper num
  in rec n 1