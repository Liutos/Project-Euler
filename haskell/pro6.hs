pro6 :: Int -> Int
pro6 n =
  let tmp = (n * (n + 1)) `div` 2
  in tmp ^ 2  - sum [ x * x | x <- [1..n]]