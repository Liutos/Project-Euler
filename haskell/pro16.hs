pro16 :: Int -> Int
pro16 n =
  let rec num sum =
        if 0 == num
        then sum
        else rec (num `div` 10) (sum + num `mod` 10)
  in rec n 0