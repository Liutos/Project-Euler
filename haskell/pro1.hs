pro1 :: Int -> Int
pro1 sup = sum [x | x <- [1..(sup - 1)], x `mod` 3 == 0 || x `mod` 5 == 0]