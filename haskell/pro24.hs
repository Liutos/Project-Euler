factorial :: Int -> Int
factorial n =
  let rec acc m =
        if 0 == m then acc else rec (acc * m) (m - 1)
  in rec 1 n

nthLexicographicPermutation :: Int -> [Int] -> Integer
nthLexicographicPermutation n digitList =
  let rec acc rest l nth
        | 0 == nth = foldl (\sum x -> 10 * sum + x) acc rest
        | otherwise =
          let fac = factorial l
          in if fac > nth
             then rec acc rest (l - 1) nth
             else let q = nth `div` fac
                      r = nth - q * fac
                      inc = rest !! q
                  in rec (10 * acc + inc) [e | e <- rest, e /= inc] (l - 1) r
  in rec 0 digitList (length digitList) n