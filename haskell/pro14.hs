collatzLength :: Int -> Int
collatzLength n =
  let rec m times
        | 1 == m = times
        | even m = rec (m `div` 2) (1 + times)
        | otherwise = rec (1 + 3 * m) (1 + times)
  in rec n 1

pro14Helper :: (Int, Int) -> Int -> (Int, Int)
pro14Helper (max, win) i =
  let len = collatzLength i
  in if len > win
     then (i, len)
     else (max, win)

pro14 :: Int -> (Int, Int)
pro14 n =
  foldl pro14Helper (0, 0) [2..n]