helper :: Int -> [Int] -> Int
helper n lst =
  foldl (\n d -> if 0 == n `mod` d then n `div` d else n) n lst

pro5 :: Int -> Int
pro5 n =
  let rec m lst =
        if m >= n
        then product lst
        else rec (m + 1) (helper m lst : lst)
  in rec 2 [1]