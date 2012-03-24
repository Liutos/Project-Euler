fib :: Integer -> Integer
fib n
  | 1 == n = 1
  | 2 == n = 1
  | otherwise = rec 1 1 n
  where rec a b m =
          if 2 == m then b else rec b (a + b) (m - 1)

numLength :: Integer -> Integer
numLength n =
  let rec num len =
        if 0 == num then len
        else rec (num `div` 10) (len + 1)
  in rec n 0

pro25 :: Integer -> Integer
pro25 len =
  let rec term =
        if numLength (fib term) >= len
        then term
        else rec $ term + 1
  in rec 13