import Data.Char

numLength :: Int -> Int
numLength num =
  let rec n len =
        if n <= 0 then len else rec (n `div` 10) (len + 1)
  in rec num 0

helper :: Int -> Int
helper n =
  let rec index testNum =
        let len = numLength testNum
        in if index <= len
           then (ord $ (show testNum) !! (index - 1)) - ord '0'
           else rec (index - len) (testNum + 1)
  in rec n 1

exptBy10 :: Int -> Int
exptBy10 e
  | 0 >= e = 1
  | otherwise = 10 * (exptBy10 $ e - 1)

main = print $ product $ map helper $ map exptBy10 [0..6]