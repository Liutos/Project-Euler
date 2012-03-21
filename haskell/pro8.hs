import qualified Data.Char as Char
import Data.List

digStrProd :: String -> Int
digStrProd digStr =
  product $ map (\c -> Char.ord c - Char.ord '0') digStr

subseq :: [a] -> Int -> Int -> [a]
subseq seq start end =
  snd $ splitAt start (take end seq)

pro8 :: String -> Int
pro8 digitString =
  let rec i max =
        if i > 995 then max
        else let num = digStrProd $ subseq digitString i $ i + 5
             in if num > max
                then rec (i + 1) num
                else rec (i + 1) max
  in rec 0 0

main = do
  digitStr <- getLine
  print $ pro8 digitStr