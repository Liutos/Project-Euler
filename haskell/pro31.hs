coinsValue :: Int -> Int
coinsValue n =
  [1, 2, 5, 10, 20, 50, 100, 200] !! (n - 1)

consCoins :: Int -> Int -> Int
consCoins amount kindsOfCoins
  | 0 == amount = 1
  | amount < 0 || 0 == kindsOfCoins = 0
  | otherwise = part1 + part2
  where part1 = consCoins amount (kindsOfCoins - 1)
        part2 = consCoins (amount - (coinsValue kindsOfCoins)) kindsOfCoins

main = print $ consCoins 200 8