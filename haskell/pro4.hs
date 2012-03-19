isPalindromic :: Int -> Bool
isPalindromic number =
  let str = show number
  in str == reverse str

pro4 =
  maximum [x * y | x <- [100..999], y <- [100..999], isPalindromic (x * y)]