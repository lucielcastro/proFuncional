fibProd :: Integer -> Integer
fibProd n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fibProd(n-1) * fibProd(n)
   