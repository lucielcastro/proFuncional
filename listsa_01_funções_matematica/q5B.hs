fibProd :: Integer -> Integer
fibProd n
   | n == 1 = 1
   | otherwise = fibProd(n-1) * fibProd(n)
   