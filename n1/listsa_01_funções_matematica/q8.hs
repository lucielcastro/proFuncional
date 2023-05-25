coleta :: Integer -> Integer
coleta n
   | n == 0 = 0
   | otherwise = 7*n+coleta(n-1)
   