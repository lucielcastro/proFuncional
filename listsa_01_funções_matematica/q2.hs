fat :: Integer -> Integer
fat n
   | n == 0  = 1
   | otherwise = n*fat(n-1)