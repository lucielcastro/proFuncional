fat :: Integer -> Integer
fat n
   | n == 0  = 1
   | otherwise = fat(n-1)*n
   
fatSoma :: Integer -> Integer
fatSoma n
   |  n == 0 = 1
   | otherwise = fat(n) + fatSoma(n-1)