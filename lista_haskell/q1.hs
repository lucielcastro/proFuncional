pot :: Integer -> Integer -> Integer
pot x y
   | y == 0 = 1
   | otherwise = x * pot (x y-1)