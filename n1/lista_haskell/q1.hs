pot :: Int -> Int -> Int
pot x y
   | y < 0 = y*(-1)
   | y == 0 = 1
   | otherwise = x * pot x (y-1)