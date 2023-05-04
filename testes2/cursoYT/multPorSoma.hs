 mult :: Int -> Int -> Int
 mult m n
   | n == 0 = 0
   | n > 0 = m + mult m (n-1)
   | otherwise = - mult m (-n)