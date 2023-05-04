 hanoil :: Int -> Int
 hanoil n
   | n == 1 = 1
   | otherwise = 1 + 2 * hanoil(n-1)