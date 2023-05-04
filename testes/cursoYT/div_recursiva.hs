divide :: Int -> Int -> Int
divide n d
   | n < d = 0
   | otherwise = 1 + divide (n-d) d