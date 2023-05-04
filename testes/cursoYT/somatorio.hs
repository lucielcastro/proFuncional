somat1 :: Int -> Int
somat1 n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = n + somat1(n-1)