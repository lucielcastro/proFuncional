abc :: Int -> Int
abc x
   | x == 0 = 1
   | x == 1 = 2
   | x == 2 = 4
   | x > 2 = 9
   | otherwise = 0
   
