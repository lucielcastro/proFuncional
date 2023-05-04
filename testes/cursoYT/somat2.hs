somat2 :: Int -> Int
somat2 n = smBase n 0
 
smBase :: Int -> Int -> Int
smBase n x
   | n == 0 = x
   | otherwise = smBase (n-1) n + x

