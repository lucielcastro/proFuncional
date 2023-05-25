 hanoi :: Int -> Int
 hanoi n
   | n == 1 = 1
   | otherwise = 1 + 2 * hanoi(n-1)
   