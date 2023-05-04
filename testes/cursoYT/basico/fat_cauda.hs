

fat2 :: Int -> Int
fat2 n = fatbase n 1

fatbase :: Int -> Int -> Int
fatbase n x
   | n == 0 = x
   | otherwise = fatbase (n-1) n * x
