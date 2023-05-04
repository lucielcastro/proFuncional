duploFat :: Int -> Int
duploFat n = dftbs n 1

dftbs :: Int -> Int -> Int
dftbs n x
   | n == 0 = x
   | n == 1 = x
   | otherwise = dftbs (n-2) n*x