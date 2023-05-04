--menor valor entre 2 valores

menor :: Int -> Int -> Int
menor x n
   | n<x = n
   | otherwise = x