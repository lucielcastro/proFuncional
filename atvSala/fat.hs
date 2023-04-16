fat2 :: Int -> Int
fat2 0 = 1
fat2 n = n*fat2(n-1)
