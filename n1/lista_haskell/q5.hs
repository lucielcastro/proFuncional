somaDiv :: Int -> Int -> Int
somaDiv n i
   | i == 1 = 1
   | mod n i /= 0 = somaDiv n (i-1)
   | otherwise = i + somaDiv n (i-1)

perfeito :: Int -> Bool
perfeito n
   | n == 1 = False
   | n == somaDiv n (n-1) = True
   | otherwise = False