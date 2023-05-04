quadradoPerfeito :: Int -> Bool
quadradoPerfeito n = aux n 0

aux :: Int -> Int -> Bool
aux n k
   | k * k > n = False
   | k * k == n = True
   | otherwise = aux n (k+1)