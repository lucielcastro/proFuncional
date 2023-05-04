pGolomb :: Int -> Int -> Int
pGolomb n x
   | primo(golomb x) && n == 1 = golomb x
   | primo(golomb x) && n > 1 = pGolomb (n-1) (x+1)
   | otherwise = pGolomb n (x+1)

primo :: Int -> Bool
primo n
   | n < 2     = False
   | otherwise = aux n (n-1)
     
aux :: Integral t => t -> t -> Bool
aux n i
   | i == 1        = True
   | mod n i == 0  = False
   | otherwise     = aux n(i-1)

golomb :: Int -> Int
golomb n
   | n == 1 = 1
   | otherwise = 1 + golomb(n - golomb(golomb(n-1)))