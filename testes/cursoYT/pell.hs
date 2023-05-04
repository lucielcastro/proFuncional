pPell :: Int -> Int -> Int
pPell n x
   | primo(pell x) && n == 1 = pell x
   | primo(pell x) && n > 1 = pPell (n-1) (x+1)
   | otherwise = pPell n (x+1)

primo :: Int -> Bool
primo n
   | n < 2     = False
   | otherwise = aux n (n-1)
     
aux :: Integral t => t -> t -> Bool
aux n i
   | i == 1        = True
   | mod n i == 0  = False
   | otherwise     = aux n(i-1)

pell :: Int -> Int
pell n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = 2*pell(n-1) + pell(n-2)