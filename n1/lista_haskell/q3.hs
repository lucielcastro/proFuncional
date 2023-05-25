primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Integral t => t -> t -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

contarPrimo :: Int -> Int 
contarPrimo i
  | i == 2 = 1
  | primo i == False = contarPrimo (i-1)
  | 1 + contarPrimo (i-1)
  | otherwise = primo i




  
{--perfeito :: Int -> Bool
perfeito n
  | n == 1 = False
  | n == somaDiv n (n-1) = True
  | otherwise = False--}

  --implementeção parcial 
  