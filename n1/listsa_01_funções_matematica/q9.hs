
gold :: Int -> (Int, Int)
gold n = somaPrimo (n-2) 2
  where
    somaPrimo :: Int -> Int -> (Int, Int)
    somaPrimo p1 p2
      |(primo p1 && primo p2) && p1 + p2 == n = (p1, p2)
      |(primo p1 && primo p2) && p1 + p2 > n = somaPrimo p1 (p2 + 1)
      | otherwise = somaPrimo (p1 - 1) (p2 + 1)
{-
buscarPar :: Int -> Int
buscarPar n p
  | primo(n - (primo p)) = primo p && n - (primo p)
-}
primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Integral t => t -> t -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

{-- goldbach :: Int -> Int -> Int
goldbach n
  | n --}