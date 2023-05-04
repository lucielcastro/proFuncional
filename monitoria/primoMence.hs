primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Integral t => t -> t -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

  --implementeção parcial 
  
pot :: Int -> Int -> Int
pot x y
   | y < 0 = y*(-1)
   | y == 0 = 1
   | otherwise = x * pot x (y-1)

primoMecen :: Int -> Bool
primoMecen n
   | n <= 2 = False
   | otherwise = primoMecen()