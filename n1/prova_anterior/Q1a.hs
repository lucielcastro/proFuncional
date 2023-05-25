primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Int -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

primoGemeos :: Int -> Int -> Bool
primoGemeos n x
   | n - x == 2 || n - x == -2 && primo n && primo x == True = True
   | otherwise = False

primoPertence :: Int -> Bool
primoPertence n
   | primo (n + 2) == True || primo (n-2) == True && primo n == True = True
   | otherwise = False

somaPrimo :: Int -> Int
somaPrimo i
   | i < 5 = 0
   | primo i == False = somaPrimo (i-1)
   | otherwise = 1 + somaPrimo (i-1)
   {-
   perfeito :: Int -> Bool
perfeito n
   | n == 1 = False
   | n == somaDiv n (n-1) = True
      | otherwise = False

   -}

parGemeo::Int->Int->Bool
parGemeo x y
   |(primo x) && (primo y) && (y-x==2) = True
   |otherwise=False
   
pertencePar::Int->Bool
pertencePar n
   |parGemeo n (n+2)|| parGemeo (n-2) n = True
     -- |primo n && (primo (n+2) || primo (n-2))=True
   |otherwise=False
   
contaParGemeo::Int->Int
contaParGemeo n
   |n<5=0
   |parGemeo (n-2) n = 1+ contaParGemeo (n-1)
   |otherwise= contaParGemeo (n-1)
   '
somaParGemeo::Int->Int
somaParGemeo n
   |n<5=0
   |parGemeo (n-2) n = n+(n-2) + somaParGemeo (n-1)
   |otherwise= somaParGemeo (n-1)
   