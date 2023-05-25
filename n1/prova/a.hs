somaCatalan :: Int -> Int -> Int
somaCatalan n m 
   | m < 2 = 0
   | catalan m = m + qtdCatalan n (m-1)
   | otherwise = qtdCatalan n (m-1)

qtdCatalan :: Int -> Int -> Int
qtdCatalan n m 
   | m < 2 = 0
   | catalan m = 1 + qtdCatalan n (m-1)
   | otherwise = qtdCatalan n (m-1)

   gold::Int->(Int, Int)
   gold n = somaPrimo (n-2) 2
      where
         somaPrimo::Int->Int->(Int, Int)
         somaPrimo p1 p2
            |(primo p1 && primo p2) && p1 + p2 == n = (p1, p2)
            |(primo p1 && primo p2) && p1 + p2 > n = somaPrimo p1 (p2+1)
            |otherwise = somaPrimo (p1-1) (p2+1)
            
   