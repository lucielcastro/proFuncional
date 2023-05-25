--D
somaCatalan::Int->Int->Int
somaCatalan n x
   |n > x = 0
   |n == 1 = 1 + n + somaCatalan (n+1) x
   |n == x && pertenceCatalan x 1 1 = x
   |n < x && pertenceCatalan n 1 1 = n + somaCatalan (n+1) x
   |otherwise = somaCatalan (n+1) x

---- na função acima deve-se passar os numeros dos intervalos que se quer verificar 
 
qtdCatalan::Int->Int
qtdCatalan n = aux 1 (n-1)
   where
      aux :: Int -> Int -> Int
      aux n m 
         | m == 1 = 2
         | pertenceCatalan m 1 1 = 1 + aux n (m-1)
         | otherwise = aux n (m-1)

-- na função acima deve-se passar o numero "m" que se quer saber quantos numeros tem abaixo e n será sempre n = 1 

pertenceCatalan::Int->Int->Int->Bool
pertenceCatalan n cont compara
  |compara == n = True
  |compara > n = False
  |otherwise = pertenceCatalan n (cont+1) (catalan cont)

-- na função acima deve-se passar o numero "n" que se quer verificar e cont = 1 e compara = 1

catalan :: Int -> Int
catalan n
   | n == 1 = 1
   | n == 0 = 1
   | otherwise = div (fat(2 * n)) (fat(n + 1) * fat n)

fat :: Int -> Int
fat n
   | n == 0 = 1
   | otherwise  = n * fat (n-1)

   