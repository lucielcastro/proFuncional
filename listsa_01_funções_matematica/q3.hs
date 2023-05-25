
contFat :: Int->Int->Int->Int
contFat n y z
   |y==0 = 1
   |y>n = 0
   |otherwise = 1 + contFat n (y*z) (z+1)

fatorialSeq :: Int -> Int
fatorialSeq n
   | n == 1 = 1
   | otherwise = n * fatorialSeq (n-1)
   
f :: Int -> Int
f x = f x 1
   where f x n
             | fatorialSeq n == x = n
             | otherwise = f x (n+1)

-- fat reverso
rev :: Int -> Int
rev n
   | n == 1 = 1
   | otherwise = aux 2 n

aux :: Int -> Int -> Int
aux x n
   | fat x == n = x
   | otherwise = aux (x+1) n

--Fatorial 
fat :: Int -> Int
fat n
   | n == 0 = 1
   | otherwise = n * fat (n-1)
