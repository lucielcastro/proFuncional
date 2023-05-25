--Conta qtd algarismo
contAlg :: Int -> Int
contAlg n
   | div n 10 == 0 = 1
   | otherwise = 1 + contAlg(div n 10)

-- Numero perfeito
perfeito :: Int -> Bool
perfeito x
   | x == 1 = False
   | somaDiv x (x-1) == x = True
   | otherwise = False

somaDiv :: Int -> Int -> Int
somaDiv x i
   | i == 1 = 1
   | mod x i == 0 = i + somaDiv x (i-1)
   | otherwise = somaDiv x (i-1)

-- fat reverso
f :: Int -> Int
f n
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

--coPrimo 
coPrimo :: Int -> Int -> Int -> Bool
coPrimo n d i
   | i == 1 = True
   | mod n i == 0 && mod d i == 0 = False
   | otherwise = coPrimo n d (i-1)