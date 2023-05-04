somaS :: Int -> Int -> Int
somaS x i
   | i == 2 = 2
   | not(primoSophie i ) = somaS x (i-1)
   | otherwise = i + somaS x (i-1)

intPrimoS :: Int -> Int -> Int
intPrimoS n m 
   | m<n = intPrimoS m n
   | n == m && primoSophie n = 1
   | n == m && not(primoSophie n) = 0
   | primoSophie m = 1 + intPrimoS n (m-1)
   | otherwise = intPrimoS n (m-1)

maiorSophie :: Int -> Int
maiorSophie n
   | n < 2 = -1
   | primoSophie n = n
   | otherwise = maiorSophie (n-1)

primoSophie :: Int -> Bool
primoSophie n
   | primo n && primo((2 * n) + 1) = True
   | otherwise = False


primo :: Int -> Bool
primo n
   | n < 2     = False
   | otherwise = aux n (n-1)
        
aux :: Int -> Int -> Bool
aux n i
   | i == 1        = True
   | mod n i == 0  = False
   | otherwise     = aux n(i-1)