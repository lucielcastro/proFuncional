somaList :: [Int] -> Int
somaList [] = 0
somaList (a:as) = a + somaList as 

repliChar :: Char ->Int-> [Char]
repliChar c n
   |n==0 = []
   |otherwise = c:repliChar c(n-1)
   
inverte :: [Char]->[Char]
inverte [] = []
inverte (a:as) = inverte as ++ [a]

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (cabeca:calda)
   | x <= cabeca = x:(cabeca:calda)
   | otherwise = cabeca : insere x calda