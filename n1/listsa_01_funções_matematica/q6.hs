fatAlt :: Int -> Int 
fatAlt n
   | n == 0 = 1
   | otherwise = (-n)*fatAlt(n-1)

somaFatAlt :: Int -> Int
somaFatAlt n
   | n == 0 = 1
   | otherwise = fatAlt(n) + somaFatAlt(n-1)