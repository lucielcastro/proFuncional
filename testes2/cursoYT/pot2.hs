{-CRiar função chamada pot2(b, e) que apresente o resultado da potência da base "b" elevada ao expoente "e" respeitando as condições matematicas que regimentam a operação-}

pot :: Int -> Int -> Int
pot b e = potBs b e 1

potBs :: Int -> Int -> Int -> Int
potBs b e v
   | e == 0 = v
   | e == 1 = v * b
   | otherwise = potBs b (e-1) b * v