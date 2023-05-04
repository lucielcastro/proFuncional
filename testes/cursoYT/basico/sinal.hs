-- retorne -1 | x<y -> 1 | x> y e -> 0 | y =<<

sinal :: Int -> Int -> Int
sinal x y
   | x < y = -1
   | x > y = 1
   | otherwise = 0